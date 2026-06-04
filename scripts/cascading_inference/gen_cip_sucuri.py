#!/usr/bin/env python3
"""Generate Sucuri Cascading Inference Pipeline variant.

PyO3 Rust extension exposes init/stage1/stage2/stage3/stage4/output, and
a Python driver builds the pyDF.DFGraph with one node per (chunk, stage)
edged by the per-chunk dependency chain, plus a final sink consuming
all chunks' stage4 outputs.
"""

import argparse, os, struct


CARGO_TOML = """[package]
name = "sucuri_cip"
version = "0.1.0"
edition = "2021"

[lib]
name = "sucuri_cip"
crate-type = ["cdylib"]

[dependencies]
pyo3 = { version = "0.25", features = ["extension-module"] }
libc = "0.2"

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
debug = false
"""


RS_TEMPLATE = r"""// Sucuri Cascading Inference Pipeline PyO3 extension.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use pyo3::prelude::*;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};

const DIM_D:    usize = 256;
const B2_SLOTS: usize = 256;
const E_DIM:    usize = 64;
const K3:       usize = 8;
const H_DIM:    usize = 128;
const C_CLS:    usize = 16;
const K1:       usize = 1024;

const ACCEPT_S1:      i32 = 1;
const REJECT_S2:      i32 = 2;
const ACCEPT_S3_BASE: i32 = 0x40;
const CLASS_BASE:     i32 = 0x80;

const N:          usize = __N__;
const CHUNK_SIZE: usize = __CHUNK_SIZE__;
const DATA_DIR:   &str  = "__DATA_DIR__";

static INIT_DONE: AtomicBool = AtomicBool::new(false);
static mut ITEMS:        *mut u8  = std::ptr::null_mut();
static mut DECISIONS:    *mut i32 = std::ptr::null_mut();
static mut EMB_ALL:      *mut f64 = std::ptr::null_mut();
static mut ACCEPT_TABLE: *mut u32 = std::ptr::null_mut();
static mut REJECT_W:     *mut i16 = std::ptr::null_mut();
static mut REF_VEC:      *mut f64 = std::ptr::null_mut();
static mut W1_MAT:       *mut f64 = std::ptr::null_mut();
static mut B1_VEC:       *mut f64 = std::ptr::null_mut();
static mut W2_MAT:       *mut f64 = std::ptr::null_mut();
static mut B2_VEC:       *mut f64 = std::ptr::null_mut();
static mut COS_TABLE:    *mut f64 = std::ptr::null_mut();
static mut T2_CFG:       i32      = 0;
static mut T3_CFG:       f64      = 0.0;

unsafe fn xmalloc<T>(n: usize) -> *mut T {
    libc::malloc(n * std::mem::size_of::<T>()) as *mut T
}

unsafe fn stage1_decide(it: *const u8) -> bool {
    let mut sig: u32 = 0;
    for i in 0..DIM_D { sig = sig.wrapping_add(*it.add(i) as u32); }
    sig &= 0xFFFF;
    *ACCEPT_TABLE.add((sig & 0x3FF) as usize) == sig
}
unsafe fn stage2_score(it: *const u8) -> i32 {
    let mut hist = [0i32; B2_SLOTS];
    for i in 0..(DIM_D - 1) {
        let b = ((*it.add(i) as i32) * 7 + *it.add(i+1) as i32) & 0xFF;
        hist[b as usize] += 1;
    }
    let mut s: i32 = 0;
    for k in 0..B2_SLOTS { s += hist[k] * (*REJECT_W.add(k) as i32); }
    s
}
unsafe fn stage3_embed(it: *const u8, emb: *mut f64) {
    for j in 0..E_DIM {
        let mut s = 0.0f64;
        let row = COS_TABLE.add(j * DIM_D);
        for i in 0..DIM_D { s += *row.add(i) * (*it.add(i) as f64 / 255.0); }
        *emb.add(j) = s;
    }
    let mut n2 = 0.0f64;
    for j in 0..E_DIM { let v = *emb.add(j); n2 += v * v; }
    if n2 > 0.0 {
        let inv = 1.0 / n2.sqrt();
        for j in 0..E_DIM { *emb.add(j) *= inv; }
    }
}
unsafe fn stage3_best(emb: *const f64) -> (usize, f64) {
    let mut best = 0usize; let mut bs = -1e300f64;
    for k in 0..K3 {
        let r = REF_VEC.add(k * E_DIM);
        let mut s = 0.0f64;
        for j in 0..E_DIM { s += *emb.add(j) * *r.add(j); }
        if s > bs { bs = s; best = k; }
    }
    (best, bs)
}
unsafe fn stage4_classify(emb: *const f64) -> usize {
    let mut hidden = [0.0f64; H_DIM];
    for h in 0..H_DIM {
        let mut s = *B1_VEC.add(h);
        let row = W1_MAT.add(h * E_DIM);
        for j in 0..E_DIM { s += *row.add(j) * *emb.add(j); }
        hidden[h] = if s > 0.0 { s } else { 0.0 };
    }
    let mut best = 0usize; let mut bs = -1e300f64;
    for c in 0..C_CLS {
        let mut s = *B2_VEC.add(c);
        let row = W2_MAT.add(c * H_DIM);
        for h in 0..H_DIM { s += *row.add(h) * hidden[h]; }
        if s > bs { bs = s; best = c; }
    }
    best
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) { return; }
        // weights.bin
        let mut buf = Vec::new();
        File::open(format!("{}/weights.bin", DATA_DIR)).unwrap().read_to_end(&mut buf).unwrap();
        let mut off = 0usize;
        macro_rules! take { ($ty:ty, $n:expr, $bytes:expr) => {{
            let p: *mut $ty = xmalloc($n);
            for i in 0..$n {
                let q = off + i*$bytes;
                let mut arr: [u8; $bytes] = [0; $bytes];
                arr.copy_from_slice(&buf[q..q+$bytes]);
                *p.add(i) = <$ty>::from_le_bytes(arr);
            }
            off += $n * $bytes;
            p
        }}; }
        ACCEPT_TABLE = take!(u32, K1, 4);
        REJECT_W     = take!(i16, B2_SLOTS, 2);
        REF_VEC      = take!(f64, K3 * E_DIM, 8);
        W1_MAT       = take!(f64, H_DIM * E_DIM, 8);
        B1_VEC       = take!(f64, H_DIM, 8);
        W2_MAT       = take!(f64, C_CLS * H_DIM, 8);
        B2_VEC       = take!(f64, C_CLS, 8);
        COS_TABLE    = take!(f64, E_DIM * DIM_D, 8);
        // input.bin
        ITEMS = xmalloc(N * DIM_D);
        let mut f = File::open(format!("{}/input.bin", DATA_DIR)).unwrap();
        let slice = std::slice::from_raw_parts_mut(ITEMS, N * DIM_D);
        f.read_exact(slice).unwrap();
        DECISIONS = xmalloc(N);
        std::ptr::write_bytes(DECISIONS, 0, N);
        EMB_ALL = xmalloc(N * E_DIM);
        // config
        let text = std::fs::read_to_string(format!("{}/config.txt", DATA_DIR)).unwrap();
        for line in text.lines() {
            let mut it = line.split_whitespace();
            let k = it.next().unwrap_or("");
            let v = it.next().unwrap_or("0");
            match k {
                "T2" => T2_CFG = v.parse().unwrap_or(0),
                "T3" => T3_CFG = v.parse().unwrap_or(0.0),
                _ => {}
            }
        }
        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn stage1(py: Python<'_>, cid: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = cid * CHUNK_SIZE;
        let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
        for i in lo..hi {
            let it = ITEMS.add(i * DIM_D);
            if stage1_decide(it) { *DECISIONS.add(i) = ACCEPT_S1; }
        }
    });
    Ok(0)
}
#[pyfunction]
fn stage2(py: Python<'_>, cid: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = cid * CHUNK_SIZE;
        let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
        for i in lo..hi {
            if *DECISIONS.add(i) != 0 { continue; }
            let it = ITEMS.add(i * DIM_D);
            let s = stage2_score(it);
            if s > T2_CFG { *DECISIONS.add(i) = REJECT_S2; }
        }
    });
    Ok(0)
}
#[pyfunction]
fn stage3(py: Python<'_>, cid: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = cid * CHUNK_SIZE;
        let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
        for i in lo..hi {
            if *DECISIONS.add(i) != 0 { continue; }
            let it  = ITEMS.add(i * DIM_D);
            let emb = EMB_ALL.add(i * E_DIM);
            stage3_embed(it, emb);
            let (best, bs) = stage3_best(emb);
            if bs > T3_CFG { *DECISIONS.add(i) = ACCEPT_S3_BASE | best as i32; }
        }
    });
    Ok(0)
}
#[pyfunction]
fn stage4(py: Python<'_>, cid: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = cid * CHUNK_SIZE;
        let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
        for i in lo..hi {
            if *DECISIONS.add(i) != 0 { continue; }
            let emb = EMB_ALL.add(i * E_DIM);
            let cls = stage4_classify(emb);
            *DECISIONS.add(i) = CLASS_BASE | cls as i32;
        }
    });
    Ok(0)
}
#[pyfunction]
fn result(py: Python<'_>) -> PyResult<u64> {
    let cs = py.allow_threads(|| unsafe {
        let mut cs: u32 = 0;
        for i in 0..N { cs = cs.wrapping_add(*DECISIONS.add(i) as u32); }
        cs as u64
    });
    Ok(cs)
}

#[pymodule]
fn sucuri_cip(py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(init,    m)?)?;
    m.add_function(wrap_pyfunction!(stage1,  m)?)?;
    m.add_function(wrap_pyfunction!(stage2,  m)?)?;
    m.add_function(wrap_pyfunction!(stage3,  m)?)?;
    m.add_function(wrap_pyfunction!(stage4,  m)?)?;
    m.add_function(wrap_pyfunction!(result,  m)?)?;
    let _ = py;
    Ok(())
}
"""


PY_DRIVER = r"""#!/usr/bin/env python3
'''Sucuri Cascading Inference Pipeline driver.

Builds a pyDF.DFGraph with one Node per (chunk, stage), edged by per-chunk
dependency chain.  A final sink Node has n_chunks input ports (or a sync
tree if n_chunks > some practical cap) and triggers the result extraction.
'''
import argparse, os, sys, time, struct

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None:
    sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Scheduler

MAX_FANIN    = 30


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rust-so", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--workers", type=int, required=True)
    args = ap.parse_args()

    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    N = int(cfg["N"]); CHUNK_SIZE = int(cfg["CHUNK_SIZE"])
    n_chunks = (N + CHUNK_SIZE - 1) // CHUNK_SIZE

    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_cip", args.rust_so)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    mod.init()

    g = DFGraph()

    def seed_fn(): return 0
    seed = Node(seed_fn, 0); g.add(seed)

    def mk(stage_fn, cid):
        def f(_args):
            stage_fn(cid); return 0
        return f

    # 4 nodes per chunk: stage1..4, chained.
    nodes_per_chunk = []
    for k in range(n_chunks):
        s1 = Node(mk(mod.stage1, k), 1); g.add(s1)
        s2 = Node(mk(mod.stage2, k), 1); g.add(s2)
        s3 = Node(mk(mod.stage3, k), 1); g.add(s3)
        s4 = Node(mk(mod.stage4, k), 1); g.add(s4)
        seed.add_edge(s1, 0)
        s1.add_edge(s2, 0)
        s2.add_edge(s3, 0)
        s3.add_edge(s4, 0)
        nodes_per_chunk.append((s1, s2, s3, s4))

    # Sync tree over stage4 outputs (max MAX_FANIN per sync node).
    def make_sink(arity):
        def f(_args): return 0
        return Node(f, arity)

    current = [c[3] for c in nodes_per_chunk]
    while len(current) > 1:
        next_level = []
        for i in range(0, len(current), MAX_FANIN):
            batch = current[i:i + MAX_FANIN]
            sync = make_sink(len(batch)); g.add(sync)
            for port, child in enumerate(batch):
                child.add_edge(sync, port)
            next_level.append(sync)
        current = next_level

    # Result sink with 1 input from root sync.
    root = current[0]
    def res_fn(_args): return 0
    res_node = Node(res_fn, 1); g.add(res_node)
    root.add_edge(res_node, 0)

    t0 = time.perf_counter()
    sched = Scheduler(g, n_workers=args.workers, mpi_enabled=False)
    sched.start()
    t1 = time.perf_counter()

    cs = mod.result()
    print(f"CHECKSUM={cs}")
    print(f"RUNTIME_SEC={t1 - t0}")


if __name__ == "__main__":
    main()
"""


def emit(project_dir, py_driver_path, data_dir, N, CHUNK_SIZE):
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs = (RS_TEMPLATE
          .replace("__N__", str(N))
          .replace("__CHUNK_SIZE__", str(CHUNK_SIZE))
          .replace("__DATA_DIR__", data_dir))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs)
    with open(py_driver_path, "w") as f:
        f.write(PY_DRIVER)
    os.chmod(py_driver_path, 0o755)
    print(f"[gen_cip_sucuri] wrote {project_dir}/ + {py_driver_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--py-driver", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.project_dir, args.py_driver, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CHUNK_SIZE"]))


if __name__ == "__main__":
    main()
