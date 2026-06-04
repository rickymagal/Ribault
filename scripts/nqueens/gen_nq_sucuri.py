#!/usr/bin/env python3
"""Generate Sucuri N-Queens variant: PyO3 Rust extension + pyDF.DFGraph
with one Node per prefix state."""

import argparse, os, struct


CARGO_TOML = """[package]
name = "sucuri_nq"
version = "0.1.0"
edition = "2021"

[lib]
name = "sucuri_nq"
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


RS_TEMPLATE = r"""// Sucuri N-Queens Rust kernel (PyO3 extension).
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use pyo3::prelude::*;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

const MAX_N:    usize = 16;
const N:        usize = __N__;
const CUTOFF:   usize = __CUTOFF__;
const N_STATES: usize = __N_STATES__;
const DATA_DIR: &str  = "__DATA_DIR__";

static INIT_DONE: AtomicBool = AtomicBool::new(false);
static mut STATES: *mut i32 = std::ptr::null_mut();
static TOTAL: AtomicU64 = AtomicU64::new(0);

unsafe fn xmalloc<T>(n: usize) -> *mut T {
    libc::malloc(n * std::mem::size_of::<T>()) as *mut T
}

#[inline(always)]
unsafe fn safe_q(queens: &[i32; MAX_N], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                                  { return false; }
        if c - r as i32 == col - row as i32          { return false; }
        if c + r as i32 == col + row as i32          { return false; }
    }
    true
}
unsafe fn solve_sub(queens: &mut [i32; MAX_N], row: usize) -> u64 {
    if row == N { return 1; }
    let mut cnt = 0u64;
    for c in 0..N as i32 {
        if safe_q(queens, row, c) {
            queens[row] = c;
            cnt += solve_sub(queens, row + 1);
        }
    }
    cnt
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) { return; }
        STATES = xmalloc(N_STATES * CUTOFF);
        let mut f = File::open(format!("{}/states.bin", DATA_DIR)).unwrap();
        let mut hdr = [0u8; 8]; f.read_exact(&mut hdr).unwrap();
        let mut buf = vec![0u8; N_STATES * CUTOFF * 4];
        f.read_exact(&mut buf).unwrap();
        for i in 0..N_STATES * CUTOFF {
            let off = i * 4;
            *STATES.add(i) = i32::from_le_bytes([buf[off],buf[off+1],buf[off+2],buf[off+3]]);
        }
        TOTAL.store(0, Ordering::Relaxed);
        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn solve(py: Python<'_>, sid: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let mut queens = [0i32; MAX_N];
        for r in 0..CUTOFF { queens[r] = *STATES.add(sid * CUTOFF + r); }
        let cnt = solve_sub(&mut queens, CUTOFF);
        TOTAL.fetch_add(cnt, Ordering::Relaxed);
    });
    Ok(0)
}

#[pyfunction]
fn result(_py: Python<'_>) -> PyResult<u64> { Ok(TOTAL.load(Ordering::Relaxed)) }

#[pymodule]
fn sucuri_nq(py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(init,   m)?)?;
    m.add_function(wrap_pyfunction!(solve,  m)?)?;
    m.add_function(wrap_pyfunction!(result, m)?)?;
    let _ = py;
    Ok(())
}
"""


PY_DRIVER = r"""#!/usr/bin/env python3
'''Sucuri N-Queens driver.'''
import argparse, os, sys, time

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None: sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Scheduler

MAX_FANIN = 30


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
    n_states = int(cfg["N_STATES"])

    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_nq", args.rust_so)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    mod.init()

    g = DFGraph()
    def seed_fn(): return 0
    seed = Node(seed_fn, 0); g.add(seed)

    def mk(sid):
        def f(_args): mod.solve(sid); return 0
        return f
    leaves = []
    for s in range(n_states):
        nd = Node(mk(s), 1); g.add(nd); seed.add_edge(nd, 0); leaves.append(nd)

    # Sync tree
    def sink(arity):
        def f(_args): return 0
        return Node(f, arity)
    current = leaves
    while len(current) > 1:
        nxt = []
        for i in range(0, len(current), MAX_FANIN):
            batch = current[i:i + MAX_FANIN]
            s = sink(len(batch)); g.add(s)
            for port, child in enumerate(batch): child.add_edge(s, port)
            nxt.append(s)
        current = nxt
    root = current[0] if current else seed
    res = sink(1); g.add(res); root.add_edge(res, 0)

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


def emit(project_dir, py_driver_path, data_dir, N, CUTOFF, n_states):
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src"); os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f: f.write(CARGO_TOML)
    rs = (RS_TEMPLATE
          .replace("__N__", str(N)).replace("__CUTOFF__", str(CUTOFF))
          .replace("__N_STATES__", str(n_states)).replace("__DATA_DIR__", data_dir))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f: f.write(rs)
    with open(py_driver_path, "w") as f: f.write(PY_DRIVER)
    os.chmod(py_driver_path, 0o755)
    print(f"[gen_nq_sucuri] wrote {project_dir}/ + {py_driver_path}")


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
         int(cfg["N"]), int(cfg["CUTOFF"]), int(cfg["N_STATES"]))


if __name__ == "__main__":
    main()
