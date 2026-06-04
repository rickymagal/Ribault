#!/usr/bin/env python3
"""Generate Sucuri sparse Cholesky variant: PyO3 Rust extension exposing
init/op/result + a Python driver that constructs the DFGraph from the
dag.bin produced by gen_input.py.

Each DAG op (POTRF/TRSM/SYRK/GEMM) becomes a Sucuri Node. Edges follow
the dependency list emitted in dag.bin. The Rust extension owns the
matrix as raw libc::malloc'd memory and exposes init() / op(idx) /
result() to Python; Python orchestrates the DAG via pyDF.DFGraph on
free-threaded 3.14t (PYTHON_GIL=0).
"""

import argparse, os, struct


CARGO_TOML = """[package]
name = "sucuri_sc"
version = "0.1.0"
edition = "2021"

[lib]
name = "sucuri_sc"
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


RS_TEMPLATE = r"""// Sucuri sparse Cholesky Rust kernel: PyO3 extension exposing init/op/result.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use pyo3::prelude::*;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};

const NB: usize       = __NB__;
const B:  usize       = __B__;
const N_OPS: usize    = __N_OPS__;
const DATA_DIR: &str  = "__DATA_DIR__";

static INIT_DONE: AtomicBool = AtomicBool::new(false);
static mut ARR: *mut f64 = std::ptr::null_mut();
// Per-op (kind, ti, tj, s1i, s1j, s2i, s2j) — packed into flat i32 arrays.
static mut OP_KIND: *mut i32 = std::ptr::null_mut();
static mut OP_TI: *mut i32 = std::ptr::null_mut();
static mut OP_TJ: *mut i32 = std::ptr::null_mut();
static mut OP_S1I: *mut i32 = std::ptr::null_mut();
static mut OP_S1J: *mut i32 = std::ptr::null_mut();
static mut OP_S2I: *mut i32 = std::ptr::null_mut();
static mut OP_S2J: *mut i32 = std::ptr::null_mut();

#[inline(always)] fn block_idx(i: usize, j: usize) -> usize { i*(i+1)/2 + j }

unsafe fn xmalloc_f64(n: usize) -> *mut f64 {
    let p = libc::malloc(n * 8) as *mut f64;
    assert!(!p.is_null()); p
}
unsafe fn xmalloc_i32(n: usize) -> *mut i32 {
    let p = libc::malloc(n * 4) as *mut i32;
    assert!(!p.is_null()); p
}

unsafe fn potrf_block(d: *mut f64) {
    for j in 0..B {
        let mut s = *d.add(j*B + j);
        for kk in 0..j { let x = *d.add(j*B + kk); s -= x*x; }
        let sq = s.sqrt();
        *d.add(j*B + j) = sq;
        let inv = 1.0 / sq;
        for i in (j+1)..B {
            let mut t = *d.add(i*B + j);
            for kk in 0..j { t -= *d.add(i*B + kk) * *d.add(j*B + kk); }
            *d.add(i*B + j) = t * inv;
        }
    }
    for i in 0..B { for j in (i+1)..B { *d.add(i*B + j) = 0.0; } }
}
unsafe fn trsm_block(x: *mut f64, l: *const f64) {
    for i in 0..B { for j in 0..B {
        let mut s = *x.add(i*B + j);
        for kk in 0..j { s -= *x.add(i*B + kk) * *l.add(j*B + kk); }
        *x.add(i*B + j) = s / *l.add(j*B + j);
    }}
}
unsafe fn syrk_block(c: *mut f64, a: *const f64) {
    for i in 0..B { for j in 0..=i {
        let mut s = 0.0;
        for kk in 0..B { s += *a.add(i*B + kk) * *a.add(j*B + kk); }
        *c.add(i*B + j) -= s;
    }}
}
unsafe fn gemm_block(c: *mut f64, a: *const f64, b_: *const f64) {
    for i in 0..B { for j in 0..B {
        let mut s = 0.0;
        for kk in 0..B { s += *a.add(i*B + kk) * *b_.add(j*B + kk); }
        *c.add(i*B + j) -= s;
    }}
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) { return; }
        let n_blocks = NB*(NB+1)/2;
        let total = n_blocks * B * B;
        ARR = xmalloc_f64(total);
        let mut f = File::open(format!("{}/A.bin", DATA_DIR)).unwrap();
        let mut buf = vec![0u8; total * 8];
        f.read_exact(&mut buf).unwrap();
        for i in 0..total {
            let off = i * 8;
            *ARR.add(i) = f64::from_le_bytes([buf[off],buf[off+1],buf[off+2],buf[off+3],
                                              buf[off+4],buf[off+5],buf[off+6],buf[off+7]]);
        }
        // Load DAG into flat arrays — skip dep lists since Python wires those.
        OP_KIND = xmalloc_i32(N_OPS);
        OP_TI = xmalloc_i32(N_OPS); OP_TJ = xmalloc_i32(N_OPS);
        OP_S1I = xmalloc_i32(N_OPS); OP_S1J = xmalloc_i32(N_OPS);
        OP_S2I = xmalloc_i32(N_OPS); OP_S2J = xmalloc_i32(N_OPS);
        let mut df = File::open(format!("{}/dag.bin", DATA_DIR)).unwrap();
        let mut hdr4 = [0u8; 4];
        df.read_exact(&mut hdr4).unwrap();
        for i in 0..N_OPS {
            let mut hdr = [0u8; 36];
            df.read_exact(&mut hdr).unwrap();
            let vals: [i32; 9] = std::array::from_fn(|k|
                i32::from_le_bytes([hdr[k*4], hdr[k*4+1], hdr[k*4+2], hdr[k*4+3]]));
            let n_deps = vals[8] as usize;
            if n_deps > 0 {
                let mut skip = vec![0u8; n_deps * 4];
                df.read_exact(&mut skip).unwrap();
            }
            *OP_KIND.add(i) = vals[0];
            *OP_TI.add(i) = vals[1]; *OP_TJ.add(i) = vals[2];
            *OP_S1I.add(i) = vals[3]; *OP_S1J.add(i) = vals[4];
            *OP_S2I.add(i) = vals[5]; *OP_S2J.add(i) = vals[6];
        }
        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn op(py: Python<'_>, idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let bs = B * B;
        let kind = *OP_KIND.add(idx);
        let ti = *OP_TI.add(idx) as usize;
        let tj = *OP_TJ.add(idx) as usize;
        let tgt = ARR.add(block_idx(ti, tj) * bs);
        match kind {
            0 => potrf_block(tgt),
            1 => trsm_block(tgt, ARR.add(block_idx(*OP_S1I.add(idx) as usize, *OP_S1J.add(idx) as usize) * bs)),
            2 => syrk_block(tgt, ARR.add(block_idx(*OP_S1I.add(idx) as usize, *OP_S1J.add(idx) as usize) * bs)),
            3 => gemm_block(tgt,
                            ARR.add(block_idx(*OP_S1I.add(idx) as usize, *OP_S1J.add(idx) as usize) * bs),
                            ARR.add(block_idx(*OP_S2I.add(idx) as usize, *OP_S2J.add(idx) as usize) * bs)),
            _ => panic!("unknown op kind"),
        }
    });
    Ok(0)
}

#[pyfunction]
fn result(py: Python<'_>) -> PyResult<u64> {
    let cs = py.allow_threads(|| unsafe {
        let n_blocks = NB*(NB+1)/2;
        let total = n_blocks * B * B;
        let mut cs: u64 = 0;
        for i in 0..total {
            let v = *ARR.add(i);
            let fixed = (v * 1e6) as i64;
            cs = (cs + (fixed as u32 as u64)) & 0xFFFFFFFF_u64;
        }
        cs
    });
    Ok(cs)
}

#[pymodule]
fn sucuri_sc(py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(init, m)?)?;
    m.add_function(wrap_pyfunction!(op, m)?)?;
    m.add_function(wrap_pyfunction!(result, m)?)?;
    let _ = py;
    Ok(())
}
"""


PY_DRIVER = r"""#!/usr/bin/env python3
'''Sucuri sparse Cholesky driver.

Reads dag.bin → for each op, creates a pyDF.Node with as many input
ports as it has dependencies; for each dependency edge, wires the
producer node to that input port. POTRFs (no dependency, level 1) are
fed by a single seed node so Sucuri has a starting frontier.
'''
import argparse, os, sys, time, struct

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None:
    sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Scheduler


def load_dag(path):
    with open(path, "rb") as f:
        data = f.read()
    n = struct.unpack_from("<i", data, 0)[0]
    off = 4
    ops = []
    for _ in range(n):
        kind, ti, tj, s1i, s1j, s2i, s2j, lvl, nd = struct.unpack_from("<9i", data, off)
        off += 36
        deps = list(struct.unpack_from("<%di" % nd, data, off)) if nd > 0 else []
        off += nd * 4
        ops.append({"kind": kind, "ti": ti, "tj": tj, "s1i": s1i, "s1j": s1j,
                    "s2i": s2i, "s2j": s2j, "level": lvl, "deps": deps})
    return ops


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rust-so", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--workers", type=int, required=True)
    args = ap.parse_args()

    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)

    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_sc", args.rust_so)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    mod.init()

    ops = load_dag(os.path.join(args.data_dir, "dag.bin"))
    nops = len(ops)

    g = DFGraph()

    def seed_fn():
        return 0
    seed = Node(seed_fn, 0)
    g.add(seed)

    # Each op-node: in-ports = max(1, len(deps)). Seed feeds dep-free ops.
    op_nodes = []
    def make_op(i):
        def f(_args):
            mod.op(i); return 0
        return f
    for i, opd in enumerate(ops):
        n_in = max(1, len(opd["deps"]))
        n = Node(make_op(i), n_in)
        g.add(n)
        op_nodes.append(n)

    for i, opd in enumerate(ops):
        if not opd["deps"]:
            seed.add_edge(op_nodes[i], 0)
        else:
            for port, dep_idx in enumerate(opd["deps"]):
                op_nodes[dep_idx].add_edge(op_nodes[i], port)

    # Result sink: 1 input from last op (which is the final TRSM/POTRF of the bottom row).
    def result_fn(_args): return 0
    result_node = Node(result_fn, 1)
    g.add(result_node)
    op_nodes[-1].add_edge(result_node, 0)

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


def count_ops(data_dir):
    with open(os.path.join(data_dir, "dag.bin"), "rb") as f:
        data = f.read(4)
    return struct.unpack("<i", data)[0]


def emit(project_dir, py_driver_path, data_dir, NB, B, n_ops):
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs_src = (RS_TEMPLATE
              .replace("__NB__", str(NB))
              .replace("__B__", str(B))
              .replace("__N_OPS__", str(n_ops))
              .replace("__DATA_DIR__", data_dir))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    with open(py_driver_path, "w") as f:
        f.write(PY_DRIVER)
    os.chmod(py_driver_path, 0o755)
    print(f"[gen_sc_sucuri] wrote {project_dir}/ + {py_driver_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--py-driver", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    n_ops = count_ops(args.data_dir)
    emit(args.project_dir, args.py_driver, os.path.abspath(args.data_dir),
         cfg["NB"], cfg["B"], n_ops)


if __name__ == "__main__":
    main()
