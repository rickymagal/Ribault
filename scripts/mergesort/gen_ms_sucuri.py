#!/usr/bin/env python3
"""Generate Sucuri mergesort variant: PyO3 Rust extension exposing
init/leaf/merge/result + a Python driver that constructs the DFGraph
mirroring the binary tree topology from gen_input.py's tree.bin.

Same algorithm and topology as Ribault-Hs/STRAT (binary tree of leaves
+ merges with the same lo/mid/hi ranges, same leaf cutoff). Sucuri's
pyDF.DFGraph orchestrates the DAG: each leaf node calls into the Rust
crate's leaf(idx) PyO3 function; each merge node has 2 inputs (from its
2 children) and calls into merge(idx). The Python orchestrator runs on
free-threaded Python 3.14t (PYTHON_GIL=0).
"""

import argparse, os


CARGO_TOML = """[package]
name = "sucuri_ms"
version = "0.1.0"
edition = "2021"

[lib]
name = "sucuri_ms"
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


RS_TEMPLATE = r"""// Sucuri mergesort Rust leaf: PyO3 extension exposing init/leaf/merge/result.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use pyo3::prelude::*;
use std::os::raw::c_int;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};

const N: usize        = __N__;
const CUTOFF: usize   = __CUTOFF__;
const N_LEAVES: usize = __N_LEAVES__;
const N_MERGES: usize = __N_MERGES__;
const DATA_DIR: &str  = "__DATA_DIR__";

static INIT_DONE: AtomicBool = AtomicBool::new(false);

static mut ARR: *mut c_int = std::ptr::null_mut();
static mut TMP: *mut c_int = std::ptr::null_mut();
static mut LEAF_LO:  *mut c_int = std::ptr::null_mut();
static mut LEAF_HI:  *mut c_int = std::ptr::null_mut();
static mut MERGE_LO: *mut c_int = std::ptr::null_mut();
static mut MERGE_MID:*mut c_int = std::ptr::null_mut();
static mut MERGE_HI: *mut c_int = std::ptr::null_mut();
static mut PRE_SUM: u64 = 0;

unsafe fn xmalloc_i32(n: usize) -> *mut c_int {
    let p = libc::malloc(n * std::mem::size_of::<c_int>()) as *mut c_int;
    assert!(!p.is_null());
    p
}

unsafe fn load_tree() {
    let path = format!("{}/tree.bin", DATA_DIR);
    let mut f = File::open(&path).unwrap();
    let mut hdr = [0u8; 8];
    f.read_exact(&mut hdr).unwrap();
    LEAF_LO   = xmalloc_i32(N_LEAVES);
    LEAF_HI   = xmalloc_i32(N_LEAVES);
    MERGE_LO  = xmalloc_i32(N_MERGES);
    MERGE_MID = xmalloc_i32(N_MERGES);
    MERGE_HI  = xmalloc_i32(N_MERGES);
    let mut buf8 = [0u8; 8];
    for i in 0..N_LEAVES {
        f.read_exact(&mut buf8).unwrap();
        *LEAF_LO.add(i) = i32::from_le_bytes([buf8[0],buf8[1],buf8[2],buf8[3]]);
        *LEAF_HI.add(i) = i32::from_le_bytes([buf8[4],buf8[5],buf8[6],buf8[7]]);
    }
    let mut buf16 = [0u8; 16];
    for i in 0..N_MERGES {
        f.read_exact(&mut buf16).unwrap();
        *MERGE_LO.add(i)  = i32::from_le_bytes([buf16[0],buf16[1],buf16[2],buf16[3]]);
        *MERGE_MID.add(i) = i32::from_le_bytes([buf16[4],buf16[5],buf16[6],buf16[7]]);
        *MERGE_HI.add(i)  = i32::from_le_bytes([buf16[8],buf16[9],buf16[10],buf16[11]]);
        // buf16[12..16] = level, unused in Sucuri (DFGraph handles deps)
    }
}

#[inline(always)]
unsafe fn insertion_sort_at(lo: usize, hi: usize) {
    let mut i = lo + 1;
    while i < hi {
        let x = *ARR.add(i);
        let mut j = i;
        while j > lo && *ARR.add(j - 1) > x {
            *ARR.add(j) = *ARR.add(j - 1);
            j -= 1;
        }
        *ARR.add(j) = x;
        i += 1;
    }
}

#[inline(always)]
unsafe fn merge_at(lo: usize, mid: usize, hi: usize) {
    let mut i = lo; let mut j = mid; let mut k = lo;
    while i < mid && j < hi {
        let av = *ARR.add(i);
        let bv = *ARR.add(j);
        if av <= bv { *TMP.add(k) = av; i += 1; }
        else        { *TMP.add(k) = bv; j += 1; }
        k += 1;
    }
    while i < mid { *TMP.add(k) = *ARR.add(i); i += 1; k += 1; }
    while j < hi  { *TMP.add(k) = *ARR.add(j); j += 1; k += 1; }
    let mut p = lo;
    while p < hi { *ARR.add(p) = *TMP.add(p); p += 1; }
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) { return; }
        ARR = xmalloc_i32(N);
        TMP = xmalloc_i32(N);
        // Load input.bin
        let path = format!("{}/input.bin", DATA_DIR);
        let mut f = File::open(&path).unwrap();
        let mut buf = vec![0u8; N * 4];
        f.read_exact(&mut buf).unwrap();
        for i in 0..N {
            *ARR.add(i) = i32::from_le_bytes([buf[i*4],buf[i*4+1],buf[i*4+2],buf[i*4+3]]);
        }
        // Pre-sort checksum
        let mut pre: u64 = 0;
        for i in 0..N { pre = (pre + (*ARR.add(i) as u32 as u64)) & 0xFFFFFFFF_u64; }
        PRE_SUM = pre;
        load_tree();
        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn leaf(py: Python<'_>, idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = *LEAF_LO.add(idx) as usize;
        let hi = *LEAF_HI.add(idx) as usize;
        insertion_sort_at(lo, hi);
    });
    Ok(0)
}

#[pyfunction]
fn merge(py: Python<'_>, idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo  = *MERGE_LO.add(idx)  as usize;
        let mid = *MERGE_MID.add(idx) as usize;
        let hi  = *MERGE_HI.add(idx)  as usize;
        merge_at(lo, mid, hi);
    });
    Ok(0)
}

#[pyfunction]
fn result(py: Python<'_>) -> PyResult<u64> {
    let cs = py.allow_threads(|| unsafe {
        let mut cs: u64 = 0;
        let mut ok = true;
        for i in 0..N {
            if i > 0 && *ARR.add(i) < *ARR.add(i - 1) { ok = false; }
            cs = (cs + (*ARR.add(i) as u32 as u64)) & 0xFFFFFFFF_u64;
        }
        if !ok { eprintln!("FATAL: not sorted"); std::process::exit(1); }
        if cs != PRE_SUM { eprintln!("FATAL: checksum changed pre={} post={}", PRE_SUM, cs); std::process::exit(1); }
        cs
    });
    Ok(cs)
}

#[pymodule]
fn sucuri_ms(py: Python, module: &Bound<'_, PyModule>) -> PyResult<()> {
    module.add_function(wrap_pyfunction!(init, module)?)?;
    module.add_function(wrap_pyfunction!(leaf, module)?)?;
    module.add_function(wrap_pyfunction!(merge, module)?)?;
    module.add_function(wrap_pyfunction!(result, module)?)?;
    let _ = py;
    Ok(())
}
"""


PY_DRIVER = r"""#!/usr/bin/env python3
'''Sucuri mergesort driver.

Rebuilds the binary tree topology from N + CUTOFF (deterministic
top-down bisection — same as gen_input.py's build_tree). Constructs a
pyDF.DFGraph with:
  seed -> leaf_0..leaf_{NL-1}             (NL outgoing edges from seed)
  leaf_i and leaf_j -> merge_k            (each merge gets its 2 children)
  merge_p -> merge_q                       (merges feed parent merges)
  root_merge -> result_node

All Oper values are sentinel 0 — the actual sort state is in the Rust
extension's static-mut pointer slots.
'''
import argparse, os, sys, time, struct

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None:
    sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Scheduler


def build_tree(N, cutoff):
    # Top-down binary bisection identical to gen_input.py's build_tree.
    # Returns (leaves, merges, root_kind, root_id).
    leaves = []
    merges = []
    def recurse(lo, hi):
        if hi - lo <= cutoff:
            i = len(leaves); leaves.append((lo, hi)); return ("leaf", i)
        mid = lo + (hi - lo) // 2
        left  = recurse(lo, mid)
        right = recurse(mid, hi)
        i = len(merges); merges.append((lo, mid, hi, left, right))
        return ("merge", i)
    root_kind, root_id = recurse(0, N)
    return leaves, merges, root_kind, root_id


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
    N = cfg["N"]; CUTOFF = cfg["CUTOFF"]

    # Load Rust extension
    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_ms", args.rust_so)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)

    # Initialize (loads input.bin, allocates, loads tree.bin, computes pre-checksum).
    mod.init()

    leaves, merges, root_kind, root_id = build_tree(N, CUTOFF)
    NL = len(leaves); NM = len(merges)

    g = DFGraph()

    # Seed (inputn=0) — emits one sentinel to every leaf.
    def seed_fn():
        return 0
    seed = Node(seed_fn, 0)
    g.add(seed)

    # Leaf nodes (each has 1 input from seed).
    def make_leaf(i):
        def f(_args):
            mod.leaf(i)
            return 0
        return f
    leaf_nodes = [Node(make_leaf(i), 1) for i in range(NL)]
    for n in leaf_nodes: g.add(n)
    for n in leaf_nodes: seed.add_edge(n, 0)

    # Merge nodes (each has 2 inputs — from its 2 children).
    def make_merge(i):
        def f(_args):
            mod.merge(i)
            return 0
        return f
    merge_nodes = [Node(make_merge(i), 2) for i in range(NM)]
    for n in merge_nodes: g.add(n)
    # Wire children -> parent merge. children are (kind, id) tuples.
    for mi, (_lo, _mid, _hi, left, right) in enumerate(merges):
        parent = merge_nodes[mi]
        for port, (kind, idx) in enumerate([left, right]):
            child = leaf_nodes[idx] if kind == "leaf" else merge_nodes[idx]
            child.add_edge(parent, port)

    # Result sink (1 input from root).
    def result_fn(_args):
        return 0
    result_node = Node(result_fn, 1)
    g.add(result_node)
    root_node = leaf_nodes[root_id] if root_kind == "leaf" else merge_nodes[root_id]
    root_node.add_edge(result_node, 0)

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


def build_tree(N, cutoff):
    leaves = []; merges = []
    def recurse(lo, hi):
        if hi - lo <= cutoff:
            i = len(leaves); leaves.append((lo, hi)); return ("leaf", i)
        mid = lo + (hi - lo) // 2
        l = recurse(lo, mid); r = recurse(mid, hi)
        i = len(merges); merges.append((lo, mid, hi, l, r))
        return ("merge", i)
    recurse(0, N)
    return leaves, merges


def emit(project_dir, py_driver_path, data_dir, N, cutoff):
    leaves, merges = build_tree(N, cutoff)
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs_src = (RS_TEMPLATE
              .replace("__N__", str(N))
              .replace("__CUTOFF__", str(cutoff))
              .replace("__N_LEAVES__", str(len(leaves)))
              .replace("__N_MERGES__", str(len(merges)))
              .replace("__DATA_DIR__", data_dir))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    with open(py_driver_path, "w") as f:
        f.write(PY_DRIVER)
    os.chmod(py_driver_path, 0o755)
    print(f"[gen_ms_sucuri] wrote {project_dir}/ + {py_driver_path}")


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
    emit(args.project_dir, args.py_driver, os.path.abspath(args.data_dir),
         cfg["N"], cfg["CUTOFF"])


if __name__ == "__main__":
    main()
