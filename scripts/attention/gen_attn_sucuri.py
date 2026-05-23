#!/usr/bin/env python3
"""Generate Sucuri attention end-to-end benchmark: PyO3 Rust extension
module with init/phaseA/phaseB/result, plus a Python driver that uses
Sucuri's pyDF.DFGraph to wire the 2-phase parallel dataflow.

Rust state lives in `static mut *mut c_double` slots filled by init()
via libc::malloc — the same raw-pointer pattern as gen_attn_rust.py and
gen_attn_rs_timely.py. We previously used `static mut Vec<f64>`, which
Rust 2024 treats as UB (Vec has Drop + complex layout under static mut).
The raw-pointer version compiles cleanly and is the same idiom every
other Rust variant in this benchmark uses for global activation buffers.

Algorithmic race-freedom: each phase-A / phase-B block writes a DISJOINT
range of rows in the activation buffers (rows [lo,hi)), and reads
weights / boundary buffers either read-only or with no aliasing to the
write region within a single phase. Within a phase, no two workers
collide; between phases, the pyDF.DFGraph barrier (K-way fan-in to a
sentinel node, then K-way fan-out to phase B) provides happens-before.
"""

import argparse, os


CARGO_TOML = """[package]
name = "sucuri_attn"
version = "0.1.0"
edition = "2021"

[lib]
name = "sucuri_attn"
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


RS_TEMPLATE = r"""// Sucuri attention Rust leaf: PyO3 extension module.
// Exposes init / phase_a / phase_b / result; the Python driver
// (pyDF.DFGraph) calls these from worker threads.
//
// State stored as `static mut *mut c_double` slots — the same raw-
// pointer pattern as ribault_rust and timely. NOT `static mut Vec<T>`
// (Rust 2024 UB).

#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use pyo3::prelude::*;
use std::os::raw::c_double;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};

const N: usize        = __N__;
const D: usize        = __D__;
const N_HEADS: usize  = __N_HEADS__;
const HEAD_DIM: usize = __HEAD_DIM__;
const D_FF: usize     = __D_FF__;
const VOCAB: usize    = __VOCAB__;
const N_BLOCKS: usize = __N_BLOCKS__;
const DATA_DIR: &str  = "__DATA_DIR__";

// Init guard — Python driver should call init() exactly once before any
// phase_a / phase_b. The atomic flag short-circuits idempotent re-entry.
static INIT_DONE: AtomicBool = AtomicBool::new(false);

// Weight + input pointers (raw, allocated via libc::malloc, never freed
// — process lifetime). One pointer per tensor.
static mut E_PTR:       *mut c_double = std::ptr::null_mut();
static mut PE_PTR:      *mut c_double = std::ptr::null_mut();
static mut WQ_PTR:      *mut c_double = std::ptr::null_mut();
static mut WK_PTR:      *mut c_double = std::ptr::null_mut();
static mut WV_PTR:      *mut c_double = std::ptr::null_mut();
static mut WO_PTR:      *mut c_double = std::ptr::null_mut();
static mut W1_PTR:      *mut c_double = std::ptr::null_mut();
static mut W2_PTR:      *mut c_double = std::ptr::null_mut();
static mut WU_PTR:      *mut c_double = std::ptr::null_mut();
static mut LN1W_PTR:    *mut c_double = std::ptr::null_mut();
static mut LN1B_PTR:    *mut c_double = std::ptr::null_mut();
static mut LN2W_PTR:    *mut c_double = std::ptr::null_mut();
static mut LN2B_PTR:    *mut c_double = std::ptr::null_mut();
static mut INPUT_PTR:   *mut u8       = std::ptr::null_mut();
static mut OUTPUT_PTR:  *mut u8       = std::ptr::null_mut();

// Activation buffers
static mut X_PTR:       *mut c_double = std::ptr::null_mut();
static mut XA_PTR:      *mut c_double = std::ptr::null_mut();
static mut Q_PTR:       *mut c_double = std::ptr::null_mut();
static mut K_PTR:       *mut c_double = std::ptr::null_mut();
static mut V_PTR:       *mut c_double = std::ptr::null_mut();
static mut ATTN_PTR:    *mut c_double = std::ptr::null_mut();
static mut XB_PTR:      *mut c_double = std::ptr::null_mut();
static mut FFN_PTR:     *mut c_double = std::ptr::null_mut();
static mut LOGITS_PTR:  *mut c_double = std::ptr::null_mut();

unsafe fn xmalloc_d(n: usize) -> *mut c_double {
    let p = libc::malloc(n * std::mem::size_of::<c_double>()) as *mut c_double;
    assert!(!p.is_null(), "malloc(d * {}) failed", n);
    p
}
unsafe fn xmalloc_u(n: usize) -> *mut u8 {
    let p = libc::malloc(n) as *mut u8;
    assert!(!p.is_null(), "malloc(u * {}) failed", n);
    p
}

unsafe fn read_bin_d(name: &str, dst: *mut c_double, count: usize) {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count * 8];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    for i in 0..count {
        let mut b = [0u8; 8];
        b.copy_from_slice(&buf[i*8..(i+1)*8]);
        *dst.add(i) = f64::from_le_bytes(b);
    }
}

unsafe fn read_bin_u8(name: &str, dst: *mut u8, count: usize) {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let s = std::slice::from_raw_parts_mut(dst, count);
    f.read_exact(s).unwrap_or_else(|e| panic!("read {}: {}", path, e));
}

#[inline] fn row_lo(b: usize) -> usize { b * N / N_BLOCKS }
#[inline] fn row_hi(b: usize) -> usize { if b == N_BLOCKS - 1 { N } else { (b + 1) * N / N_BLOCKS } }

unsafe fn matmul_block(lo: usize, hi: usize, a: *const c_double, b: *const c_double,
                       c: *mut c_double, k_dim: usize, n_dim: usize) {
    for m in lo..hi {
        let arow = a.add(m * k_dim);
        let crow = c.add(m * n_dim);
        for n in 0..n_dim { *crow.add(n) = 0.0; }
        for k in 0..k_dim {
            let av = *arow.add(k);
            let brow = b.add(k * n_dim);
            for n in 0..n_dim { *crow.add(n) += av * *brow.add(n); }
        }
    }
}

unsafe fn layer_norm_block(lo: usize, hi: usize, x: *const c_double, w: *const c_double,
                            b: *const c_double, out: *mut c_double, dim: usize) {
    let eps = 1e-5;
    for i in lo..hi {
        let xr = x.add(i * dim);
        let or_ = out.add(i * dim);
        let mut mean = 0.0;
        for j in 0..dim { mean += *xr.add(j); }
        mean /= dim as f64;
        let mut var = 0.0;
        for j in 0..dim { let d = *xr.add(j) - mean; var += d * d; }
        var /= dim as f64;
        let inv = 1.0 / (var + eps).sqrt();
        for j in 0..dim {
            *or_.add(j) = (*xr.add(j) - mean) * inv * *w.add(j) + *b.add(j);
        }
    }
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) {
            return;
        }
        E_PTR      = xmalloc_d(VOCAB * D);    read_bin_d("E.bin", E_PTR, VOCAB * D);
        PE_PTR     = xmalloc_d(N * D);        read_bin_d("PE.bin", PE_PTR, N * D);
        WQ_PTR     = xmalloc_d(D * D);        read_bin_d("W_Q.bin", WQ_PTR, D * D);
        WK_PTR     = xmalloc_d(D * D);        read_bin_d("W_K.bin", WK_PTR, D * D);
        WV_PTR     = xmalloc_d(D * D);        read_bin_d("W_V.bin", WV_PTR, D * D);
        WO_PTR     = xmalloc_d(D * D);        read_bin_d("W_O.bin", WO_PTR, D * D);
        W1_PTR     = xmalloc_d(D * D_FF);     read_bin_d("W_1.bin", W1_PTR, D * D_FF);
        W2_PTR     = xmalloc_d(D_FF * D);     read_bin_d("W_2.bin", W2_PTR, D_FF * D);
        WU_PTR     = xmalloc_d(D * VOCAB);    read_bin_d("W_U.bin", WU_PTR, D * VOCAB);
        LN1W_PTR   = xmalloc_d(D);            read_bin_d("LN_1_w.bin", LN1W_PTR, D);
        LN1B_PTR   = xmalloc_d(D);            read_bin_d("LN_1_b.bin", LN1B_PTR, D);
        LN2W_PTR   = xmalloc_d(D);            read_bin_d("LN_2_w.bin", LN2W_PTR, D);
        LN2B_PTR   = xmalloc_d(D);            read_bin_d("LN_2_b.bin", LN2B_PTR, D);
        INPUT_PTR  = xmalloc_u(N);            read_bin_u8("input_tokens.bin", INPUT_PTR, N);
        OUTPUT_PTR = xmalloc_u(N);

        X_PTR      = xmalloc_d(N * D);
        XA_PTR     = xmalloc_d(N * D);
        Q_PTR      = xmalloc_d(N * D);
        K_PTR      = xmalloc_d(N * D);
        V_PTR      = xmalloc_d(N * D);
        ATTN_PTR   = xmalloc_d(N * D);
        XB_PTR     = xmalloc_d(N * D);
        FFN_PTR    = xmalloc_d(N * D_FF);
        LOGITS_PTR = xmalloc_d(N * VOCAB);

        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn phase_a(py: Python<'_>, block_idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = row_lo(block_idx);
        let hi = row_hi(block_idx);

        // embed + sinusoidal pos
        for i in lo..hi {
            let tok = *INPUT_PTR.add(i) as usize;
            let ei = E_PTR.add(tok * D);
            let pi = PE_PTR.add(i * D);
            let xi = X_PTR.add(i * D);
            for j in 0..D { *xi.add(j) = *ei.add(j) + *pi.add(j); }
        }
        layer_norm_block(lo, hi, X_PTR, LN1W_PTR, LN1B_PTR, XA_PTR, D);
        matmul_block(lo, hi, XA_PTR, WQ_PTR, Q_PTR, D, D);
        matmul_block(lo, hi, XA_PTR, WK_PTR, K_PTR, D, D);
        matmul_block(lo, hi, XA_PTR, WV_PTR, V_PTR, D, D);
    });
    Ok(0)
}

#[pyfunction]
fn phase_b(py: Python<'_>, block_idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = row_lo(block_idx);
        let hi = row_hi(block_idx);
        let inv = 1.0 / (HEAD_DIM as f64).sqrt();
        let mut scores = vec![0.0_f64; N];

        for i in lo..hi {
            let ai = ATTN_PTR.add(i * D);
            for j in 0..D { *ai.add(j) = 0.0; }
        }
        for i in lo..hi {
            for h in 0..N_HEADS {
                let qhi = Q_PTR.add(i * D + h * HEAD_DIM);
                for j in 0..N {
                    let khj = K_PTR.add(j * D + h * HEAD_DIM);
                    let mut s = 0.0;
                    for k in 0..HEAD_DIM { s += *qhi.add(k) * *khj.add(k); }
                    scores[j] = s * inv;
                }
                let mut m = scores[0];
                for j in 1..N { if scores[j] > m { m = scores[j]; } }
                let mut sum = 0.0;
                for j in 0..N { scores[j] = (scores[j] - m).exp(); sum += scores[j]; }
                for j in 0..N { scores[j] /= sum; }
                let ahi = ATTN_PTR.add(i * D + h * HEAD_DIM);
                for j in 0..N {
                    let a = scores[j];
                    let vhj = V_PTR.add(j * D + h * HEAD_DIM);
                    for k in 0..HEAD_DIM { *ahi.add(k) += a * *vhj.add(k); }
                }
            }
        }
        matmul_block(lo, hi, ATTN_PTR, WO_PTR, XA_PTR, D, D);
        for i in lo..hi {
            let xi = X_PTR.add(i * D);
            let yi = XA_PTR.add(i * D);
            for j in 0..D { *xi.add(j) += *yi.add(j); }
        }
        layer_norm_block(lo, hi, X_PTR, LN2W_PTR, LN2B_PTR, XB_PTR, D);
        matmul_block(lo, hi, XB_PTR, W1_PTR, FFN_PTR, D, D_FF);
        for i in lo..hi {
            let hi_ = FFN_PTR.add(i * D_FF);
            for j in 0..D_FF { if *hi_.add(j) < 0.0 { *hi_.add(j) = 0.0; } }
        }
        matmul_block(lo, hi, FFN_PTR, W2_PTR, XA_PTR, D_FF, D);
        for i in lo..hi {
            let xi = X_PTR.add(i * D);
            let yi = XA_PTR.add(i * D);
            for j in 0..D { *xi.add(j) += *yi.add(j); }
        }
        matmul_block(lo, hi, X_PTR, WU_PTR, LOGITS_PTR, D, VOCAB);
        for i in lo..hi {
            let li = LOGITS_PTR.add(i * VOCAB);
            let mut best = 0usize;
            let mut bv = *li;
            for v in 1..VOCAB {
                let lv = *li.add(v);
                if lv > bv { bv = lv; best = v; }
            }
            *OUTPUT_PTR.add(i) = best as u8;
        }
    });
    Ok(0)
}

#[pyfunction]
fn result(py: Python<'_>) -> PyResult<u64> {
    let cs = py.allow_threads(|| unsafe {
        let mut cs: u64 = 0;
        for i in 0..N {
            cs = (cs + (i as u64 + 1) * (*OUTPUT_PTR.add(i) as u64)) & 0xFFFFFFFF_u64;
        }
        cs
    });
    Ok(cs)
}

#[pymodule]
fn sucuri_attn(py: Python, module: &Bound<'_, PyModule>) -> PyResult<()> {
    module.add_function(wrap_pyfunction!(init, module)?)?;
    module.add_function(wrap_pyfunction!(phase_a, module)?)?;
    module.add_function(wrap_pyfunction!(phase_b, module)?)?;
    module.add_function(wrap_pyfunction!(result, module)?)?;
    let _ = py;
    Ok(())
}
"""


PY_DRIVER = r"""#!/usr/bin/env python3
'''Sucuri attention driver.

Wires the 2-phase dataflow graph with pyDF.DFGraph:
  - 1 seed node (no inputs, emits "go")
  - K phase-A nodes (each has 1 input from seed; calls mod.phase_a(b))
  - 1 barrier node (K inputs from phase-A; emits "go" to phase B)
  - K phase-B nodes (each has 1 input from barrier; calls mod.phase_b(b))
  - 1 result node (K inputs from phase-B; final sentinel)

All Oper values are sentinels (the actual activation state lives in the
Rust module's static-mut pointer slots and is written via raw pointers
into disjoint per-block row ranges — no Python-side data flow needed).

Uses the Node(f, inputn) API + add_edge(dst, dstport): f receives a list
of input values (or no args for inputn=0) and returns a value that is
fan-broadcast over every outgoing edge.
'''
import argparse, os, sys, time

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None:
    sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Scheduler


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rust-so", required=True, help="Path to libsucuri_attn.so")
    ap.add_argument("--n-blocks", type=int, required=True)
    ap.add_argument("--workers", type=int, required=True)
    args = ap.parse_args()

    # Load the compiled PyO3 extension module from a file path.
    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_attn", args.rust_so)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)

    # Initialize all weights (single-threaded; called once before parallel phases).
    mod.init()

    K = args.n_blocks
    g = DFGraph()

    # Seed (inputn=0): emits a single "go" sentinel to every phase-A node.
    def seed_fn():
        return 0
    seed = Node(seed_fn, 0)
    g.add(seed)

    # Phase-A nodes: inputn=1 (the go sentinel), call mod.phase_a(b), emit 0.
    def make_phase_a(b):
        def f(_args):
            mod.phase_a(b)
            return 0
        return f
    phaseA_nodes = []
    for b in range(K):
        n = Node(make_phase_a(b), 1)
        g.add(n)
        phaseA_nodes.append(n)

    # Seed -> each phase-A on dstport 0
    for n in phaseA_nodes:
        seed.add_edge(n, 0)

    # Barrier: inputn=K (one per phase-A). When all fired, emit one "go".
    def barrier_fn(_args):
        return 0
    barrier = Node(barrier_fn, K)
    g.add(barrier)
    for i, n in enumerate(phaseA_nodes):
        n.add_edge(barrier, i)

    # Phase-B nodes: inputn=1 (go from barrier), call mod.phase_b(b), emit 0.
    def make_phase_b(b):
        def f(_args):
            mod.phase_b(b)
            return 0
        return f
    phaseB_nodes = []
    for b in range(K):
        n = Node(make_phase_b(b), 1)
        g.add(n)
        phaseB_nodes.append(n)

    # Barrier -> each phase-B on dstport 0
    for n in phaseB_nodes:
        barrier.add_edge(n, 0)

    # Result aggregator: inputn=K, final sink. We do NOT compute the
    # checksum inside the graph — it's deterministic from OUTPUT_PTR and
    # the driver reads it via mod.result() after the scheduler returns.
    def result_fn(_args):
        return 0
    result_node = Node(result_fn, K)
    g.add(result_node)
    for i, n in enumerate(phaseB_nodes):
        n.add_edge(result_node, i)

    t0 = time.perf_counter()
    scheduler = Scheduler(g, n_workers=args.workers, mpi_enabled=False)
    scheduler.start()
    t1 = time.perf_counter()

    cs = mod.result()
    print(f"CHECKSUM={cs}")
    print(f"RUNTIME_SEC={t1 - t0}")


if __name__ == "__main__":
    main()
"""


def emit(project_dir, py_driver_path, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs_src = (RS_TEMPLATE
              .replace("__N__", str(N))
              .replace("__D__", str(D))
              .replace("__N_HEADS__", str(n_heads))
              .replace("__HEAD_DIM__", str(head_dim))
              .replace("__D_FF__", str(d_ff))
              .replace("__VOCAB__", str(vocab))
              .replace("__N_BLOCKS__", str(n_blocks))
              .replace("__DATA_DIR__", data_dir))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    with open(py_driver_path, "w") as f:
        f.write(PY_DRIVER)
    os.chmod(py_driver_path, 0o755)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--py-driver", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.project_dir, args.py_driver, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
