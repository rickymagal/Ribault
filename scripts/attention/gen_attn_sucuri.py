#!/usr/bin/env python3
"""Generate Sucuri attention end-to-end benchmark: PyO3 Rust extension
module with init/phaseA/phaseB/result, plus a Python driver that uses
Sucuri's pyDF.DFGraph to wire the 2-phase parallel dataflow.

The Rust module owns all global mutable state (weights, activations,
output tokens) via `static mut`. Python invokes phaseA(b) and phaseB(b)
across N_BLOCKS blocks; pyDF schedules them across worker threads.
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
"""


RS_TEMPLATE = r"""// Sucuri attention Rust leaf: PyO3 extension module exposing init/phaseA/phaseB/result.
use pyo3::prelude::*;
use std::fs::File;
use std::io::Read;
use std::sync::atomic::{AtomicBool, Ordering};

const N: usize       = __N__;
const D: usize       = __D__;
const N_HEADS: usize = __N_HEADS__;
const HEAD_DIM: usize= __HEAD_DIM__;
const D_FF: usize    = __D_FF__;
const VOCAB: usize   = __VOCAB__;
const N_BLOCKS: usize= __N_BLOCKS__;
const DATA_DIR: &str = "__DATA_DIR__";

static INIT_DONE: AtomicBool = AtomicBool::new(false);

static mut E: Vec<f64> = Vec::new();
static mut PE: Vec<f64> = Vec::new();
static mut W_Q: Vec<f64> = Vec::new();
static mut W_K_PTR: Vec<f64> = Vec::new();
static mut W_V: Vec<f64> = Vec::new();
static mut W_O: Vec<f64> = Vec::new();
static mut W_1: Vec<f64> = Vec::new();
static mut W_2: Vec<f64> = Vec::new();
static mut W_U: Vec<f64> = Vec::new();
static mut LN_1_W: Vec<f64> = Vec::new();
static mut LN_1_B: Vec<f64> = Vec::new();
static mut LN_2_W: Vec<f64> = Vec::new();
static mut LN_2_B: Vec<f64> = Vec::new();
static mut INPUT_TOKENS: Vec<u8> = Vec::new();
static mut OUTPUT_TOKENS: Vec<u8> = Vec::new();

static mut X_BUF: Vec<f64> = Vec::new();
static mut XA_BUF: Vec<f64> = Vec::new();
static mut Q_BUF: Vec<f64> = Vec::new();
static mut K_BUF: Vec<f64> = Vec::new();
static mut V_BUF: Vec<f64> = Vec::new();
static mut ATTN_BUF: Vec<f64> = Vec::new();
static mut XB_BUF: Vec<f64> = Vec::new();
static mut FFN_H: Vec<f64> = Vec::new();
static mut LOGITS_BUF: Vec<f64> = Vec::new();

fn read_doubles(name: &str, count: usize) -> Vec<f64> {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count * 8];
    f.read_exact(&mut buf).unwrap();
    let mut out = vec![0.0_f64; count];
    for i in 0..count {
        let mut b = [0u8; 8];
        b.copy_from_slice(&buf[i*8..(i+1)*8]);
        out[i] = f64::from_le_bytes(b);
    }
    out
}
fn read_bytes(name: &str, count: usize) -> Vec<u8> {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap();
    let mut buf = vec![0u8; count]; f.read_exact(&mut buf).unwrap(); buf
}

#[inline] fn row_lo(b: usize) -> usize { b * N / N_BLOCKS }
#[inline] fn row_hi(b: usize) -> usize { if b == N_BLOCKS - 1 { N } else { (b + 1) * N / N_BLOCKS } }

unsafe fn matmul_block(lo: usize, hi: usize, a: &[f64], b: &[f64], c: &mut [f64], k_dim: usize, n_dim: usize) {
    for m in lo..hi {
        for n in 0..n_dim { c[m * n_dim + n] = 0.0; }
        for k in 0..k_dim {
            let av = a[m * k_dim + k];
            for n in 0..n_dim { c[m * n_dim + n] += av * b[k * n_dim + n]; }
        }
    }
}
unsafe fn layer_norm_block(lo: usize, hi: usize, x: &[f64], w: &[f64], b: &[f64], out: &mut [f64], dim: usize) {
    let eps = 1e-5;
    for i in lo..hi {
        let mut mean = 0.0;
        for j in 0..dim { mean += x[i*dim + j]; }
        mean /= dim as f64;
        let mut var = 0.0;
        for j in 0..dim { let d = x[i*dim + j] - mean; var += d*d; }
        var /= dim as f64;
        let inv = 1.0 / (var + eps).sqrt();
        for j in 0..dim { out[i*dim + j] = (x[i*dim + j] - mean) * inv * w[j] + b[j]; }
    }
}

#[pyfunction]
fn init(py: Python<'_>) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        if INIT_DONE.load(Ordering::SeqCst) { return; }
        E       = read_doubles("E.bin",      VOCAB * D);
        PE      = read_doubles("PE.bin",     N * D);
        W_Q     = read_doubles("W_Q.bin",    D * D);
        W_K_PTR = read_doubles("W_K.bin",    D * D);
        W_V     = read_doubles("W_V.bin",    D * D);
        W_O     = read_doubles("W_O.bin",    D * D);
        W_1     = read_doubles("W_1.bin",    D * D_FF);
        W_2     = read_doubles("W_2.bin",    D_FF * D);
        W_U     = read_doubles("W_U.bin",    D * VOCAB);
        LN_1_W  = read_doubles("LN_1_w.bin", D);
        LN_1_B  = read_doubles("LN_1_b.bin", D);
        LN_2_W  = read_doubles("LN_2_w.bin", D);
        LN_2_B  = read_doubles("LN_2_b.bin", D);
        INPUT_TOKENS  = read_bytes("input_tokens.bin", N);
        OUTPUT_TOKENS = vec![0u8; N];
        X_BUF      = vec![0.0; N * D];
        XA_BUF     = vec![0.0; N * D];
        Q_BUF      = vec![0.0; N * D];
        K_BUF      = vec![0.0; N * D];
        V_BUF      = vec![0.0; N * D];
        ATTN_BUF   = vec![0.0; N * D];
        XB_BUF     = vec![0.0; N * D];
        FFN_H      = vec![0.0; N * D_FF];
        LOGITS_BUF = vec![0.0; N * VOCAB];
        INIT_DONE.store(true, Ordering::SeqCst);
    });
    Ok(0)
}

#[pyfunction]
fn phase_a(py: Python<'_>, block_idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = row_lo(block_idx); let hi = row_hi(block_idx);
        for i in lo..hi {
            let tok = INPUT_TOKENS[i] as usize;
            for j in 0..D { X_BUF[i*D + j] = E[tok*D + j] + PE[i*D + j]; }
        }
        // SAFETY: we re-split &mut references inline to avoid aliasing issues
        let (x_buf_r, xa_buf_w) = (&X_BUF[..], &mut XA_BUF[..]);
        layer_norm_block(lo, hi, x_buf_r, &LN_1_W, &LN_1_B, xa_buf_w, D);
        matmul_block(lo, hi, &XA_BUF, &W_Q,     &mut Q_BUF, D, D);
        matmul_block(lo, hi, &XA_BUF, &W_K_PTR, &mut K_BUF, D, D);
        matmul_block(lo, hi, &XA_BUF, &W_V,     &mut V_BUF, D, D);
    });
    Ok(0)
}

#[pyfunction]
fn phase_b(py: Python<'_>, block_idx: usize) -> PyResult<i64> {
    py.allow_threads(|| unsafe {
        let lo = row_lo(block_idx); let hi = row_hi(block_idx);
        let inv = 1.0 / (HEAD_DIM as f64).sqrt();
        let mut scores = vec![0.0_f64; N];
        for i in lo..hi {
            for j in 0..D { ATTN_BUF[i*D + j] = 0.0; }
        }
        for i in lo..hi {
            for h in 0..N_HEADS {
                for j in 0..N {
                    let mut s = 0.0;
                    for k in 0..HEAD_DIM {
                        s += Q_BUF[i*D + h*HEAD_DIM + k] * K_BUF[j*D + h*HEAD_DIM + k];
                    }
                    scores[j] = s * inv;
                }
                let mut m = scores[0];
                for j in 1..N { if scores[j] > m { m = scores[j]; } }
                let mut sum = 0.0;
                for j in 0..N { scores[j] = (scores[j] - m).exp(); sum += scores[j]; }
                for j in 0..N { scores[j] /= sum; }
                for j in 0..N {
                    let a = scores[j];
                    for k in 0..HEAD_DIM {
                        ATTN_BUF[i*D + h*HEAD_DIM + k] += a * V_BUF[j*D + h*HEAD_DIM + k];
                    }
                }
            }
        }
        matmul_block(lo, hi, &ATTN_BUF, &W_O, &mut XA_BUF, D, D);
        for i in lo..hi {
            for j in 0..D { X_BUF[i*D + j] += XA_BUF[i*D + j]; }
        }
        let (x_buf_r2, xb_buf_w) = (&X_BUF[..], &mut XB_BUF[..]);
        layer_norm_block(lo, hi, x_buf_r2, &LN_2_W, &LN_2_B, xb_buf_w, D);
        matmul_block(lo, hi, &XB_BUF, &W_1, &mut FFN_H, D, D_FF);
        for i in lo..hi {
            for j in 0..D_FF { if FFN_H[i*D_FF + j] < 0.0 { FFN_H[i*D_FF + j] = 0.0; } }
        }
        matmul_block(lo, hi, &FFN_H, &W_2, &mut XA_BUF, D_FF, D);
        for i in lo..hi {
            for j in 0..D { X_BUF[i*D + j] += XA_BUF[i*D + j]; }
        }
        matmul_block(lo, hi, &X_BUF, &W_U, &mut LOGITS_BUF, D, VOCAB);
        for i in lo..hi {
            let mut best = 0usize; let mut bv = LOGITS_BUF[i*VOCAB];
            for v in 1..VOCAB { let lv = LOGITS_BUF[i*VOCAB + v]; if lv > bv { bv = lv; best = v; } }
            OUTPUT_TOKENS[i] = best as u8;
        }
    });
    Ok(0)
}

#[pyfunction]
fn result(py: Python<'_>) -> PyResult<u64> {
    let cs = py.allow_threads(|| unsafe {
        let mut cs: u64 = 0;
        for i in 0..N {
            cs = (cs + (i as u64 + 1) * (OUTPUT_TOKENS[i] as u64)) & 0xFFFFFFFF_u64;
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

Uses pyDF.DFGraph to wire 2 phases × N_BLOCKS nodes with a barrier.
Each phase node calls into the Rust sucuri_attn module via PyO3.
'''
import argparse, os, sys, time
from pathlib import Path

SUCURI_ROOT = os.environ.get("SUCURI_ROOT")
if SUCURI_ROOT is None:
    sys.stderr.write("ERROR: SUCURI_ROOT not set\n"); sys.exit(2)
sys.path.insert(0, SUCURI_ROOT)
from pyDF import DFGraph, Node, Oper, Scheduler

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rust-so", required=True, help="Path to sucuri_attn.so")
    ap.add_argument("--n-blocks", type=int, required=True)
    ap.add_argument("--workers", type=int, required=True)
    args = ap.parse_args()

    # Load the compiled PyO3 extension module
    import importlib.util
    spec = importlib.util.spec_from_file_location("sucuri_attn", args.rust_so)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)

    # Initialize all weights (single-threaded; only block 0 does it).
    mod.init()

    K = args.n_blocks

    def make_phase_a(b):
        def f(_dummy=0):
            mod.phase_a(b)
            return 0
        return f
    def make_phase_b(b):
        def f(*_args):
            mod.phase_b(b)
            return 0
        return f

    g = DFGraph()
    # Phase A nodes (each takes a dummy seed input)
    seed = Node(lambda: 0, n_input=0, n_output=K)
    g.add(seed)
    phaseA_nodes = []
    for b in range(K):
        n = Node(make_phase_a(b), n_input=1, n_output=1)
        g.add(n); g.connect(seed, n, output_idx=b)
        phaseA_nodes.append(n)
    # Barrier: fan-in K phaseA outputs to a single signal
    def barrier(*args):
        return 0
    barr = Node(barrier, n_input=K, n_output=K)
    g.add(barr)
    for i, n in enumerate(phaseA_nodes):
        g.connect(n, barr, input_idx=i)
    # Phase B nodes
    phaseB_nodes = []
    for b in range(K):
        n = Node(make_phase_b(b), n_input=1, n_output=1)
        g.add(n); g.connect(barr, n, output_idx=b)
        phaseB_nodes.append(n)
    # Result aggregator
    def result(*args):
        return mod.result()
    res = Node(result, n_input=K, n_output=0)
    g.add(res)
    for i, n in enumerate(phaseB_nodes):
        g.connect(n, res, input_idx=i)

    t0 = time.perf_counter()
    sched = Scheduler(g, n_workers=args.workers)
    sched.run()
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
