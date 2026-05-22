#!/usr/bin/env python3
"""Generate Timely Dataflow Rust crate for full transformer-block attention.

Two-phase epoch: epoch 0 = phase A (per-block embed/LN_1/Q/K/V),
epoch 1 = phase B (per-block MHSA/W_O/LN_2/FFN/W_U/argmax). Worker 0
feeds block-coord tuples into an InputHandle; an Exchange operator
shards by block index across workers; a map runs the per-block compute;
a probe barriers between epochs.

Compute body mirrors gen_attn_rust.py (Ribault-Rust). Shared mutable
state via raw *mut f64 wrapped in Send+Sync newtype; algorithmically
race-free because each block writes disjoint rows in phase A and reads
all rows in phase B (after the inter-epoch frontier advance).
"""

import argparse, os


CARGO_TOML = """[package]
name = "attn_timely"
version = "0.1.0"
edition = "2021"

[dependencies]
timely = "0.13"

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
debug = false
"""


RS_TEMPLATE = r"""// Auto-generated: Timely Dataflow attention forward (2-phase epoch).

use timely::dataflow::operators::{Input, Map, Probe, Exchange};
use timely::dataflow::{InputHandle, ProbeHandle};
use std::sync::Arc;
use std::time::Instant;
use std::fs::File;
use std::io::Read;

const N: usize       = __N__;
const D: usize       = __D__;
const N_HEADS: usize = __N_HEADS__;
const HEAD_DIM: usize= __HEAD_DIM__;
const D_FF: usize    = __D_FF__;
const VOCAB: usize   = __VOCAB__;
const N_BLOCKS: usize= __N_BLOCKS__;
const DATA_DIR: &str = "__DATA_DIR__";

// All pointers stored as usize and converted back in unsafe blocks.
// usize is trivially Send + Sync.
#[derive(Clone, Copy)]
struct PtrSet {
    e: usize, pe: usize,
    wq: usize, wk: usize, wv: usize, wo: usize,
    w1: usize, w2: usize, wu: usize,
    ln1w: usize, ln1b: usize, ln2w: usize, ln2b: usize,
    it: usize, ot: usize,
    x: usize, xa: usize, q: usize, k: usize, v: usize,
    attn: usize, xb: usize, ffn: usize, lg: usize,
}

fn read_doubles(name: &str, count: usize) -> Vec<f64> {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count * 8];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
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
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    buf
}

#[inline]
fn row_lo(b: usize) -> usize { b * N / N_BLOCKS }
#[inline]
fn row_hi(b: usize) -> usize { if b == N_BLOCKS - 1 { N } else { (b + 1) * N / N_BLOCKS } }

unsafe fn matmul_block(lo: usize, hi: usize, a: *const f64, b: *const f64,
                       c: *mut f64, k_dim: usize, n_dim: usize) {
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

unsafe fn layer_norm_block(lo: usize, hi: usize, x: *const f64, w: *const f64,
                            b: *const f64, out: *mut f64, dim: usize) {
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
        for j in 0..dim { *or_.add(j) = (*xr.add(j) - mean) * inv * *w.add(j) + *b.add(j); }
    }
}

unsafe fn run_phase_a(
    b: usize, input_tokens: *const u8,
    e: *const f64, pe: *const f64,
    ln1w: *const f64, ln1b: *const f64,
    w_q: *const f64, w_k: *const f64, w_v: *const f64,
    x: *mut f64, xa: *mut f64,
    q: *mut f64, k_buf: *mut f64, v: *mut f64,
) {
    let lo = row_lo(b); let hi = row_hi(b);
    for i in lo..hi {
        let tok = *input_tokens.add(i) as usize;
        let ei = e.add(tok * D);
        let pi = pe.add(i * D);
        let xi = x.add(i * D);
        for j in 0..D { *xi.add(j) = *ei.add(j) + *pi.add(j); }
    }
    layer_norm_block(lo, hi, x, ln1w, ln1b, xa, D);
    matmul_block(lo, hi, xa, w_q, q, D, D);
    matmul_block(lo, hi, xa, w_k, k_buf, D, D);
    matmul_block(lo, hi, xa, w_v, v, D, D);
}

unsafe fn run_phase_b(
    b: usize,
    q: *const f64, k_buf: *const f64, v: *const f64,
    attn: *mut f64, x: *mut f64, xa: *mut f64, xb: *mut f64,
    w_o: *const f64, ln2w: *const f64, ln2b: *const f64,
    w_1: *const f64, w_2: *const f64, w_u: *const f64,
    ffn_h: *mut f64, logits: *mut f64, output_tokens: *mut u8,
) {
    let lo = row_lo(b); let hi = row_hi(b);
    let inv = 1.0 / (HEAD_DIM as f64).sqrt();
    let mut scores = vec![0.0_f64; N];
    for i in lo..hi {
        let ai = attn.add(i * D);
        for j in 0..D { *ai.add(j) = 0.0; }
    }
    for i in lo..hi {
        for h in 0..N_HEADS {
            let qhi = q.add(i * D + h * HEAD_DIM);
            for j in 0..N {
                let khj = k_buf.add(j * D + h * HEAD_DIM);
                let mut s = 0.0;
                for k in 0..HEAD_DIM { s += *qhi.add(k) * *khj.add(k); }
                scores[j] = s * inv;
            }
            let mut m = scores[0];
            for j in 1..N { if scores[j] > m { m = scores[j]; } }
            let mut sum = 0.0;
            for j in 0..N { scores[j] = (scores[j] - m).exp(); sum += scores[j]; }
            for j in 0..N { scores[j] /= sum; }
            let ahi = attn.add(i * D + h * HEAD_DIM);
            for j in 0..N {
                let a = scores[j];
                let vhj = v.add(j * D + h * HEAD_DIM);
                for k in 0..HEAD_DIM { *ahi.add(k) += a * *vhj.add(k); }
            }
        }
    }
    matmul_block(lo, hi, attn, w_o, xa, D, D);
    for i in lo..hi {
        let xi = x.add(i * D); let yi = xa.add(i * D);
        for j in 0..D { *xi.add(j) += *yi.add(j); }
    }
    layer_norm_block(lo, hi, x, ln2w, ln2b, xb, D);
    matmul_block(lo, hi, xb, w_1, ffn_h, D, D_FF);
    for i in lo..hi {
        let hi_ = ffn_h.add(i * D_FF);
        for j in 0..D_FF { if *hi_.add(j) < 0.0 { *hi_.add(j) = 0.0; } }
    }
    matmul_block(lo, hi, ffn_h, w_2, xa, D_FF, D);
    for i in lo..hi {
        let xi = x.add(i * D); let yi = xa.add(i * D);
        for j in 0..D { *xi.add(j) += *yi.add(j); }
    }
    matmul_block(lo, hi, x, w_u, logits, D, VOCAB);
    for i in lo..hi {
        let li = logits.add(i * VOCAB);
        let mut best = 0usize; let mut bv = *li;
        for v in 1..VOCAB { let lv = *li.add(v); if lv > bv { bv = lv; best = v; } }
        *output_tokens.add(i) = best as u8;
    }
}

fn main() {
    // Load weights and inputs once in main thread; share via raw pointers.
    let e        = read_doubles("E.bin",      VOCAB * D);
    let pe       = read_doubles("PE.bin",     N * D);
    let w_q      = read_doubles("W_Q.bin",    D * D);
    let w_k      = read_doubles("W_K.bin",    D * D);
    let w_v      = read_doubles("W_V.bin",    D * D);
    let w_o      = read_doubles("W_O.bin",    D * D);
    let w_1      = read_doubles("W_1.bin",    D * D_FF);
    let w_2      = read_doubles("W_2.bin",    D_FF * D);
    let w_u      = read_doubles("W_U.bin",    D * VOCAB);
    let ln1w     = read_doubles("LN_1_w.bin", D);
    let ln1b     = read_doubles("LN_1_b.bin", D);
    let ln2w     = read_doubles("LN_2_w.bin", D);
    let ln2b     = read_doubles("LN_2_b.bin", D);
    let input_tokens  = read_bytes("input_tokens.bin", N);

    let mut x_buf      = vec![0.0_f64; N * D];
    let mut xa_buf     = vec![0.0_f64; N * D];
    let mut q_buf      = vec![0.0_f64; N * D];
    let mut k_vec      = vec![0.0_f64; N * D];
    let mut v_buf      = vec![0.0_f64; N * D];
    let mut attn_buf   = vec![0.0_f64; N * D];
    let mut xb_buf     = vec![0.0_f64; N * D];
    let mut ffn_h      = vec![0.0_f64; N * D_FF];
    let mut logits_buf = vec![0.0_f64; N * VOCAB];
    let mut output_tokens = vec![0_u8; N];

    let ptrs = PtrSet {
        e: e.as_ptr() as usize, pe: pe.as_ptr() as usize,
        wq: w_q.as_ptr() as usize, wk: w_k.as_ptr() as usize, wv: w_v.as_ptr() as usize,
        wo: w_o.as_ptr() as usize, w1: w_1.as_ptr() as usize, w2: w_2.as_ptr() as usize,
        wu: w_u.as_ptr() as usize,
        ln1w: ln1w.as_ptr() as usize, ln1b: ln1b.as_ptr() as usize,
        ln2w: ln2w.as_ptr() as usize, ln2b: ln2b.as_ptr() as usize,
        it: input_tokens.as_ptr() as usize, ot: output_tokens.as_mut_ptr() as usize,
        x: x_buf.as_mut_ptr() as usize, xa: xa_buf.as_mut_ptr() as usize,
        q: q_buf.as_mut_ptr() as usize, k: k_vec.as_mut_ptr() as usize,
        v: v_buf.as_mut_ptr() as usize, attn: attn_buf.as_mut_ptr() as usize,
        xb: xb_buf.as_mut_ptr() as usize, ffn: ffn_h.as_mut_ptr() as usize,
        lg: logits_buf.as_mut_ptr() as usize,
    };

    let t0 = Instant::now();

    timely::execute_from_args(std::env::args(), move |worker| {
        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();

        worker.dataflow::<u64, _, _>(|scope| {
            input.to_stream(scope)
                .exchange(|(_phase, b): &(u32, u32)| *b as u64)
                .map(move |(phase, b)| {
                    let b = b as usize;
                    unsafe {
                        if phase == 0 {
                            run_phase_a(b,
                                ptrs.it as *const u8,
                                ptrs.e as *const f64, ptrs.pe as *const f64,
                                ptrs.ln1w as *const f64, ptrs.ln1b as *const f64,
                                ptrs.wq as *const f64, ptrs.wk as *const f64, ptrs.wv as *const f64,
                                ptrs.x as *mut f64, ptrs.xa as *mut f64,
                                ptrs.q as *mut f64, ptrs.k as *mut f64, ptrs.v as *mut f64);
                        } else {
                            run_phase_b(b,
                                ptrs.q as *const f64, ptrs.k as *const f64, ptrs.v as *const f64,
                                ptrs.attn as *mut f64, ptrs.x as *mut f64,
                                ptrs.xa as *mut f64, ptrs.xb as *mut f64,
                                ptrs.wo as *const f64, ptrs.ln2w as *const f64, ptrs.ln2b as *const f64,
                                ptrs.w1 as *const f64, ptrs.w2 as *const f64, ptrs.wu as *const f64,
                                ptrs.ffn as *mut f64, ptrs.lg as *mut f64, ptrs.ot as *mut u8);
                        }
                    }
                    (phase, b as u32)
                })
                .probe_with(&mut probe);
        });

        // Epoch 0: phase A
        if worker.index() == 0 {
            for b in 0..N_BLOCKS as u32 { input.send((0, b)); }
        }
        input.advance_to(1);
        while probe.less_than(input.time()) { worker.step(); }

        // Epoch 1: phase B
        if worker.index() == 0 {
            for b in 0..N_BLOCKS as u32 { input.send((1, b)); }
        }
        input.advance_to(2);
        while probe.less_than(input.time()) { worker.step(); }
    }).unwrap();

    let elapsed = t0.elapsed();

    // Checksum
    let mut cs: u64 = 0;
    for i in 0..N {
        cs = (cs + (i as u64 + 1) * (output_tokens[i] as u64)) & 0xFFFFFFFF_u64;
    }
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", elapsed.as_secs_f64());
}
"""


def emit(project_dir, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
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
    with open(os.path.join(src_dir, "main.rs"), "w") as f:
        f.write(rs_src)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.project_dir, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
