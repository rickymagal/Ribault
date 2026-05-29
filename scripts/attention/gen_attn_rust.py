#!/usr/bin/env python3
"""Generate Ribault attention files with Rust-implemented supers.

Mirrors gen_attn_c.py but emits a Rust staticlib that build_supers_rust.sh
links with the C wrappers. The dataflow graph (.fl) is IDENTICAL to the C
version — only the super body language changes.

Output: <out-dir>/{attn.fl, attn_rs_supers/{Cargo.toml, src/lib.rs}}.
"""

import argparse, os


SUPER_INIT    = 13
SUPER_PHASE_A = 12
SUPER_BARRIER = 14
SUPER_PHASE_B = 11
SUPER_RESULT  = 10


CARGO_TOML = """[package]
name = "attn_rs_supers"
version = "0.1.0"
edition = "2021"

[dependencies]
libc = "0.2"

[lib]
name = "supers_rust"
crate-type = ["staticlib"]

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
debug = false
panic = "abort"
"""


RS_TEMPLATE = r"""//! Auto-generated: attention Rust supers.

#![allow(non_snake_case)]
#![allow(static_mut_refs)]

use std::os::raw::c_double;
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

static mut E: *mut c_double       = std::ptr::null_mut();
static mut PE: *mut c_double      = std::ptr::null_mut();
static mut W_Q: *mut c_double     = std::ptr::null_mut();
static mut W_K_PTR: *mut c_double = std::ptr::null_mut();
static mut W_V: *mut c_double     = std::ptr::null_mut();
static mut W_O: *mut c_double     = std::ptr::null_mut();
static mut W_1: *mut c_double     = std::ptr::null_mut();
static mut W_2: *mut c_double     = std::ptr::null_mut();
static mut W_U: *mut c_double     = std::ptr::null_mut();
static mut LN_1_W: *mut c_double  = std::ptr::null_mut();
static mut LN_1_B: *mut c_double  = std::ptr::null_mut();
static mut LN_2_W: *mut c_double  = std::ptr::null_mut();
static mut LN_2_B: *mut c_double  = std::ptr::null_mut();

static mut X_BUF: *mut c_double     = std::ptr::null_mut();
static mut XA_BUF: *mut c_double    = std::ptr::null_mut();
static mut Q_BUF: *mut c_double     = std::ptr::null_mut();
static mut K_BUF: *mut c_double     = std::ptr::null_mut();
static mut V_BUF: *mut c_double     = std::ptr::null_mut();
static mut ATTN_BUF: *mut c_double  = std::ptr::null_mut();
static mut XB_BUF: *mut c_double    = std::ptr::null_mut();
static mut FFN_H: *mut c_double     = std::ptr::null_mut();
static mut LOGITS_BUF: *mut c_double= std::ptr::null_mut();
static mut INPUT_TOKENS: *mut u8    = std::ptr::null_mut();
static mut OUTPUT_TOKENS: *mut u8   = std::ptr::null_mut();

unsafe fn xmalloc_d(n: usize) -> *mut c_double {
    let p = libc::malloc(n * std::mem::size_of::<c_double>()) as *mut c_double;
    assert!(!p.is_null(), "malloc failed");
    p
}
unsafe fn xmalloc_u(n: usize) -> *mut u8 {
    let p = libc::malloc(n) as *mut u8;
    assert!(!p.is_null(), "malloc failed");
    p
}

unsafe fn read_bin_d(name: &str, dst: *mut c_double, count: usize) {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count * 8];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    for i in 0..count {
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&buf[i*8..(i+1)*8]);
        *dst.add(i) = f64::from_le_bytes(bytes);
    }
}
unsafe fn read_bin_u8(name: &str, dst: *mut u8, count: usize) {
    let path = format!("{}/{}", DATA_DIR, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let s = std::slice::from_raw_parts_mut(dst, count);
    f.read_exact(s).unwrap_or_else(|e| panic!("read {}: {}", path, e));
}

unsafe fn row_lo(b: usize) -> usize { b * N / N_BLOCKS }
unsafe fn row_hi(b: usize) -> usize { if b == N_BLOCKS - 1 { N } else { (b + 1) * N / N_BLOCKS } }

unsafe fn matmul_block(lo: usize, hi: usize, a: *const c_double, b: *const c_double,
                       c: *mut c_double, k_dim: usize, n_dim: usize) {
    for m in lo..hi {
        let arow = a.add(m * k_dim);
        let crow = c.add(m * n_dim);
        std::ptr::write_bytes(crow, 0, n_dim * 8 / std::mem::size_of::<c_double>());
        for k in 0..k_dim {
            let av = *arow.add(k);
            let brow = b.add(k * n_dim);
            for n in 0..n_dim {
                *crow.add(n) += av * *brow.add(n);
            }
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
        for j in 0..dim { *or_.add(j) = (*xr.add(j) - mean) * inv * *w.add(j) + *b.add(j); }
    }
}

unsafe fn s_init() -> i64 {
    E       = xmalloc_d(VOCAB * D); read_bin_d("E.bin", E, VOCAB * D);
    PE      = xmalloc_d(N * D);     read_bin_d("PE.bin", PE, N * D);
    W_Q     = xmalloc_d(D * D);     read_bin_d("W_Q.bin", W_Q, D * D);
    W_K_PTR = xmalloc_d(D * D);     read_bin_d("W_K.bin", W_K_PTR, D * D);
    W_V     = xmalloc_d(D * D);     read_bin_d("W_V.bin", W_V, D * D);
    W_O     = xmalloc_d(D * D);     read_bin_d("W_O.bin", W_O, D * D);
    W_1     = xmalloc_d(D * D_FF);  read_bin_d("W_1.bin", W_1, D * D_FF);
    W_2     = xmalloc_d(D_FF * D);  read_bin_d("W_2.bin", W_2, D_FF * D);
    W_U     = xmalloc_d(D * VOCAB); read_bin_d("W_U.bin", W_U, D * VOCAB);
    LN_1_W  = xmalloc_d(D);         read_bin_d("LN_1_w.bin", LN_1_W, D);
    LN_1_B  = xmalloc_d(D);         read_bin_d("LN_1_b.bin", LN_1_B, D);
    LN_2_W  = xmalloc_d(D);         read_bin_d("LN_2_w.bin", LN_2_W, D);
    LN_2_B  = xmalloc_d(D);         read_bin_d("LN_2_b.bin", LN_2_B, D);
    INPUT_TOKENS  = xmalloc_u(N); read_bin_u8("input_tokens.bin", INPUT_TOKENS, N);
    OUTPUT_TOKENS = xmalloc_u(N);

    X_BUF      = xmalloc_d(N * D);
    XA_BUF     = xmalloc_d(N * D);
    Q_BUF      = xmalloc_d(N * D);
    K_BUF      = xmalloc_d(N * D);
    V_BUF      = xmalloc_d(N * D);
    ATTN_BUF   = xmalloc_d(N * D);
    XB_BUF     = xmalloc_d(N * D);
    FFN_H      = xmalloc_d(N * D_FF);
    LOGITS_BUF = xmalloc_d(N * VOCAB);
    0
}

unsafe fn s_phaseA(block_idx: i64) -> i64 {
    let b = block_idx as usize;
    let lo = row_lo(b);
    let hi = row_hi(b);
    // embed + pos
    for i in lo..hi {
        let token = *INPUT_TOKENS.add(i) as usize;
        let ei = E.add(token * D);
        let pi = PE.add(i * D);
        let xi = X_BUF.add(i * D);
        for j in 0..D { *xi.add(j) = *ei.add(j) + *pi.add(j); }
    }
    layer_norm_block(lo, hi, X_BUF, LN_1_W, LN_1_B, XA_BUF, D);
    matmul_block(lo, hi, XA_BUF, W_Q,     Q_BUF, D, D);
    matmul_block(lo, hi, XA_BUF, W_K_PTR, K_BUF, D, D);
    matmul_block(lo, hi, XA_BUF, W_V,     V_BUF, D, D);
    0
}

unsafe fn s_phaseB(block_idx: i64) -> i64 {
    let b = block_idx as usize;
    let lo = row_lo(b);
    let hi = row_hi(b);
    let inv = 1.0 / (HEAD_DIM as f64).sqrt();
    let mut scores: Vec<f64> = vec![0.0; N];

    // Zero attn_buf rows
    for i in lo..hi {
        let ai = ATTN_BUF.add(i * D);
        std::ptr::write_bytes(ai, 0, D * 8 / std::mem::size_of::<c_double>());
    }
    for i in lo..hi {
        for h in 0..N_HEADS {
            let qhi = Q_BUF.add(i * D + h * HEAD_DIM);
            for j in 0..N {
                let khj = K_BUF.add(j * D + h * HEAD_DIM);
                let mut s = 0.0;
                for k in 0..HEAD_DIM { s += *qhi.add(k) * *khj.add(k); }
                scores[j] = s * inv;
            }
            let mut m = scores[0];
            for j in 1..N { if scores[j] > m { m = scores[j]; } }
            let mut sum = 0.0;
            for j in 0..N { scores[j] = (scores[j] - m).exp(); sum += scores[j]; }
            for j in 0..N { scores[j] /= sum; }
            let ahi = ATTN_BUF.add(i * D + h * HEAD_DIM);
            for j in 0..N {
                let a = scores[j];
                let vhj = V_BUF.add(j * D + h * HEAD_DIM);
                for k in 0..HEAD_DIM { *ahi.add(k) += a * *vhj.add(k); }
            }
        }
    }
    matmul_block(lo, hi, ATTN_BUF, W_O, XA_BUF, D, D);
    for i in lo..hi {
        let xi = X_BUF.add(i * D);
        let yi = XA_BUF.add(i * D);
        for j in 0..D { *xi.add(j) += *yi.add(j); }
    }
    layer_norm_block(lo, hi, X_BUF, LN_2_W, LN_2_B, XB_BUF, D);
    matmul_block(lo, hi, XB_BUF, W_1, FFN_H, D, D_FF);
    for i in lo..hi {
        let hi_ = FFN_H.add(i * D_FF);
        for j in 0..D_FF { if *hi_.add(j) < 0.0 { *hi_.add(j) = 0.0; } }
    }
    matmul_block(lo, hi, FFN_H, W_2, XA_BUF, D_FF, D);
    for i in lo..hi {
        let xi = X_BUF.add(i * D);
        let yi = XA_BUF.add(i * D);
        for j in 0..D { *xi.add(j) += *yi.add(j); }
    }
    matmul_block(lo, hi, X_BUF, W_U, LOGITS_BUF, D, VOCAB);
    for i in lo..hi {
        let li = LOGITS_BUF.add(i * VOCAB);
        let mut best = 0usize;
        let mut bv = *li;
        for v in 1..VOCAB { let lv = *li.add(v); if lv > bv { bv = lv; best = v; } }
        *OUTPUT_TOKENS.add(i) = best as u8;
    }
    0
}

unsafe fn s_result() -> i64 {
    let mut cs: u64 = 0;
    for i in 0..N {
        cs = (cs + (i as u64 + 1) * (*OUTPUT_TOKENS.add(i) as u64)) & 0xFFFFFFFFu64;
    }
    println!("CHECKSUM={}", cs);
    let path = format!("{}/output_tokens.bin", DATA_DIR);
    if let Ok(mut f) = std::fs::File::create(&path) {
        use std::io::Write;
        let s = std::slice::from_raw_parts(OUTPUT_TOKENS, N);
        let _ = f.write_all(s);
    }
    cs as i64
}

#[no_mangle]
pub unsafe extern "C" fn s__SUPER_INIT__(_in: *const i64, out: *mut i64) {
    *out = s_init();
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_PHASE_A__(in_: *const i64, out: *mut i64) {
    *out = s_phaseA(*in_.add(1));
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_BARRIER__(_in: *const i64, out: *mut i64) {
    *out = 0;
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_PHASE_B__(in_: *const i64, out: *mut i64) {
    *out = s_phaseB(*in_.add(1));
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_RESULT__(_in: *const i64, out: *mut i64) {
    *out = s_result();
}

#[no_mangle] pub extern "C" fn supers_hs_init() {}
#[no_mangle] pub extern "C" fn supers_hs_exit() {}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {}
"""


def emit_chunked_fan_in(final_kind, leaf_names, final_name, intermediate_kind="barrier", branch=16):
    """Same logic as gen_attn_c.py: chunk a K-way fan-in into chunks of
    `branch` to stay within the TALM assembler's 5-bit src-count field.
    Intermediate levels use `intermediate_kind` (no side effects); only the
    final level uses `final_kind`."""
    if len(leaf_names) <= branch:
        return [f"{final_kind} {final_name}, " + ", ".join(leaf_names)], final_name
    lines = []
    intermediates = []
    for ci in range(0, len(leaf_names), branch):
        chunk = leaf_names[ci:ci + branch]
        name = f"{final_name}_lvl0_{ci // branch}"
        lines.append(f"{intermediate_kind} {name}, " + ", ".join(chunk))
        intermediates.append(name)
    if len(intermediates) <= branch:
        lines.append(f"{final_kind} {final_name}, " + ", ".join(intermediates))
        return lines, final_name
    raise ValueError(f"n_blocks={len(leaf_names)} exceeds branch^2={branch*branch}; extend tree")


def emit(out_dir, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
    os.makedirs(out_dir, exist_ok=True)

    # ---- .fl (identical to gen_attn_c.py) ----
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('phaseA',  {SUPER_PHASE_A}, 1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('phaseB',  {SUPER_PHASE_B}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('phaseA', 1000)",
        "avgtime('phaseB', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for b in range(n_blocks):
        fl_lines.append(f"const k_A_{b}, {b}")
        fl_lines.append(f"phaseA a{b}, ini, k_A_{b}")
    barrier_lines, barrier_name = emit_chunked_fan_in(
        "barrier", [f"a{b}" for b in range(n_blocks)], "br",
        intermediate_kind="barrier")
    fl_lines.extend(barrier_lines)
    for b in range(n_blocks):
        fl_lines.append(f"const k_B_{b}, {b}")
        fl_lines.append(f"phaseB b{b}, {barrier_name}, k_B_{b}")
    output_lines, _ = emit_chunked_fan_in(
        "output", [f"b{b}" for b in range(n_blocks)], "out",
        intermediate_kind="barrier")
    fl_lines.extend(output_lines)

    fl_path = os.path.join(out_dir, "attn.fl")
    with open(fl_path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_attn_rs] wrote {fl_path}  (n_blocks={n_blocks})")

    # ---- Rust crate ----
    project_dir = os.path.join(out_dir, "attn_rs_supers")
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
              .replace("__DATA_DIR__", data_dir)
              .replace("__SUPER_INIT__", str(SUPER_INIT))
              .replace("__SUPER_PHASE_A__", str(SUPER_PHASE_A))
              .replace("__SUPER_BARRIER__", str(SUPER_BARRIER))
              .replace("__SUPER_PHASE_B__", str(SUPER_PHASE_B))
              .replace("__SUPER_RESULT__", str(SUPER_RESULT)))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    print(f"[gen_attn_rs] wrote {project_dir}/")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()

    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
