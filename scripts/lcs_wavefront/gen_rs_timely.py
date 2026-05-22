#!/usr/bin/env python3
"""Generate a Timely Dataflow LCS wavefront benchmark.

Emits a complete Cargo project (Cargo.toml + src/main.rs) parameterized
with N, alphabet, seed, DIM_ROWS, DIM_COLS baked in as Rust consts.

Wavefront uses Timely's epoch-based dataflow: anti-diagonal d is one
epoch; blocks on the same anti-diagonal are processed in parallel by
workers (Exchange distributes by block index). Next anti-diagonal
starts after the probe sees the previous epoch's frontier advance.

Compute backend: shared mutable boundary arrays via raw pointers (Rust
unsafe), wrapped in Send+Sync newtype. Algorithmically race-free because
blocks on the same anti-diagonal have disjoint write regions.
"""

import argparse, os


CARGO_TMPL = """[package]
name = "lcs_wf_timely"
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


RS_TMPL = r"""// Auto-generated: LCS wavefront benchmark (Timely Dataflow, epoch-per-antidiag)
use timely::dataflow::operators::{Input, Map, Probe, Exchange};
use timely::dataflow::{InputHandle, ProbeHandle};
use std::sync::Arc;
use std::time::Instant;

const SEQ_LEN: usize = __N__;
const ALPHA: u64 = __ALPHA__;
const SEED: u64 = __SEED__;
const DIM_ROWS: usize = __DIM_ROWS__;
const DIM_COLS: usize = __DIM_COLS__;

#[derive(Clone, Copy)]
struct PtrWrap(*mut i64);
unsafe impl Send for PtrWrap {}
unsafe impl Sync for PtrWrap {}

#[inline(always)]
fn lcg_next(r: u64) -> u64 {
    6364136223846793005u64
        .wrapping_mul(r)
        .wrapping_add(1442695040888963407u64)
        & 0x7FFFFFFFFFFFFFFFu64
}

fn gen_seq(seed: u64, n: usize, alpha: u64) -> (Vec<i64>, u64) {
    let mut out = vec![0i64; n];
    let mut r = seed;
    for i in 0..n {
        r = lcg_next(r);
        out[i] = ((r >> 33) % alpha) as i64;
    }
    (out, r)
}

fn compute_block(
    sa: &[i64], sb: &[i64],
    hb_ptr: *mut i64, vb_ptr: *mut i64,
    bi: usize, bj: usize,
) {
    let n = SEQ_LEN;
    let cols = n + 1;
    let chunk_r = n / DIM_ROWS;
    let chunk_c = n / DIM_COLS;
    let row_start = bi * chunk_r + 1;
    let row_end = if bi == DIM_ROWS - 1 { n } else { (bi + 1) * chunk_r };
    let col_start = bj * chunk_c + 1;
    let col_end = if bj == DIM_COLS - 1 { n } else { (bj + 1) * chunk_c };
    let local_cols = col_end - col_start + 1;
    let hb_read_base = bi * cols + col_start - 1;
    let hb_write_base = (bi + 1) * cols + col_start - 1;
    let vb_read_base = bj * cols;
    let vb_write_base = (bj + 1) * cols;
    let sb_base = col_start - 2;

    let mut buf1 = vec![0i64; local_cols + 1];
    let mut buf2 = vec![0i64; local_cols + 1];

    unsafe {
        for lj in 0..=local_cols {
            buf1[lj] = *hb_ptr.add(hb_read_base + lj);
        }
    }

    let mut use_buf1_as_prev = true;
    for i in row_start..=row_end {
        let left_val = unsafe { *vb_ptr.add(vb_read_base + i) };
        let ai = sa[i - 1];
        if use_buf1_as_prev {
            buf2[0] = left_val;
            for lj in 1..=local_cols {
                let bj_v = sb[sb_base + lj];
                let new_val = if ai == bj_v {
                    buf1[lj - 1] + 1
                } else {
                    std::cmp::max(buf1[lj], buf2[lj - 1])
                };
                buf2[lj] = new_val;
            }
            unsafe { *vb_ptr.add(vb_write_base + i) = buf2[local_cols]; }
        } else {
            buf1[0] = left_val;
            for lj in 1..=local_cols {
                let bj_v = sb[sb_base + lj];
                let new_val = if ai == bj_v {
                    buf2[lj - 1] + 1
                } else {
                    std::cmp::max(buf2[lj], buf1[lj - 1])
                };
                buf1[lj] = new_val;
            }
            unsafe { *vb_ptr.add(vb_write_base + i) = buf1[local_cols]; }
        }
        use_buf1_as_prev = !use_buf1_as_prev;
    }

    let final_buf: &[i64] = if use_buf1_as_prev { &buf1 } else { &buf2 };
    unsafe {
        for lj in 0..=local_cols {
            *hb_ptr.add(hb_write_base + lj) = final_buf[lj];
        }
    }
}

fn main() {
    // Generate sequences (single-threaded, deterministic from seed).
    let (sa, rng1) = gen_seq(SEED, SEQ_LEN, ALPHA);
    let (sb, _) = gen_seq(rng1, SEQ_LEN, ALPHA);

    let cols = SEQ_LEN + 1;
    let hb_len = (DIM_ROWS + 1) * cols;
    let vb_len = (DIM_COLS + 1) * cols;
    let mut hb = vec![0i64; hb_len];
    let mut vb = vec![0i64; vb_len];

    let hb_wrap = PtrWrap(hb.as_mut_ptr());
    let vb_wrap = PtrWrap(vb.as_mut_ptr());
    let sa_arc = Arc::new(sa);
    let sb_arc = Arc::new(sb);

    let t0 = Instant::now();

    timely::execute_from_args(std::env::args(), move |worker| {
        let sa = sa_arc.clone();
        let sb = sb_arc.clone();
        let hb_ptr = hb_wrap;
        let vb_ptr = vb_wrap;
        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();

        worker.dataflow::<u64, _, _>(|scope| {
            input.to_stream(scope)
                .exchange(|coord: &(usize, usize)| {
                    (coord.0 * DIM_COLS + coord.1) as u64
                })
                .map(move |(i, j)| {
                    compute_block(sa.as_slice(), sb.as_slice(),
                                  hb_ptr.0, vb_ptr.0, i, j);
                    (i, j)
                })
                .probe_with(&mut probe);
        });

        let total_diag = DIM_ROWS + DIM_COLS - 1;
        for d in 0..total_diag {
            if worker.index() == 0 {
                let lo = if d + 1 > DIM_COLS { d + 1 - DIM_COLS } else { 0 };
                let hi = std::cmp::min(DIM_ROWS - 1, d);
                for i in lo..=hi {
                    let j = d - i;
                    input.send((i, j));
                }
            }
            input.advance_to((d + 1) as u64);
            while probe.less_than(input.time()) {
                worker.step();
            }
        }
    }).unwrap();

    let elapsed = t0.elapsed();
    let score = hb[DIM_ROWS * cols + SEQ_LEN];
    println!("RESULT={}", score);
    println!("RUNTIME_SEC={}", elapsed.as_secs_f64());
}
"""


def emit(project_dir, input_dir, dim_rows, dim_cols):
    os.makedirs(project_dir, exist_ok=True)
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])
    rs_src = (RS_TMPL
              .replace("__N__", str(seq_len))
              .replace("__ALPHA__", str(alphabet))
              .replace("__SEED__", str(seed))
              .replace("__DIM_ROWS__", str(dim_rows))
              .replace("__DIM_COLS__", str(dim_cols)))
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TMPL)
    with open(os.path.join(src_dir, "main.rs"), "w") as f:
        f.write(rs_src)
    print(f"[gen_lcs_wf_timely] wrote {project_dir}  (N={seq_len}, {dim_rows}x{dim_cols})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim-rows", type=int, required=True)
    ap.add_argument("--dim-cols", type=int, required=True)
    args = ap.parse_args()
    emit(args.project_dir, args.input_dir, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
