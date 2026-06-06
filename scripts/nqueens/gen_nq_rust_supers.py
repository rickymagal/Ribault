#!/usr/bin/env python3
"""Generate a Rust super-body crate for Ribault N-Queens.

Pairs with the SAME .hss / .fl produced by codegen for the ribault_hs
variant.  Provides strong-symbol overrides for s4 (print), s10
(safeImpl) and s11 (solveRestImpl).  N is baked at compile time.
"""

import argparse, os


CARGO_TOML = """[package]
name = "nq_rs_supers"
version = "0.1.0"
edition = "2021"

[lib]
name = "supers_rust"
crate-type = ["staticlib"]

[profile.release]
opt-level = 3
lto = "fat"
codegen-units = 1
debug = false
panic = "abort"
"""


def emit_rs(N):
    return f'''//! Auto-generated Ribault N-Queens Rust supers (N={N}).
//!
//! Overrides s10 (safeImpl) and s11 (solveRestImpl) with strong Rust
//! symbols (exposed `extern "C"`).  The dataflow graph (.fl) is unchanged
//! -- the recursive TALM tree comes from the same .hss compiled by codegen.
//! Only the leaf compute moves from Haskell to Rust.
//!
//! solveRestImpl semantics mirror nq_seq.rs bit-for-bit: decode the
//! base-N integer path into a stack `[i32; N]`, then recurse with
//! direct array indexing.

#![allow(non_snake_case)]

const NQ_N: usize = {N};

#[inline(always)]
fn nq_decode(path: i64, row: usize, queens: &mut [i32; NQ_N]) {{
    let mut p = path;
    for i in 0..row {{
        queens[i] = (p as i32) % (NQ_N as i32);
        p /= NQ_N as i64;
    }}
}}

#[inline(always)]
fn nq_safe(queens: &[i32; NQ_N], row: usize, col: i32) -> bool {{
    for r in 0..row {{
        let c = queens[r];
        if c == col {{ return false; }}
        if c - r as i32 == col - row as i32 {{ return false; }}
        if c + r as i32 == col + row as i32 {{ return false; }}
    }}
    true
}}

fn nq_solve(queens: &mut [i32; NQ_N], row: usize) -> u64 {{
    if row == NQ_N {{ return 1; }}
    let mut cnt: u64 = 0;
    for c in 0..NQ_N as i32 {{
        if nq_safe(queens, row, c) {{
            queens[row] = c;
            cnt += nq_solve(queens, row + 1);
        }}
    }}
    cnt
}}

fn safe_impl(path: i64, curr_row: i64, col: i64) -> i64 {{
    let mut queens: [i32; NQ_N] = [0; NQ_N];
    nq_decode(path, curr_row as usize, &mut queens);
    if nq_safe(&queens, curr_row as usize, col as i32) {{ 1 }} else {{ 0 }}
}}

fn solve_rest_impl(path: i64, _pow: i64, row: i64) -> i64 {{
    let mut queens: [i32; NQ_N] = [0; NQ_N];
    nq_decode(path, row as usize, &mut queens);
    nq_solve(&mut queens, row as usize) as i64
}}

// Strong-symbol overrides of the supers_wrappers.c weak forward decls.

#[no_mangle]
pub unsafe extern "C" fn s4(in_: *const i64, out: *mut i64) {{
    let v = *in_.add(0);
    println!("{{}}", v);
    *out.add(0) = v;
}}

#[no_mangle]
pub unsafe extern "C" fn s10(in_: *const i64, out: *mut i64) {{
    // in: [path, currRow, col]
    *out.add(0) = safe_impl(*in_.add(0), *in_.add(1), *in_.add(2));
}}

#[no_mangle]
pub unsafe extern "C" fn s11(in_: *const i64, out: *mut i64) {{
    // in: [path, pow, row]
    *out.add(0) = solve_rest_impl(*in_.add(0), *in_.add(1), *in_.add(2));
}}

// Trebuchet keys off the presence of supers_hs_init_thread to switch from
// "mutex-serialized Haskell" to "per-thread direct call" mode.  Rust supers
// need no per-thread init.
#[no_mangle] pub extern "C" fn supers_hs_init() {{}}
#[no_mangle] pub extern "C" fn supers_hs_exit() {{}}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {{}}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {{}}
'''


def emit(out_dir, N):
    project_dir = os.path.join(out_dir, "nq_rs_supers")
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(emit_rs(N))
    print(f"[gen_nq_rust_supers] wrote {project_dir} (N={N})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    args = ap.parse_args()
    emit(args.out_dir, args.N)


if __name__ == "__main__":
    main()
