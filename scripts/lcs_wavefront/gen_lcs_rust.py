#!/usr/bin/env python3
"""Generate Ribault LCS files with Rust-implemented supers.

Produces:
  1. lcs_wf.fl       — same dataflow graph as gen_talm_input.py / gen_lcs_c.py
  2. lcs_rs_supers/  — a Cargo project (crate-type = staticlib) whose src/
                       lib.rs implements s10/s11/s12/s13 with constants
                       (LCS_SEQ_LEN, LCS_DIM_ROWS, LCS_DIM_COLS, LCS_SEED,
                       LCS_ALPHA) baked at compile time. Mirrors the C and
                       Haskell super bodies byte-for-byte.

The Rust crate emits `extern "C"` symbols s10/s11/s12/s13 as strong
references that override the WEAK_FN exports in supers_wrappers.c.
build_supers_rust.sh links the resulting `.a` together with
supers_wrappers.c to produce libsupers.so.
"""

import argparse, os


SUPER_INIT    = 13
SUPER_BLOCK1  = 12  # 2 inputs: dep + idx
SUPER_BLOCK2  = 11  # 3 inputs: top + left + idx
SUPER_RESULT  = 10


def emit(out_dir, seq_len, alphabet, seed, dim_rows, dim_cols):
    os.makedirs(out_dir, exist_ok=True)

    # ---- 1. .fl  (identical to gen_lcs_c.py) ----
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('block1', {SUPER_BLOCK1}, 1, False, False)",
        f"superinst('block2', {SUPER_BLOCK2}, 1, False, False)",
        f"superinst('output', {SUPER_RESULT}, 1, False, False)",
        f"avgtime('block1', 10000)",
        f"avgtime('block2', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    def bname(i, j):
        return f"blck{i * dim_cols + j}"

    def kname(i, j):
        return f"k_{i}_{j}"

    fl_lines.append(f"const {kname(0, 0)}, 0")
    fl_lines.append(f"block1 {bname(0, 0)}, ini, {kname(0, 0)}")

    for j in range(1, dim_cols):
        idx = j
        fl_lines.append(f"const {kname(0, j)}, {idx}")
        fl_lines.append(f"block1 {bname(0, j)}, {bname(0, j-1)}, {kname(0, j)}")

    for i in range(1, dim_rows):
        idx = i * dim_cols
        fl_lines.append(f"const {kname(i, 0)}, {idx}")
        fl_lines.append(f"block1 {bname(i, 0)}, {bname(i-1, 0)}, {kname(i, 0)}")

    for i in range(1, dim_rows):
        for j in range(1, dim_cols):
            idx = i * dim_cols + j
            fl_lines.append(f"const {kname(i, j)}, {idx}")
            fl_lines.append(
                f"block2 {bname(i, j)}, {bname(i-1, j)}, {bname(i, j-1)}, {kname(i, j)}"
            )

    fl_lines.append(f"output out, {bname(dim_rows-1, dim_cols-1)}")

    fl_path = os.path.join(out_dir, "lcs_wf.fl")
    with open(fl_path, "w", encoding="utf-8") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_lcs_rs] wrote {fl_path}  ({dim_rows*dim_cols} blocks)")

    # ---- 2. Rust Cargo project ----
    project_dir = os.path.join(out_dir, "lcs_rs_supers")
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)

    cargo_toml = """[package]
name = "lcs_rs_supers"
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
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(cargo_toml)

    rs_src = f"""//! Auto-generated: LCS wavefront supers (Rust implementation).
//! Mirrors gen_lcs_c.py byte-for-byte: same LCG, same boundary arrays,
//! same inner DP loop. Exposed as `extern "C"` symbols s10/s11/s12/s13
//! so the C wrappers in supers_wrappers.c call directly into Rust.

#![allow(non_snake_case)]
#![allow(static_mut_refs)]

use std::os::raw::c_int;

const LCS_SEQ_LEN: usize = {seq_len};
const LCS_ALPHA: u64 = {alphabet};
const LCS_SEED: u64 = {seed};
const LCS_DIM_ROWS: usize = {dim_rows};
const LCS_DIM_COLS: usize = {dim_cols};

// Shared state, accessed via raw pointers from multiple Trebuchet worker
// threads. The dataflow firing rule guarantees the wavefront-style access
// pattern: blocks at the same anti-diagonal write to disjoint regions
// (different bi+1 row of g_h_bound, different bj+1 column of g_v_bound).
// Same algorithmic safety argument as gen_lcs_c.py and gen_rs_timely.py.
static mut G_SA: *mut c_int = std::ptr::null_mut();
static mut G_SB: *mut c_int = std::ptr::null_mut();
static mut G_HBOUND: *mut c_int = std::ptr::null_mut();
static mut G_VBOUND: *mut c_int = std::ptr::null_mut();

#[inline(always)]
fn lcs_next_rng(r: u64) -> u64 {{
    (6364136223846793005u64
        .wrapping_mul(r)
        .wrapping_add(1442695040888963407u64))
        & 0x7FFFFFFFFFFFFFFFu64
}}

unsafe fn lcs_gen_seq(rng0: u64, arr: *mut c_int, length: usize, alpha: u64) -> u64 {{
    let mut r = rng0;
    for i in 0..length {{
        r = lcs_next_rng(r);
        *arr.add(i) = ((r >> 33) % alpha) as c_int;
    }}
    r
}}

unsafe fn calloc_ints(count: usize) -> *mut c_int {{
    let bytes = count.checked_mul(std::mem::size_of::<c_int>()).expect("size overflow");
    let p = libc::calloc(count, std::mem::size_of::<c_int>()) as *mut c_int;
    if p.is_null() {{
        panic!("calloc failed for {{}} ints ({{}} bytes)", count, bytes);
    }}
    p
}}

unsafe fn malloc_ints(count: usize) -> *mut c_int {{
    let p = libc::malloc(count * std::mem::size_of::<c_int>()) as *mut c_int;
    if p.is_null() {{
        panic!("malloc failed for {{}} ints", count);
    }}
    p
}}

unsafe fn lcs_init() {{
    let n = LCS_SEQ_LEN;
    let cols = n + 1;
    G_SA = malloc_ints(n);
    G_SB = malloc_ints(n);
    G_HBOUND = calloc_ints((LCS_DIM_ROWS + 1) * cols);
    G_VBOUND = calloc_ints((LCS_DIM_COLS + 1) * cols);
    let mut rng: u64 = LCS_SEED;
    rng = lcs_gen_seq(rng, G_SA, n, LCS_ALPHA);
    lcs_gen_seq(rng, G_SB, n, LCS_ALPHA);
}}

unsafe fn lcs_block(block_idx: i64) {{
    let bi = (block_idx as usize) / LCS_DIM_COLS;
    let bj = (block_idx as usize) % LCS_DIM_COLS;
    let n = LCS_SEQ_LEN;
    let cols = n + 1;
    let chunk_r = n / LCS_DIM_ROWS;
    let chunk_c = n / LCS_DIM_COLS;
    let row_start = bi * chunk_r + 1;
    let row_end = if bi == LCS_DIM_ROWS - 1 {{ n }} else {{ (bi + 1) * chunk_r }};
    let col_start = bj * chunk_c + 1;
    let col_end = if bj == LCS_DIM_COLS - 1 {{ n }} else {{ (bj + 1) * chunk_c }};
    let local_cols = col_end - col_start + 1;
    let hb_read_base = bi * cols + col_start - 1;
    let hb_write_base = (bi + 1) * cols + col_start - 1;
    let vb_read_base = bj * cols;
    let vb_write_base = (bj + 1) * cols;
    let sb_base = col_start - 2;

    let sa = G_SA;
    let sb = G_SB;
    let hb = G_HBOUND;
    let vb = G_VBOUND;

    let mut buf1: Vec<c_int> = vec![0; local_cols + 1];
    let mut buf2: Vec<c_int> = vec![0; local_cols + 1];

    for lj in 0..=local_cols {{
        buf1[lj] = *hb.add(hb_read_base + lj);
    }}

    let mut use_buf1_as_prev = true;
    for i in row_start..=row_end {{
        let left_val = *vb.add(vb_read_base + i);
        let ai = *sa.add(i - 1);
        if use_buf1_as_prev {{
            buf2[0] = left_val;
            for lj in 1..=local_cols {{
                let bj_v = *sb.add(sb_base + lj);
                let new_val = if ai == bj_v {{
                    buf1[lj - 1] + 1
                }} else {{
                    std::cmp::max(buf1[lj], buf2[lj - 1])
                }};
                buf2[lj] = new_val;
            }}
            *vb.add(vb_write_base + i) = buf2[local_cols];
        }} else {{
            buf1[0] = left_val;
            for lj in 1..=local_cols {{
                let bj_v = *sb.add(sb_base + lj);
                let new_val = if ai == bj_v {{
                    buf2[lj - 1] + 1
                }} else {{
                    std::cmp::max(buf2[lj], buf1[lj - 1])
                }};
                buf1[lj] = new_val;
            }}
            *vb.add(vb_write_base + i) = buf1[local_cols];
        }}
        use_buf1_as_prev = !use_buf1_as_prev;
    }}

    let final_buf: &[c_int] = if use_buf1_as_prev {{ &buf1 }} else {{ &buf2 }};
    for lj in 0..=local_cols {{
        *hb.add(hb_write_base + lj) = final_buf[lj];
    }}
}}

unsafe fn lcs_result() {{
    let score = *G_HBOUND.add(LCS_DIM_ROWS * (LCS_SEQ_LEN + 1) + LCS_SEQ_LEN);
    println!("RESULT={{}}", score);
}}

// Strong-symbol overrides for the WEAK_FN slots in supers_wrappers.c.

#[no_mangle]
pub unsafe extern "C" fn s{SUPER_INIT}(_in: *const i64, out: *mut i64) {{
    lcs_init();
    *out.add(0) = 0;
}}

#[no_mangle]
pub unsafe extern "C" fn s{SUPER_BLOCK1}(in_: *const i64, out: *mut i64) {{
    // in[0] = dep (unused), in[1] = blockIdx
    lcs_block(*in_.add(1));
    *out.add(0) = 0;
}}

#[no_mangle]
pub unsafe extern "C" fn s{SUPER_BLOCK2}(in_: *const i64, out: *mut i64) {{
    // in[0] = top (unused), in[1] = left (unused), in[2] = blockIdx
    lcs_block(*in_.add(2));
    *out.add(0) = 0;
}}

#[no_mangle]
pub unsafe extern "C" fn s{SUPER_RESULT}(_in: *const i64, out: *mut i64) {{
    lcs_result();
    *out.add(0) = 0;
}}

// Stub exports that flip the Trebuchet interpreter from "central supers
// worker" (mutex-serialized, used for Haskell) to "per-thread direct call"
// mode. Rust supers — like C supers — need no per-thread init.
#[no_mangle]
pub extern "C" fn supers_hs_init() {{}}
#[no_mangle]
pub extern "C" fn supers_hs_exit() {{}}
#[no_mangle]
pub extern "C" fn supers_hs_init_thread() {{}}
#[no_mangle]
pub extern "C" fn supers_hs_thread_done() {{}}
"""
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    print(f"[gen_lcs_rs] wrote {project_dir}")

    # We need libc for malloc/calloc. Add it to Cargo.toml as a dep.
    cargo_toml_with_dep = cargo_toml.replace(
        "[lib]",
        "[dependencies]\nlibc = \"0.2\"\n\n[lib]",
    )
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(cargo_toml_with_dep)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--input-dir", required=True,
                    help="Directory containing params.txt (N alpha seed)")
    ap.add_argument("--dim-rows", type=int, required=True)
    ap.add_argument("--dim-cols", type=int, required=True)
    args = ap.parse_args()

    with open(os.path.join(args.input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len, alphabet, seed = int(parts[0]), int(parts[1]), int(parts[2])

    emit(args.out_dir, seq_len, alphabet, seed, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
