#!/usr/bin/env python3
"""Generate Ribault mergesort files with Rust-implemented supers.

Mirrors gen_ms_c.py exactly in dataflow shape (same .fl, same tree
topology in tree.bin); only the super body language differs. The Rust
super body is built as a staticlib via tools/build_supers_rust.sh and
linked with the C wrappers.
"""

import argparse, os, struct


SUPER_INIT    = 13
SUPER_LEAF    = 12
SUPER_BARRIER = 14
SUPER_MERGE   = 11
SUPER_RESULT  = 10


def build_tree(N, cutoff):
    leaves = []; merges = []
    def recurse(lo, hi):
        if hi - lo <= cutoff:
            i = len(leaves); leaves.append((lo, hi)); return ("leaf", i)
        mid = lo + (hi - lo) // 2
        left = recurse(lo, mid); right = recurse(mid, hi)
        i = len(merges); merges.append((lo, mid, hi, left, right))
        return ("merge", i)
    root = recurse(0, N)
    return leaves, merges, root


def write_tree_bin(out_dir, leaves, merges):
    path = os.path.join(out_dir, "tree.bin")
    with open(path, "wb") as f:
        f.write(struct.pack("<2i", len(leaves), len(merges)))
        for (lo, hi) in leaves:
            f.write(struct.pack("<2i", lo, hi))
        for (lo, mid, hi, _l, _r) in merges:
            f.write(struct.pack("<3i", lo, mid, hi))


CARGO_TOML = """[package]
name = "ms_rs_supers"
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


RS_TEMPLATE = r"""//! Auto-generated: Ribault mergesort Rust supers.

#![allow(non_snake_case)]
#![allow(static_mut_refs)]

use std::os::raw::c_int;
use std::fs::File;
use std::io::Read;

const N: usize        = __N__;
const CUTOFF: usize   = __CUTOFF__;
const N_LEAVES: usize = __N_LEAVES__;
const N_MERGES: usize = __N_MERGES__;
const DATA_DIR: &str  = "__DATA_DIR__";

static mut ARR: *mut c_int = std::ptr::null_mut();
static mut TMP: *mut c_int = std::ptr::null_mut();
static mut LEAF_LO:  *mut c_int = std::ptr::null_mut();
static mut LEAF_HI:  *mut c_int = std::ptr::null_mut();
static mut MERGE_LO: *mut c_int = std::ptr::null_mut();
static mut MERGE_MID:*mut c_int = std::ptr::null_mut();
static mut MERGE_HI: *mut c_int = std::ptr::null_mut();

unsafe fn xmalloc_i32(n: usize) -> *mut c_int {
    let p = libc::malloc(n * std::mem::size_of::<c_int>()) as *mut c_int;
    assert!(!p.is_null(), "malloc failed");
    p
}

unsafe fn load_tree() {
    let path = format!("{}/tree.bin", DATA_DIR);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut hdr = [0u8; 8];
    f.read_exact(&mut hdr).unwrap();
    let nl = i32::from_le_bytes([hdr[0],hdr[1],hdr[2],hdr[3]]) as usize;
    let nm = i32::from_le_bytes([hdr[4],hdr[5],hdr[6],hdr[7]]) as usize;
    assert_eq!(nl, N_LEAVES, "tree.bin leaves mismatch");
    assert_eq!(nm, N_MERGES, "tree.bin merges mismatch");

    LEAF_LO  = xmalloc_i32(N_LEAVES);
    LEAF_HI  = xmalloc_i32(N_LEAVES);
    MERGE_LO = xmalloc_i32(N_MERGES);
    MERGE_MID= xmalloc_i32(N_MERGES);
    MERGE_HI = xmalloc_i32(N_MERGES);

    let mut buf = [0u8; 8];
    for i in 0..N_LEAVES {
        f.read_exact(&mut buf).unwrap();
        *LEAF_LO.add(i) = i32::from_le_bytes([buf[0],buf[1],buf[2],buf[3]]);
        *LEAF_HI.add(i) = i32::from_le_bytes([buf[4],buf[5],buf[6],buf[7]]);
    }
    let mut buf16 = [0u8; 16];  // lo, mid, hi, level — level for STRAT/parpseq only
    for i in 0..N_MERGES {
        f.read_exact(&mut buf16).unwrap();
        *MERGE_LO.add(i)  = i32::from_le_bytes([buf16[0],buf16[1],buf16[2],buf16[3]]);
        *MERGE_MID.add(i) = i32::from_le_bytes([buf16[4],buf16[5],buf16[6],buf16[7]]);
        *MERGE_HI.add(i)  = i32::from_le_bytes([buf16[8],buf16[9],buf16[10],buf16[11]]);
        // buf16[12..16] = level, ignored (Ribault firing rule handles deps)
    }
}

unsafe fn s_init() -> i64 {
    ARR = xmalloc_i32(N);
    TMP = xmalloc_i32(N);
    let path = format!("{}/input.bin", DATA_DIR);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; N * 4];
    f.read_exact(&mut buf).unwrap();
    for i in 0..N {
        *ARR.add(i) = i32::from_le_bytes([buf[i*4],buf[i*4+1],buf[i*4+2],buf[i*4+3]]);
    }
    load_tree();
    0
}

#[inline(always)]
unsafe fn insertion_sort(a: *mut c_int, lo: usize, hi: usize) {
    let mut i = lo + 1;
    while i < hi {
        let x = *a.add(i);
        let mut j = i;
        while j > lo && *a.add(j - 1) > x {
            *a.add(j) = *a.add(j - 1);
            j -= 1;
        }
        *a.add(j) = x;
        i += 1;
    }
}

#[inline(always)]
unsafe fn merge_op(lo: usize, mid: usize, hi: usize) {
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

unsafe fn s_leaf(leaf_idx: i64) -> i64 {
    let idx = leaf_idx as usize;
    let lo = *LEAF_LO.add(idx) as usize;
    let hi = *LEAF_HI.add(idx) as usize;
    insertion_sort(ARR, lo, hi);
    0
}

unsafe fn s_merge(merge_idx: i64) -> i64 {
    let idx = merge_idx as usize;
    let lo  = *MERGE_LO.add(idx) as usize;
    let mid = *MERGE_MID.add(idx) as usize;
    let hi  = *MERGE_HI.add(idx) as usize;
    merge_op(lo, mid, hi);
    0
}

unsafe fn s_output() -> i64 {
    let mut cs: u64 = 0;
    let mut ok = true;
    for i in 0..N {
        if i > 0 && *ARR.add(i) < *ARR.add(i - 1) { ok = false; }
        cs = (cs + (*ARR.add(i) as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    if !ok { eprintln!("WARN: array not sorted"); }
    println!("CHECKSUM={}", cs);
    cs as i64
}

#[no_mangle]
pub unsafe extern "C" fn s__SUPER_INIT__(_in: *const i64, out: *mut i64) {
    *out = s_init();
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_LEAF__(in_: *const i64, out: *mut i64) {
    *out = s_leaf(*in_.add(1));
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_MERGE__(in_: *const i64, out: *mut i64) {
    *out = s_merge(*in_.add(2));
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_BARRIER__(_in: *const i64, out: *mut i64) {
    *out = 0;
}
#[no_mangle]
pub unsafe extern "C" fn s__SUPER_RESULT__(_in: *const i64, out: *mut i64) {
    *out = s_output();
}

#[no_mangle] pub extern "C" fn supers_hs_init() {}
#[no_mangle] pub extern "C" fn supers_hs_exit() {}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {}
"""


def node_var(kind, idx):
    return f"l_{idx}" if kind == "leaf" else f"m_{idx}"


def emit_fl(out_dir, leaves, merges, root):
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('leaf',    {SUPER_LEAF},    1, False, False)",
        f"superinst('merge',   {SUPER_MERGE},   1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('leaf', 100)",
        "avgtime('merge', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for i in range(len(leaves)):
        fl_lines.append(f"const lk_{i}, {i}")
        fl_lines.append(f"leaf l_{i}, ini, lk_{i}")
    for i, (lo, mid, hi, left, right) in enumerate(merges):
        fl_lines.append(f"const mk_{i}, {i}")
        fl_lines.append(f"merge m_{i}, {node_var(*left)}, {node_var(*right)}, mk_{i}")
    fl_lines.append(f"output out, {node_var(*root)}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, N, cutoff):
    os.makedirs(out_dir, exist_ok=True)
    leaves, merges, root = build_tree(N, cutoff)
    # tree.bin is owned by gen_input.py
    emit_fl(out_dir, leaves, merges, root)
    print(f"[gen_ms_rust] wrote {out_dir}/attn.fl  (leaves={len(leaves)} merges={len(merges)})")

    project_dir = os.path.join(out_dir, "ms_rs_supers")
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs_src = (RS_TEMPLATE
              .replace("__N__", str(N))
              .replace("__CUTOFF__", str(cutoff))
              .replace("__N_LEAVES__", str(len(leaves)))
              .replace("__N_MERGES__", str(len(merges)))
              .replace("__DATA_DIR__", data_dir)
              .replace("__SUPER_INIT__",    str(SUPER_INIT))
              .replace("__SUPER_LEAF__",    str(SUPER_LEAF))
              .replace("__SUPER_MERGE__",   str(SUPER_MERGE))
              .replace("__SUPER_BARRIER__", str(SUPER_BARRIER))
              .replace("__SUPER_RESULT__",  str(SUPER_RESULT)))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs_src)
    print(f"[gen_ms_rust] wrote {project_dir}/")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["N"], cfg["CUTOFF"])


if __name__ == "__main__":
    main()
