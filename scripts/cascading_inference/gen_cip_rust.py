#!/usr/bin/env python3
"""Generate Ribault Cascading Inference Pipeline files with Rust-implemented supers.

Mirror of gen_cip_c.py with Rust super bodies (staticlib via libc::malloc +
raw pointers).  Same .fl topology and super numbering.
"""

import argparse, os, struct


SUPER_INIT   = 10
SUPER_STAGE1 = 11
SUPER_STAGE2 = 12
SUPER_STAGE3 = 13
SUPER_STAGE4 = 14
SUPER_SYNC   = 15
SUPER_OUTPUT = 16
MAX_FANIN    = 31


RS_CARGO_TOML = """[package]
name = "cip_rs_supers"
version = "0.1.0"
edition = "2021"

[lib]
name = "cip_rs_supers"
crate-type = ["staticlib"]
path = "lib.rs"

[dependencies]
libc = "0.2"

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
debug = false
"""


RS_TEMPLATE = r"""// Auto-generated: Ribault Cascading Inference Pipeline Rust supers.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use std::fs::File;
use std::io::Read;
use std::os::raw::c_int;

const DIM_D:    usize = 256;
const B2_SLOTS: usize = 256;
const E_DIM:    usize = 64;
const K3:       usize = 8;
const H_DIM:    usize = 128;
const C_CLS:    usize = 16;
const K1:       usize = 1024;

const ACCEPT_S1:      i32 = 1;
const REJECT_S2:      i32 = 2;
const ACCEPT_S3_BASE: i32 = 0x40;
const CLASS_BASE:     i32 = 0x80;

const N:          usize = __N__;
const CHUNK_SIZE: usize = __CHUNK_SIZE__;
const DATA_DIR:   &str  = "__DATA_DIR__";

static mut ITEMS:        *mut u8     = std::ptr::null_mut();
static mut DECISIONS:    *mut i32    = std::ptr::null_mut();
static mut EMB_ALL:      *mut f64    = std::ptr::null_mut();
static mut ACCEPT_TABLE: *mut u32    = std::ptr::null_mut();
static mut REJECT_W:     *mut i16    = std::ptr::null_mut();
static mut REF_VEC:      *mut f64    = std::ptr::null_mut();
static mut W1_MAT:       *mut f64    = std::ptr::null_mut();
static mut B1_VEC:       *mut f64    = std::ptr::null_mut();
static mut W2_MAT:       *mut f64    = std::ptr::null_mut();
static mut B2_VEC:       *mut f64    = std::ptr::null_mut();
static mut COS_TABLE:    *mut f64    = std::ptr::null_mut();
static mut T2_CFG:       i32         = 0;
static mut T3_CFG:       f64         = 0.0;

unsafe fn xmalloc<T>(n: usize) -> *mut T {
    let p = libc::malloc(n * std::mem::size_of::<T>()) as *mut T;
    assert!(!p.is_null());
    p
}

unsafe fn load_config() {
    let text = std::fs::read_to_string(format!("{}/config.txt", DATA_DIR)).unwrap();
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let k = it.next().unwrap_or("");
        let v = it.next().unwrap_or("0");
        match k {
            "T2" => T2_CFG = v.parse().unwrap_or(0),
            "T3" => T3_CFG = v.parse().unwrap_or(0.0),
            _ => {}
        }
    }
}

unsafe fn read_u32_n(buf: &[u8], off: &mut usize, n: usize) -> *mut u32 {
    let p: *mut u32 = xmalloc(n);
    for i in 0..n {
        let q = *off + i*4;
        *p.add(i) = u32::from_le_bytes([buf[q],buf[q+1],buf[q+2],buf[q+3]]);
    }
    *off += n*4; p
}
unsafe fn read_i16_n(buf: &[u8], off: &mut usize, n: usize) -> *mut i16 {
    let p: *mut i16 = xmalloc(n);
    for i in 0..n {
        let q = *off + i*2;
        *p.add(i) = i16::from_le_bytes([buf[q],buf[q+1]]);
    }
    *off += n*2; p
}
unsafe fn read_f64_n(buf: &[u8], off: &mut usize, n: usize) -> *mut f64 {
    let p: *mut f64 = xmalloc(n);
    for i in 0..n {
        let q = *off + i*8;
        *p.add(i) = f64::from_le_bytes([buf[q],buf[q+1],buf[q+2],buf[q+3],
                                        buf[q+4],buf[q+5],buf[q+6],buf[q+7]]);
    }
    *off += n*8; p
}

unsafe fn load_weights() {
    let mut buf = Vec::new();
    File::open(format!("{}/weights.bin", DATA_DIR)).unwrap().read_to_end(&mut buf).unwrap();
    let mut off = 0usize;
    ACCEPT_TABLE = read_u32_n(&buf, &mut off, K1);
    REJECT_W     = read_i16_n(&buf, &mut off, B2_SLOTS);
    REF_VEC      = read_f64_n(&buf, &mut off, K3 * E_DIM);
    W1_MAT       = read_f64_n(&buf, &mut off, H_DIM * E_DIM);
    B1_VEC       = read_f64_n(&buf, &mut off, H_DIM);
    W2_MAT       = read_f64_n(&buf, &mut off, C_CLS * H_DIM);
    B2_VEC       = read_f64_n(&buf, &mut off, C_CLS);
    COS_TABLE    = read_f64_n(&buf, &mut off, E_DIM * DIM_D);
}

unsafe fn load_input() {
    ITEMS = xmalloc(N * DIM_D);
    let mut f = File::open(format!("{}/input.bin", DATA_DIR)).unwrap();
    let slice = std::slice::from_raw_parts_mut(ITEMS, N * DIM_D);
    f.read_exact(slice).unwrap();
}

#[inline(always)]
unsafe fn stage1_decide(it: *const u8) -> bool {
    let mut sig: u32 = 0;
    for i in 0..DIM_D { sig = sig.wrapping_add(*it.add(i) as u32); }
    sig &= 0xFFFF;
    let slot = (sig & 0x3FF) as usize;
    *ACCEPT_TABLE.add(slot) == sig
}
#[inline(always)]
unsafe fn stage2_score(it: *const u8) -> i32 {
    let mut hist = [0i32; B2_SLOTS];
    for i in 0..(DIM_D - 1) {
        let b = ((*it.add(i) as i32) * 7 + *it.add(i+1) as i32) & 0xFF;
        hist[b as usize] += 1;
    }
    let mut s: i32 = 0;
    for k in 0..B2_SLOTS { s += hist[k] * (*REJECT_W.add(k) as i32); }
    s
}
#[inline(always)]
unsafe fn stage3_embed(it: *const u8, emb: *mut f64) {
    for j in 0..E_DIM {
        let mut s = 0.0f64;
        let row = COS_TABLE.add(j * DIM_D);
        for i in 0..DIM_D { s += *row.add(i) * (*it.add(i) as f64 / 255.0); }
        *emb.add(j) = s;
    }
    let mut n2 = 0.0f64;
    for j in 0..E_DIM { let v = *emb.add(j); n2 += v * v; }
    if n2 > 0.0 {
        let inv = 1.0 / n2.sqrt();
        for j in 0..E_DIM { *emb.add(j) *= inv; }
    }
}
#[inline(always)]
unsafe fn stage3_best(emb: *const f64) -> (usize, f64) {
    let mut best = 0usize; let mut bs = -1e300f64;
    for k in 0..K3 {
        let r = REF_VEC.add(k * E_DIM);
        let mut s = 0.0f64;
        for j in 0..E_DIM { s += *emb.add(j) * *r.add(j); }
        if s > bs { bs = s; best = k; }
    }
    (best, bs)
}
#[inline(always)]
unsafe fn stage4_classify(emb: *const f64) -> usize {
    let mut hidden = [0.0f64; H_DIM];
    for h in 0..H_DIM {
        let mut s = *B1_VEC.add(h);
        let row = W1_MAT.add(h * E_DIM);
        for j in 0..E_DIM { s += *row.add(j) * *emb.add(j); }
        hidden[h] = if s > 0.0 { s } else { 0.0 };
    }
    let mut best = 0usize; let mut bs = -1e300f64;
    for c in 0..C_CLS {
        let mut s = *B2_VEC.add(c);
        let row = W2_MAT.add(c * H_DIM);
        for h in 0..H_DIM { s += *row.add(h) * hidden[h]; }
        if s > bs { bs = s; best = c; }
    }
    best
}

unsafe fn s_init() -> i64 {
    load_config();
    load_weights();
    load_input();
    DECISIONS = xmalloc(N);
    std::ptr::write_bytes(DECISIONS, 0, N);
    EMB_ALL = xmalloc(N * E_DIM);
    0
}
unsafe fn s_stage1(chunk_id: i64) -> i64 {
    let k = chunk_id as usize; let lo = k * CHUNK_SIZE;
    let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
    for i in lo..hi {
        let it = ITEMS.add(i * DIM_D);
        if stage1_decide(it) { *DECISIONS.add(i) = ACCEPT_S1; }
    }
    0
}
unsafe fn s_stage2(chunk_id: i64) -> i64 {
    let k = chunk_id as usize; let lo = k * CHUNK_SIZE;
    let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
    for i in lo..hi {
        if *DECISIONS.add(i) != 0 { continue; }
        let it = ITEMS.add(i * DIM_D);
        let s = stage2_score(it);
        if s > T2_CFG { *DECISIONS.add(i) = REJECT_S2; }
    }
    0
}
unsafe fn s_stage3(chunk_id: i64) -> i64 {
    let k = chunk_id as usize; let lo = k * CHUNK_SIZE;
    let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
    for i in lo..hi {
        if *DECISIONS.add(i) != 0 { continue; }
        let it  = ITEMS.add(i * DIM_D);
        let emb = EMB_ALL.add(i * E_DIM);
        stage3_embed(it, emb);
        let (best, bs) = stage3_best(emb);
        if bs > T3_CFG { *DECISIONS.add(i) = ACCEPT_S3_BASE | best as i32; }
    }
    0
}
unsafe fn s_stage4(chunk_id: i64) -> i64 {
    let k = chunk_id as usize; let lo = k * CHUNK_SIZE;
    let hi = if lo + CHUNK_SIZE > N { N } else { lo + CHUNK_SIZE };
    for i in lo..hi {
        if *DECISIONS.add(i) != 0 { continue; }
        let emb = EMB_ALL.add(i * E_DIM);
        let cls = stage4_classify(emb);
        *DECISIONS.add(i) = CLASS_BASE | cls as i32;
    }
    0
}
unsafe fn s_sync() -> i64 { 0 }
unsafe fn s_output() -> i64 {
    let mut cs: u32 = 0;
    for i in 0..N { cs = cs.wrapping_add(*DECISIONS.add(i) as u32); }
    println!("CHECKSUM={}", cs);
    cs as i64
}

#[no_mangle] pub unsafe extern "C" fn s__SUPER_INIT__(_in: *const i64, out: *mut i64) {
    *out = s_init();
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_STAGE1__(_in: *const i64, out: *mut i64) {
    *out = s_stage1(*_in);
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_STAGE2__(_in: *const i64, out: *mut i64) {
    *out = s_stage2(*_in);
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_STAGE3__(_in: *const i64, out: *mut i64) {
    *out = s_stage3(*_in);
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_STAGE4__(_in: *const i64, out: *mut i64) {
    *out = s_stage4(*_in);
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_SYNC__(_in: *const i64, out: *mut i64) {
    *out = s_sync();
}
#[no_mangle] pub unsafe extern "C" fn s__SUPER_OUTPUT__(_in: *const i64, out: *mut i64) {
    *out = s_output();
}

#[no_mangle] pub extern "C" fn supers_hs_init() {}
#[no_mangle] pub extern "C" fn supers_hs_exit() {}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {}
"""


def emit_fl(out_dir, n_chunks):
    # SAME .fl as gen_cip_c.py — both use the same superinst names.
    fl_lines = [
        f"superinst('init',     {SUPER_INIT},     1, False, False)",
        f"superinst('stage1',   {SUPER_STAGE1},   1, False, False)",
        f"superinst('stage2',   {SUPER_STAGE2},   1, False, False)",
        f"superinst('stage3',   {SUPER_STAGE3},   1, False, False)",
        f"superinst('stage4',   {SUPER_STAGE4},   1, False, False)",
        f"superinst('sync',     {SUPER_SYNC},     1, False, False)",
        f"superinst('output',   {SUPER_OUTPUT},   1, False, False)",
        "avgtime('stage1',  100)",
        "avgtime('stage2',  500)",
        "avgtime('stage3', 5000)",
        "avgtime('stage4', 5000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for k in range(n_chunks):
        fl_lines.append(f"const cid_{k}, {k}")
        fl_lines.append(f"stage1 s1_{k}, cid_{k}, ini")
        fl_lines.append(f"stage2 s2_{k}, cid_{k}, s1_{k}")
        fl_lines.append(f"stage3 s3_{k}, cid_{k}, s2_{k}")
        fl_lines.append(f"stage4 s4_{k}, cid_{k}, s3_{k}")
    current = [f"s4_{k}" for k in range(n_chunks)]
    sync_id = 0
    if len(current) == 1:
        root_var = current[0]
    else:
        while len(current) > 1:
            next_level = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl_lines.append(f"const {cname}, 0")
                fl_lines.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                next_level.append(sname)
                sync_id += 1
            current = next_level
        root_var = current[0]
    fl_lines.append(f"output out, {root_var}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, N, CHUNK_SIZE):
    os.makedirs(out_dir, exist_ok=True)
    # Crate dir for the staticlib (per gen_sc_rust.py convention).
    crate_dir = os.path.join(out_dir, "cip_rs_supers")
    os.makedirs(crate_dir, exist_ok=True)
    n_chunks = (N + CHUNK_SIZE - 1) // CHUNK_SIZE
    emit_fl(out_dir, n_chunks)
    print(f"[gen_cip_rust] wrote {out_dir}/attn.fl  (n_chunks={n_chunks})")
    with open(os.path.join(crate_dir, "Cargo.toml"), "w") as f:
        f.write(RS_CARGO_TOML)
    src = (RS_TEMPLATE
           .replace("__N__", str(N))
           .replace("__CHUNK_SIZE__", str(CHUNK_SIZE))
           .replace("__DATA_DIR__", data_dir)
           .replace("__SUPER_INIT__",   str(SUPER_INIT))
           .replace("__SUPER_STAGE1__", str(SUPER_STAGE1))
           .replace("__SUPER_STAGE2__", str(SUPER_STAGE2))
           .replace("__SUPER_STAGE3__", str(SUPER_STAGE3))
           .replace("__SUPER_STAGE4__", str(SUPER_STAGE4))
           .replace("__SUPER_SYNC__",   str(SUPER_SYNC))
           .replace("__SUPER_OUTPUT__", str(SUPER_OUTPUT)))
    with open(os.path.join(crate_dir, "lib.rs"), "w") as f:
        f.write(src)
    print(f"[gen_cip_rust] wrote {crate_dir}/lib.rs + Cargo.toml")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.out_dir, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CHUNK_SIZE"]))


if __name__ == "__main__":
    main()
