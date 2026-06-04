#!/usr/bin/env python3
"""Generate Ribault dense block Cholesky files with Rust-implemented supers.
Mirrors gen_sc_c.py — same .fl, different super body language (Rust staticlib)."""

import argparse, os, struct


SUPER_OUTPUT, SUPER_TRSM, SUPER_POTRF = 10, 11, 12
SUPER_INIT, SUPER_SYRK, SUPER_GEMM    = 13, 14, 15

OP_KIND_TO_NAME = {0: 'potrf', 1: 'trsm', 2: 'syrk', 3: 'gemm'}


def read_dag(data_dir):
    path = os.path.join(data_dir, "dag.bin")
    ops = []
    with open(path, "rb") as f:
        (n_ops,) = struct.unpack("<i", f.read(4))
        for _ in range(n_ops):
            kind, ti, tj, s1i, s1j, s2i, s2j, level, n_deps = struct.unpack("<9i", f.read(36))
            deps = list(struct.unpack(f"<{n_deps}i", f.read(4 * n_deps))) if n_deps else []
            ops.append({'id': len(ops), 'kind': kind, 'ti': ti, 'tj': tj,
                        's1i': s1i, 's1j': s1j, 's2i': s2i, 's2j': s2j,
                        'level': level, 'deps': deps})
    return ops


CARGO_TOML = """[package]
name = "sc_rs_supers"
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


RS_TEMPLATE = r"""//! Auto-generated Ribault dense block Cholesky Rust supers.
#![allow(non_snake_case)]
#![allow(static_mut_refs)]

use std::os::raw::{c_double, c_int};
use std::fs::File;
use std::io::Read;

const NB: usize    = __NB__;
const B: usize     = __B__;
const N_OPS: usize = __N_OPS__;
const DATA_DIR: &str = "__DATA_DIR__";

static mut A: *mut c_double = std::ptr::null_mut();
static mut OP_TI:  *mut c_int = std::ptr::null_mut();
static mut OP_TJ:  *mut c_int = std::ptr::null_mut();
static mut OP_S1I: *mut c_int = std::ptr::null_mut();
static mut OP_S1J: *mut c_int = std::ptr::null_mut();
static mut OP_S2I: *mut c_int = std::ptr::null_mut();
static mut OP_S2J: *mut c_int = std::ptr::null_mut();

#[inline(always)] fn block_idx(i: usize, j: usize) -> usize { i * (i + 1) / 2 + j }

unsafe fn xmalloc_d(n: usize) -> *mut c_double {
    let p = libc::malloc(n * std::mem::size_of::<c_double>()) as *mut c_double;
    assert!(!p.is_null()); p
}
unsafe fn xmalloc_i(n: usize) -> *mut c_int {
    let p = libc::malloc(n * std::mem::size_of::<c_int>()) as *mut c_int;
    assert!(!p.is_null()); p
}

unsafe fn load_dag() {
    let path = format!("{}/dag.bin", DATA_DIR);
    let mut f = File::open(&path).unwrap();
    let mut hdr4 = [0u8; 4];
    f.read_exact(&mut hdr4).unwrap();
    let n = i32::from_le_bytes(hdr4) as usize;
    assert_eq!(n, N_OPS);
    OP_TI  = xmalloc_i(N_OPS);
    OP_TJ  = xmalloc_i(N_OPS);
    OP_S1I = xmalloc_i(N_OPS);
    OP_S1J = xmalloc_i(N_OPS);
    OP_S2I = xmalloc_i(N_OPS);
    OP_S2J = xmalloc_i(N_OPS);
    for i in 0..N_OPS {
        let mut hdr = [0u8; 36];
        f.read_exact(&mut hdr).unwrap();
        let vals: [i32; 9] = std::array::from_fn(|k|
            i32::from_le_bytes([hdr[k*4], hdr[k*4+1], hdr[k*4+2], hdr[k*4+3]]));
        *OP_TI.add(i)  = vals[1]; *OP_TJ.add(i)  = vals[2];
        *OP_S1I.add(i) = vals[3]; *OP_S1J.add(i) = vals[4];
        *OP_S2I.add(i) = vals[5]; *OP_S2J.add(i) = vals[6];
        let nd = vals[8] as usize;
        if nd > 0 { let mut sk = vec![0u8; nd * 4]; f.read_exact(&mut sk).unwrap(); }
    }
}

unsafe fn s_init() -> i64 {
    let n_blocks = NB * (NB + 1) / 2;
    A = xmalloc_d(n_blocks * B * B);
    let path = format!("{}/A.bin", DATA_DIR);
    let mut f = File::open(&path).unwrap();
    let total = n_blocks * B * B;
    let mut buf = vec![0u8; total * 8];
    f.read_exact(&mut buf).unwrap();
    for i in 0..total {
        *A.add(i) = f64::from_le_bytes([buf[i*8],buf[i*8+1],buf[i*8+2],buf[i*8+3],
                                        buf[i*8+4],buf[i*8+5],buf[i*8+6],buf[i*8+7]]);
    }
    load_dag();
    0
}

unsafe fn block_ptr(i: usize, j: usize) -> *mut c_double {
    A.add(block_idx(i, j) * B * B)
}

unsafe fn potrf_at(tgt: *mut c_double) {
    for j in 0..B {
        let mut s = *tgt.add(j*B + j);
        for kk in 0..j { let x = *tgt.add(j*B + kk); s -= x * x; }
        let sq = s.sqrt();
        *tgt.add(j*B + j) = sq;
        let inv = 1.0 / sq;
        for i in (j+1)..B {
            let mut t = *tgt.add(i*B + j);
            for kk in 0..j { t -= *tgt.add(i*B + kk) * *tgt.add(j*B + kk); }
            *tgt.add(i*B + j) = t * inv;
        }
    }
    for i in 0..B { for j in (i+1)..B { *tgt.add(i*B + j) = 0.0; } }
}

unsafe fn trsm_at(x: *mut c_double, l: *const c_double) {
    for i in 0..B {
        for j in 0..B {
            let mut s = *x.add(i*B + j);
            for kk in 0..j { s -= *x.add(i*B + kk) * *l.add(j*B + kk); }
            *x.add(i*B + j) = s / *l.add(j*B + j);
        }
    }
}

unsafe fn syrk_at(c: *mut c_double, a: *const c_double) {
    for i in 0..B { for j in 0..=i {
        let mut s = 0.0;
        for kk in 0..B { s += *a.add(i*B + kk) * *a.add(j*B + kk); }
        *c.add(i*B + j) -= s;
    } }
}

unsafe fn gemm_at(c: *mut c_double, a: *const c_double, b_: *const c_double) {
    for i in 0..B { for j in 0..B {
        let mut s = 0.0;
        for kk in 0..B { s += *a.add(i*B + kk) * *b_.add(j*B + kk); }
        *c.add(i*B + j) -= s;
    } }
}

unsafe fn s_potrf(idx: i64) -> i64 {
    let i = idx as usize;
    potrf_at(block_ptr(*OP_TI.add(i) as usize, *OP_TJ.add(i) as usize));
    0
}
unsafe fn s_trsm(idx: i64) -> i64 {
    let i = idx as usize;
    trsm_at(block_ptr(*OP_TI.add(i) as usize, *OP_TJ.add(i) as usize),
            block_ptr(*OP_S1I.add(i) as usize, *OP_S1J.add(i) as usize));
    0
}
unsafe fn s_syrk(idx: i64) -> i64 {
    let i = idx as usize;
    syrk_at(block_ptr(*OP_TI.add(i) as usize, *OP_TJ.add(i) as usize),
            block_ptr(*OP_S1I.add(i) as usize, *OP_S1J.add(i) as usize));
    0
}
unsafe fn s_gemm(idx: i64) -> i64 {
    let i = idx as usize;
    gemm_at(block_ptr(*OP_TI.add(i) as usize, *OP_TJ.add(i) as usize),
            block_ptr(*OP_S1I.add(i) as usize, *OP_S1J.add(i) as usize),
            block_ptr(*OP_S2I.add(i) as usize, *OP_S2J.add(i) as usize));
    0
}

unsafe fn s_output() -> i64 {
    let n_blocks = NB * (NB + 1) / 2;
    let total = n_blocks * B * B;
    let mut cs: u64 = 0;
    for i in 0..total {
        let v = *A.add(i);
        let fixed = (v * 1e6) as i64;
        cs = (cs + (fixed as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    println!("CHECKSUM={}", cs);
    cs as i64
}

#[no_mangle] pub unsafe extern "C" fn s__SUPER_INIT__(_in: *const i64, out: *mut i64)   { *out = s_init(); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_POTRF__(in_: *const i64, out: *mut i64)  { *out = s_potrf(*in_); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_TRSM__(in_: *const i64, out: *mut i64)   { *out = s_trsm(*in_); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_SYRK__(in_: *const i64, out: *mut i64)   { *out = s_syrk(*in_); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_GEMM__(in_: *const i64, out: *mut i64)   { *out = s_gemm(*in_); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_RESULT__(_in: *const i64, out: *mut i64) { *out = s_output(); }

#[no_mangle] pub extern "C" fn supers_hs_init() {}
#[no_mangle] pub extern "C" fn supers_hs_exit() {}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {}
"""


def node_var(i): return f"op_{i}"


def emit_fl(out_dir, ops):
    fl = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('potrf',  {SUPER_POTRF},  1, False, False)",
        f"superinst('trsm',   {SUPER_TRSM},   1, False, False)",
        f"superinst('syrk',   {SUPER_SYRK},   1, False, False)",
        f"superinst('gemm',   {SUPER_GEMM},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('potrf', 100)",
        "avgtime('trsm',  500)",
        "avgtime('syrk',  500)",
        "avgtime('gemm',  500)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for op in ops:
        oid = op['id']; oname = OP_KIND_TO_NAME[op['kind']]
        dep_vars = [node_var(d) for d in op['deps']] if op['deps'] else ['ini']
        fl.append(f"const k_{oid}, {oid}")
        fl.append(f"{oname} {node_var(oid)}, k_{oid}, " + ", ".join(dep_vars))
    fl.append(f"output out, {node_var(len(ops) - 1)}")
    with open(os.path.join(out_dir, "attn.fl"), "w") as f:
        f.write("\n".join(fl) + "\n")


def emit(out_dir, data_dir, NB, B):
    os.makedirs(out_dir, exist_ok=True)
    ops = read_dag(data_dir)
    emit_fl(out_dir, ops)
    print(f"[gen_sc_rust] wrote {out_dir}/attn.fl  (n_ops={len(ops)})")

    project_dir = os.path.join(out_dir, "sc_rs_supers")
    src_dir = os.path.join(project_dir, "src")
    os.makedirs(src_dir, exist_ok=True)
    with open(os.path.join(project_dir, "Cargo.toml"), "w") as f:
        f.write(CARGO_TOML)
    rs = (RS_TEMPLATE
          .replace("__NB__", str(NB))
          .replace("__B__", str(B))
          .replace("__N_OPS__", str(len(ops)))
          .replace("__DATA_DIR__", data_dir)
          .replace("__SUPER_INIT__",   str(SUPER_INIT))
          .replace("__SUPER_POTRF__",  str(SUPER_POTRF))
          .replace("__SUPER_TRSM__",   str(SUPER_TRSM))
          .replace("__SUPER_SYRK__",   str(SUPER_SYRK))
          .replace("__SUPER_GEMM__",   str(SUPER_GEMM))
          .replace("__SUPER_RESULT__", str(SUPER_OUTPUT)))
    with open(os.path.join(src_dir, "lib.rs"), "w") as f:
        f.write(rs)
    print(f"[gen_sc_rust] wrote {project_dir}/")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split(); cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["NB"], cfg["B"])


if __name__ == "__main__":
    main()
