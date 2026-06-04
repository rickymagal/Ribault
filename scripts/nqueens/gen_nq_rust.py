#!/usr/bin/env python3
"""Generate Ribault N-Queens files with Rust-implemented supers.

Mirror of gen_nq_c.py with Rust super bodies (staticlib via libc::malloc
+ raw pointers).  Same .fl topology and super numbering.
"""

import argparse, os


SUPER_INIT   = 10
SUPER_SOLVE  = 11
SUPER_SYNC   = 15
SUPER_OUTPUT = 16
MAX_FANIN    = 30


RS_CARGO = """[package]
name = "nq_rs_supers"
version = "0.1.0"
edition = "2021"

[lib]
name = "supers_rust"
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


RS_TEMPLATE = r"""// Auto-generated: Ribault N-Queens Rust supers.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(static_mut_refs)]

use std::fs::File;
use std::io::Read;

const MAX_N:    usize = 16;
const N:        usize = __N__;
const CUTOFF:   usize = __CUTOFF__;
const N_STATES: usize = __N_STATES__;
const DATA_DIR: &str  = "__DATA_DIR__";

static mut STATES: *mut i32 = std::ptr::null_mut();
static mut COUNTS: *mut u64 = std::ptr::null_mut();
static mut TOTAL:  u64      = 0;

unsafe fn xmalloc<T>(n: usize) -> *mut T {
    libc::malloc(n * std::mem::size_of::<T>()) as *mut T
}

unsafe fn load_states() {
    let mut f = File::open(format!("{}/states.bin", DATA_DIR)).unwrap();
    let mut hdr = [0u8; 8]; f.read_exact(&mut hdr).unwrap();
    let nh = i32::from_le_bytes([hdr[0],hdr[1],hdr[2],hdr[3]]) as usize;
    let ch = i32::from_le_bytes([hdr[4],hdr[5],hdr[6],hdr[7]]) as usize;
    assert_eq!(nh, N_STATES); assert_eq!(ch, CUTOFF);
    STATES = xmalloc(N_STATES * CUTOFF);
    let mut buf = vec![0u8; N_STATES * CUTOFF * 4];
    f.read_exact(&mut buf).unwrap();
    for i in 0..N_STATES * CUTOFF {
        let off = i * 4;
        *STATES.add(i) = i32::from_le_bytes([buf[off],buf[off+1],buf[off+2],buf[off+3]]);
    }
    COUNTS = xmalloc(N_STATES);
    std::ptr::write_bytes(COUNTS, 0, N_STATES);
}


#[inline(always)]
unsafe fn safe_q(queens: &[i32; MAX_N], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                                  { return false; }
        if c - r as i32 == col - row as i32          { return false; }
        if c + r as i32 == col + row as i32          { return false; }
    }
    true
}

unsafe fn solve_sub(queens: &mut [i32; MAX_N], row: usize) -> u64 {
    if row == N { return 1; }
    let mut cnt = 0u64;
    for c in 0..N as i32 {
        if safe_q(queens, row, c) {
            queens[row] = c;
            cnt += solve_sub(queens, row + 1);
        }
    }
    cnt
}


unsafe fn s_init() -> i64 { load_states(); 0 }

unsafe fn s_solve(state_idx: i64) -> i64 {
    let si = state_idx as usize;
    let mut queens = [0i32; MAX_N];
    for r in 0..CUTOFF { queens[r] = *STATES.add(si * CUTOFF + r); }
    *COUNTS.add(si) = solve_sub(&mut queens, CUTOFF);
    0
}

unsafe fn s_sync() -> i64 { 0 }

unsafe fn s_output() -> i64 {
    let mut t: u64 = 0;
    for i in 0..N_STATES { t = t.wrapping_add(*COUNTS.add(i)); }
    TOTAL = t;
    println!("CHECKSUM={}", t);
    t as i64
}


#[no_mangle] pub unsafe extern "C" fn s__SUPER_INIT__(_in: *const i64, out: *mut i64)   { *out = s_init(); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_SOLVE__(_in: *const i64, out: *mut i64)  { *out = s_solve(*_in); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_SYNC__(_in: *const i64, out: *mut i64)   { *out = s_sync(); }
#[no_mangle] pub unsafe extern "C" fn s__SUPER_OUTPUT__(_in: *const i64, out: *mut i64) { *out = s_output(); }

#[no_mangle] pub extern "C" fn supers_hs_init() {}
#[no_mangle] pub extern "C" fn supers_hs_exit() {}
#[no_mangle] pub extern "C" fn supers_hs_init_thread() {}
#[no_mangle] pub extern "C" fn supers_hs_thread_done() {}
"""


def emit_fl(out_dir, n_states):
    fl = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('solve',  {SUPER_SOLVE},  1, False, False)",
        f"superinst('sync',   {SUPER_SYNC},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('solve', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for s in range(n_states):
        fl.append(f"const sid_{s}, {s}")
        fl.append(f"solve sv_{s}, sid_{s}, ini")
    current = [f"sv_{s}" for s in range(n_states)]
    sync_id = 0
    if len(current) == 1:
        root = current[0]
    else:
        while len(current) > 1:
            nxt = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl.append(f"const {cname}, 0")
                fl.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                nxt.append(sname); sync_id += 1
            current = nxt
        root = current[0]
    fl.append(f"output out, {root}")
    with open(os.path.join(out_dir, "attn.fl"), "w") as f:
        f.write("\n".join(fl) + "\n")


def emit(out_dir, data_dir, N, CUTOFF, n_states):
    os.makedirs(out_dir, exist_ok=True)
    crate = os.path.join(out_dir, "nq_rs_supers"); os.makedirs(crate, exist_ok=True)
    emit_fl(out_dir, n_states)
    print(f"[gen_nq_rust] wrote {out_dir}/attn.fl  (n_states={n_states})")
    with open(os.path.join(crate, "Cargo.toml"), "w") as f: f.write(RS_CARGO)
    src = (RS_TEMPLATE
           .replace("__N__", str(N))
           .replace("__CUTOFF__", str(CUTOFF))
           .replace("__N_STATES__", str(n_states))
           .replace("__DATA_DIR__", data_dir)
           .replace("__SUPER_INIT__",   str(SUPER_INIT))
           .replace("__SUPER_SOLVE__",  str(SUPER_SOLVE))
           .replace("__SUPER_SYNC__",   str(SUPER_SYNC))
           .replace("__SUPER_OUTPUT__", str(SUPER_OUTPUT)))
    with open(os.path.join(crate, "lib.rs"), "w") as f: f.write(src)
    print(f"[gen_nq_rust] wrote {crate}/")


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
         int(cfg["N"]), int(cfg["CUTOFF"]), int(cfg["N_STATES"]))


if __name__ == "__main__":
    main()
