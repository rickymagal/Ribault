//! N-Queens sequential baseline in Rust — mirrors nq_seq.c byte-for-byte
//! algorithmically.  The fairness preservation idiom here is NOT `unsafe`
//! raw pointers (the inner loop has no memory indirection that could
//! benefit from unchecked indexing — `queens` is a 16-element stack
//! array and the compiler eliminates bounds checks via `#[inline(always)]`
//! and the small fixed size).  Instead we lean on `opt-level=3`,
//! `lto="thin"`, `codegen-units=1`, and aggressive inlining.

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

const MAX_N: usize = 16;


fn load_config(dir: &str) -> (usize, usize, usize) {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut n = 0usize; let mut cutoff = 0usize; let mut n_states = 0usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key {
            "N"        => n        = val,
            "CUTOFF"   => cutoff   = val,
            "N_STATES" => n_states = val,
            _ => {}
        }
    }
    (n, cutoff, n_states)
}

fn load_states(dir: &str, n_states: usize, cutoff: usize) -> Vec<i32> {
    let mut f = File::open(format!("{}/states.bin", dir)).unwrap();
    let mut hdr = [0u8; 8];
    f.read_exact(&mut hdr).unwrap();
    let n_hdr  = i32::from_le_bytes([hdr[0], hdr[1], hdr[2], hdr[3]]) as usize;
    let c_hdr  = i32::from_le_bytes([hdr[4], hdr[5], hdr[6], hdr[7]]) as usize;
    assert_eq!(n_hdr,  n_states);
    assert_eq!(c_hdr,  cutoff);
    let mut buf = vec![0u8; n_states * cutoff * 4];
    f.read_exact(&mut buf).unwrap();
    (0..n_states * cutoff).map(|i| {
        let off = i * 4;
        i32::from_le_bytes([buf[off], buf[off+1], buf[off+2], buf[off+3]])
    }).collect()
}


#[inline(always)]
fn safe_q(queens: &[i32; MAX_N], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                                        { return false; }
        if c - r as i32 == col - row as i32                { return false; }
        if c + r as i32 == col + row as i32                { return false; }
    }
    true
}

fn solve_sub(queens: &mut [i32; MAX_N], row: usize, n: usize) -> u64 {
    if row == n { return 1; }
    let mut count = 0u64;
    for c in 0..n as i32 {
        if safe_q(queens, row, c) {
            queens[row] = c;
            count += solve_sub(queens, row + 1, n);
        }
    }
    count
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 { eprintln!("usage: {} DATA_DIR", args[0]); std::process::exit(2); }
    let dir = &args[1];
    let (n, cutoff, n_states) = load_config(dir);
    let states = load_states(dir, n_states, cutoff);

    let t0 = Instant::now();
    let mut total: u64 = 0;
    let mut queens = [0i32; MAX_N];
    for s in 0..n_states {
        for r in 0..cutoff { queens[r] = states[s * cutoff + r]; }
        total += solve_sub(&mut queens, cutoff, n);
    }
    let secs = t0.elapsed().as_secs_f64();
    println!("CHECKSUM={}", total);
    println!("RUNTIME_SEC={}", secs);
}
