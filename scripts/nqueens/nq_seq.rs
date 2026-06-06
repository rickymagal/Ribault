//! N-Queens sequential baseline in Rust — full recursion from row=0
//! inside this binary; mirrors nq_seq.c byte-for-byte algorithmically.
//! Fairness: the inner loop has no memory indirection that benefits
//! from unchecked indexing, so we lean on `#[inline(always)]` and
//! `opt-level=3 lto="thin"` instead of `unsafe`.

use std::env;
use std::time::Instant;

const MAX_N: usize = 20;


fn load_config(dir: &str) -> usize {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut n = 0usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        if key == "N" { n = val; }
    }
    n
}

#[inline(always)]
fn safe_q(queens: &[i32; MAX_N], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                                  { return false; }
        if c - r as i32 == col - row as i32          { return false; }
        if c + r as i32 == col + row as i32          { return false; }
    }
    true
}

fn solve(queens: &mut [i32; MAX_N], row: usize, n: usize) -> u64 {
    if row == n { return 1; }
    let mut cnt = 0u64;
    for c in 0..n as i32 {
        if safe_q(queens, row, c) {
            queens[row] = c;
            cnt += solve(queens, row + 1, n);
        }
    }
    cnt
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 { eprintln!("usage: {} DATA_DIR", args[0]); std::process::exit(2); }
    let n = load_config(&args[1]);

    let t0 = Instant::now();
    let mut queens = [0i32; MAX_N];
    let total = solve(&mut queens, 0, n);
    let secs = t0.elapsed().as_secs_f64();
    println!("CHECKSUM={}", total);
    println!("RUNTIME_SEC={}", secs);
}
