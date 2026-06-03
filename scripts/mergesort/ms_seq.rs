//! Mergesort sequential baseline in Rust — monolithic top-down recursive
//! merge sort on a flat i32 array. Below CUTOFF: insertion sort. Above:
//! split, recurse, merge into scratch, copy back. Same algorithm as ms_seq.c.
//!
//! CRITICAL FAIRNESS NOTE: the parallel Rust variants (ribault_rust, timely,
//! sucuri) do raw `*mut i32` pointer arithmetic + `unsafe` on the inner
//! kernels (insertion sort, merge). If seq_rust kept Rust's default bounds
//! checking it would be artificially slow vs the parallel numerator. seq_rust
//! therefore uses the same `unsafe` raw-pointer pattern on every inner loop.

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

fn load_config(dir: &str) -> (usize, usize) {
    let path = format!("{}/config.txt", dir);
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut n: usize = 0;
    let mut cutoff: usize = 64;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key {
            "N"      => n = val,
            "CUTOFF" => cutoff = val,
            _ => {}
        }
    }
    (n, cutoff)
}

#[inline(always)]
unsafe fn insertion_sort(a: *mut i32, lo: usize, hi: usize) {
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
unsafe fn merge_to(a: *mut i32, lo: usize, mid: usize, hi: usize, t: *mut i32) {
    let mut i = lo;
    let mut j = mid;
    let mut k = lo;
    while i < mid && j < hi {
        let av = *a.add(i);
        let bv = *a.add(j);
        if av <= bv { *t.add(k) = av; i += 1; }
        else        { *t.add(k) = bv; j += 1; }
        k += 1;
    }
    while i < mid { *t.add(k) = *a.add(i); i += 1; k += 1; }
    while j < hi  { *t.add(k) = *a.add(j); j += 1; k += 1; }
    // copy t[lo..hi) back to a[lo..hi)
    let mut p = lo;
    while p < hi { *a.add(p) = *t.add(p); p += 1; }
}

unsafe fn ms_sort(a: *mut i32, lo: usize, hi: usize, t: *mut i32, cutoff: usize) {
    if hi - lo <= cutoff { insertion_sort(a, lo, hi); return; }
    let mid = lo + (hi - lo) / 2;
    ms_sort(a, lo, mid, t, cutoff);
    ms_sort(a, mid, hi, t, cutoff);
    merge_to(a, lo, mid, hi, t);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} DATA_DIR", args[0]);
        std::process::exit(2);
    }
    let dir = &args[1];
    let (n, cutoff) = load_config(dir);

    // Load input.bin
    let path = format!("{}/input.bin", dir);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; n * 4];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    let mut arr: Vec<i32> = (0..n).map(|i| {
        i32::from_le_bytes([buf[i*4], buf[i*4+1], buf[i*4+2], buf[i*4+3]])
    }).collect();
    let mut tmp: Vec<i32> = vec![0; n];

    // Pre-sort checksum
    let mut pre: u64 = 0;
    for i in 0..n { pre = (pre + (arr[i] as u32 as u64)) & 0xFFFFFFFF_u64; }

    let t0 = Instant::now();
    unsafe { ms_sort(arr.as_mut_ptr(), 0, n, tmp.as_mut_ptr(), cutoff); }
    let secs = t0.elapsed().as_secs_f64();

    // Verify sorted + post-sort checksum
    let mut cs: u64 = 0;
    for i in 0..n {
        if i > 0 && arr[i] < arr[i - 1] {
            eprintln!("FATAL: array not sorted at i={}", i);
            std::process::exit(1);
        }
        cs = (cs + (arr[i] as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    if cs != pre {
        eprintln!("FATAL: checksum changed pre={} post={}", pre, cs);
        std::process::exit(1);
    }

    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
