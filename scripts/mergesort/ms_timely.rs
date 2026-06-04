//! Mergesort Timely Dataflow variant — same binary tree topology as
//! Ribault-Hs and STRAT (loaded from data_dir/tree.bin), parallelism via
//! Timely's epoch-based driver: epoch 0 = leaf sorts, epoch L = level-L
//! merges. Probe.less_than() barriers between epochs.
//!
//! Inner kernels: raw `*mut i32` + unsafe — identical to ribault_rust
//! mergesort leaf compute, so the per-block math matches byte-for-byte.

use timely::dataflow::operators::{Input, Map, Probe, Exchange};
use timely::dataflow::{InputHandle, ProbeHandle};
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
        match key { "N" => n = val, "CUTOFF" => cutoff = val, _ => {} }
    }
    (n, cutoff)
}

fn load_tree(dir: &str) -> (Vec<(usize, usize)>, Vec<(usize, usize, usize, usize)>) {
    let path = format!("{}/tree.bin", dir);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut hdr = [0u8; 8];
    f.read_exact(&mut hdr).unwrap();
    let nl = i32::from_le_bytes([hdr[0], hdr[1], hdr[2], hdr[3]]) as usize;
    let nm = i32::from_le_bytes([hdr[4], hdr[5], hdr[6], hdr[7]]) as usize;

    let mut leaves = Vec::with_capacity(nl);
    let mut buf8 = [0u8; 8];
    for _ in 0..nl {
        f.read_exact(&mut buf8).unwrap();
        let lo = i32::from_le_bytes([buf8[0],buf8[1],buf8[2],buf8[3]]) as usize;
        let hi = i32::from_le_bytes([buf8[4],buf8[5],buf8[6],buf8[7]]) as usize;
        leaves.push((lo, hi));
    }
    let mut merges = Vec::with_capacity(nm);
    let mut buf16 = [0u8; 16];
    for _ in 0..nm {
        f.read_exact(&mut buf16).unwrap();
        let lo  = i32::from_le_bytes([buf16[0],buf16[1],buf16[2],buf16[3]]) as usize;
        let mid = i32::from_le_bytes([buf16[4],buf16[5],buf16[6],buf16[7]]) as usize;
        let hi  = i32::from_le_bytes([buf16[8],buf16[9],buf16[10],buf16[11]]) as usize;
        let lev = i32::from_le_bytes([buf16[12],buf16[13],buf16[14],buf16[15]]) as usize;
        merges.push((lo, mid, hi, lev));
    }
    (leaves, merges)
}

fn read_input_i32(dir: &str, n: usize) -> Vec<i32> {
    let path = format!("{}/input.bin", dir);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; n * 4];
    f.read_exact(&mut buf).unwrap();
    (0..n).map(|i| i32::from_le_bytes([buf[i*4], buf[i*4+1], buf[i*4+2], buf[i*4+3]])).collect()
}

// Leaf sort: pdqsort (std::sort_unstable). O(B log B). Same as ms_seq.rs.
unsafe fn leaf_sort(a: *mut i32, lo: usize, hi: usize) {
    let slice = std::slice::from_raw_parts_mut(a.add(lo), hi - lo);
    slice.sort_unstable();
}

unsafe fn merge_op(arr: *mut i32, tmp: *mut i32, lo: usize, mid: usize, hi: usize) {
    let mut i = lo; let mut j = mid; let mut k = lo;
    while i < mid && j < hi {
        let av = *arr.add(i);
        let bv = *arr.add(j);
        if av <= bv { *tmp.add(k) = av; i += 1; }
        else        { *tmp.add(k) = bv; j += 1; }
        k += 1;
    }
    while i < mid { *tmp.add(k) = *arr.add(i); i += 1; k += 1; }
    while j < hi  { *tmp.add(k) = *arr.add(j); j += 1; k += 1; }
    let mut p = lo;
    while p < hi { *arr.add(p) = *tmp.add(p); p += 1; }
}

#[derive(Clone, Copy)]
struct PtrSet { arr: usize, tmp: usize }

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut dir = None;
    let mut filtered: Vec<String> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        let a = &args[i];
        // Pass timely's own args through (e.g., -w, -n, --threads)
        if a == "-w" || a == "-n" || a == "--threads" || a == "--processes" || a == "--process" {
            filtered.push(a.clone());
            if i + 1 < args.len() { filtered.push(args[i+1].clone()); i += 1; }
        } else if a.starts_with("--") {
            filtered.push(a.clone());
        } else if i == 0 {
            filtered.push(a.clone());
        } else if dir.is_none() {
            dir = Some(a.clone());
        }
        i += 1;
    }
    let dir = dir.unwrap_or_else(|| { eprintln!("usage: ms_timely DATA_DIR [-w P]"); std::process::exit(2); });

    let (n, _cutoff) = load_config(&dir);
    let (leaves, merges) = load_tree(&dir);

    // Pre-compute level groups: levels[lev] = vec![merge_idx, ...] for lev = 1..max
    let max_lev = merges.iter().map(|m| m.3).max().unwrap_or(0);
    let mut level_groups: Vec<Vec<u32>> = vec![Vec::new(); max_lev + 1];
    for (i, m) in merges.iter().enumerate() {
        level_groups[m.3].push(i as u32);
    }

    let mut arr = read_input_i32(&dir, n);
    let mut tmp: Vec<i32> = vec![0; n];

    // Pre-sort checksum
    let mut pre: u64 = 0;
    for i in 0..n { pre = (pre + (arr[i] as u32 as u64)) & 0xFFFFFFFF_u64; }

    let ptrs = PtrSet {
        arr: arr.as_mut_ptr() as usize,
        tmp: tmp.as_mut_ptr() as usize,
    };

    // Move leaves/merges into Arcs for sharing across workers
    let leaves = std::sync::Arc::new(leaves);
    let merges = std::sync::Arc::new(merges);
    let level_groups = std::sync::Arc::new(level_groups);

    let t0 = Instant::now();

    timely::execute_from_args(filtered.into_iter(), move |worker| {
        let leaves_for_map  = leaves.clone();
        let merges_for_map  = merges.clone();
        let leaves_for_send = leaves.clone();
        let level_groups_for_send = level_groups.clone();

        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();

        worker.dataflow::<u64, _, _>(|scope| {
            input.to_stream(scope)
                .exchange(|&(_epoch, idx): &(u32, u32)| idx as u64)
                .map(move |(epoch, idx)| {
                    let idx = idx as usize;
                    unsafe {
                        let arr = ptrs.arr as *mut i32;
                        let tmp = ptrs.tmp as *mut i32;
                        if epoch == 0 {
                            // Leaf sort
                            let (lo, hi) = leaves_for_map[idx];
                            leaf_sort(arr, lo, hi);
                        } else {
                            // Merge at level `epoch`
                            let (lo, mid, hi, _) = merges_for_map[idx];
                            merge_op(arr, tmp, lo, mid, hi);
                        }
                    }
                    (epoch, idx as u32)
                })
                .probe_with(&mut probe);
        });

        // Epoch 0: leaves
        if worker.index() == 0 {
            for i in 0..leaves_for_send.len() as u32 { input.send((0, i)); }
        }
        input.advance_to(1);
        while probe.less_than(input.time()) { worker.step(); }

        // Epochs 1..max_lev: per-level merges
        for lev in 1..=max_lev as u64 {
            if worker.index() == 0 {
                for &midx in level_groups_for_send[lev as usize].iter() {
                    input.send((lev as u32, midx));
                }
            }
            input.advance_to(lev + 1);
            while probe.less_than(input.time()) { worker.step(); }
        }
    }).unwrap();

    let secs = t0.elapsed().as_secs_f64();

    // Verify + checksum
    let mut cs: u64 = 0;
    let mut ok = true;
    for i in 0..n {
        if i > 0 && arr[i] < arr[i - 1] { ok = false; }
        cs = (cs + (arr[i] as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    if !ok { eprintln!("FATAL: not sorted"); std::process::exit(1); }
    if cs != pre { eprintln!("FATAL: checksum changed pre={} post={}", pre, cs); std::process::exit(1); }

    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
