//! N-Queens Timely Dataflow variant.  Reads the same depth-`cutoff`
//! prefix states emitted by gen_input.py and runs the sequential
//! `solve_sub` on each via a Timely operator with `.exchange()` for
//! load balance and a single epoch.  No multi-epoch recursion is
//! needed because the depth-`cutoff` expansion produces an
//! embarrassingly-parallel set of subtree-roots; this is the
//! idiomatic Timely shape for backtracking with a cutoff (same shape
//! as the Strategies / par-pseq variants — only the runtime differs).
//!
//! Same kernels as nq_seq.rs (no `unsafe` because the inner loop has
//! no memory indirection that benefits from unchecked indexing).

use std::env;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;
use timely::dataflow::operators::{Input, Exchange, Inspect, Probe};
use timely::dataflow::{InputHandle, ProbeHandle};

const MAX_N: usize = 16;


fn load_config(dir: &str) -> (usize, usize, usize) {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut n = 0usize; let mut cutoff = 0usize; let mut n_states = 0usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key { "N" => n = val, "CUTOFF" => cutoff = val, "N_STATES" => n_states = val, _ => {} }
    }
    (n, cutoff, n_states)
}

fn load_states(dir: &str, n_states: usize, cutoff: usize) -> Vec<i32> {
    let mut f = File::open(format!("{}/states.bin", dir)).unwrap();
    let mut hdr = [0u8; 8]; f.read_exact(&mut hdr).unwrap();
    let mut buf = vec![0u8; n_states * cutoff * 4]; f.read_exact(&mut buf).unwrap();
    (0..n_states * cutoff).map(|i| {
        let off = i * 4;
        i32::from_le_bytes([buf[off], buf[off+1], buf[off+2], buf[off+3]])
    }).collect()
}


#[inline(always)]
fn safe_q(queens: &[i32; MAX_N], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                              { return false; }
        if c - r as i32 == col - row as i32      { return false; }
        if c + r as i32 == col + row as i32      { return false; }
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
    let raw_args: Vec<String> = env::args().collect();
    if raw_args.len() < 3 { eprintln!("usage: {} W DATA_DIR", raw_args[0]); std::process::exit(2); }
    let workers: usize = raw_args[1].parse().unwrap();
    let dir = raw_args[2].clone();

    let (n, cutoff, n_states) = load_config(&dir);
    let states = load_states(&dir, n_states, cutoff);
    let states_arc = Arc::new(states);
    let total = Arc::new(AtomicU64::new(0));

    let t0 = Instant::now();
    let cfg = timely::Config::process(workers);
    let states_for_exec = states_arc.clone();
    let total_for_exec = total.clone();
    timely::execute(cfg, move |worker| {
        let widx = worker.index();
        let wcount = worker.peers();
        let states_local = states_for_exec.clone();
        let total_local = total_for_exec.clone();

        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();
        worker.dataflow(|scope| {
            input.to_stream(scope)
                .exchange(|&idx: &usize| idx as u64)
                .inspect(move |&idx| {
                    let mut queens = [0i32; MAX_N];
                    for r in 0..cutoff { queens[r] = states_local[idx * cutoff + r]; }
                    let count = solve_sub(&mut queens, cutoff, n);
                    total_local.fetch_add(count, Ordering::Relaxed);
                })
                .probe_with(&mut probe);
        });

        if widx == 0 {
            for i in 0..n_states { input.send(i); }
        }
        input.advance_to(1);
        while probe.less_than(input.time()) { worker.step(); }
        let _ = wcount;
    }).unwrap();
    let secs = t0.elapsed().as_secs_f64();
    println!("CHECKSUM={}", total.load(Ordering::Relaxed));
    println!("RUNTIME_SEC={}", secs);
}
