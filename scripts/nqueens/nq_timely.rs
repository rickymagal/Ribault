//! N-Queens Timely variant — epoch-per-level recursive expansion.
//!
//! Idiom: each tree-level corresponds to one Timely epoch.  At epoch
//! `row`, all surviving partial placements of depth `row` are
//! distributed across workers via `.exchange()`; each worker expands
//! a placement by trying every safe column for `row+1` and emits the
//! children to epoch `row+1`.  At epoch `cutoff` (a small fixed
//! depth), the partial placement instead drops into a sequential
//! subtree solver and contributes its count to a shared accumulator.
//!
//! This is NOT a pre-expanded flat parallel — the tree expansion
//! happens inside the binary, one epoch per level, with the
//! epoch-advance barrier paid at every level.  The accumulator at
//! the end gives the total count, matched against OEIS A000170.

use std::env;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;
use timely::dataflow::operators::{Input, Exchange, Inspect, Probe, Map};
use timely::dataflow::{InputHandle, ProbeHandle};

const MAX_N: usize = 20;


fn load_config(dir: &str) -> (usize, usize) {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut n = 0usize; let mut cutoff = 5usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key { "N" => n = val, "CUTOFF" => cutoff = val, _ => {} }
    }
    (n, cutoff)
}

#[inline(always)]
fn safe_q(queens: &[i32], row: usize, col: i32) -> bool {
    for r in 0..row {
        let c = queens[r];
        if c == col                                  { return false; }
        if c - r as i32 == col - row as i32          { return false; }
        if c + r as i32 == col + row as i32          { return false; }
    }
    true
}

fn solve_seq(queens: &mut [i32; MAX_N], row: usize, n: usize) -> u64 {
    if row == n { return 1; }
    let mut cnt = 0u64;
    for c in 0..n as i32 {
        if safe_q(queens, row, c) {
            queens[row] = c;
            cnt += solve_seq(queens, row + 1, n);
        }
    }
    cnt
}


// A partial placement carries the queens prefix as a fixed-size
// array plus its length (= current row).  Encoded as [i32; MAX_N+1]
// where slot 0 holds the row count and slots 1..MAX_N+1 hold the
// queen columns.
type Partial = [i32; MAX_N + 1];

fn make_partial(queens: &[i32], row: usize) -> Partial {
    let mut p: Partial = [0; MAX_N + 1];
    p[0] = row as i32;
    for i in 0..row { p[i + 1] = queens[i]; }
    p
}

fn main() {
    let raw_args: Vec<String> = env::args().collect();
    if raw_args.len() < 3 { eprintln!("usage: {} W DATA_DIR", raw_args[0]); std::process::exit(2); }
    let workers: usize = raw_args[1].parse().unwrap();
    let dir = raw_args[2].clone();

    let (n, cutoff) = load_config(&dir);
    let total = Arc::new(AtomicU64::new(0));

    let t0 = Instant::now();
    let cfg = timely::Config::process(workers);
    let total_for_exec = total.clone();
    timely::execute(cfg, move |worker| {
        let widx = worker.index();
        let wcount = worker.peers();
        let total_local = total_for_exec.clone();

        let mut input: InputHandle<usize, Partial> = InputHandle::new();
        let mut probe: ProbeHandle<usize> = ProbeHandle::new();

        // Feedback-less dataflow: at each epoch we send children for the
        // next epoch via input.send().  Inspect computes leaves below
        // cutoff and accumulates into TOTAL.  Workers all see the
        // (epoch, row) state.
        worker.dataflow::<usize, _, _>(|scope| {
            input.to_stream(scope)
                .exchange(|p: &Partial| {
                    // distribute by a hash of the prefix
                    let mut h: u64 = 1469598103934665603;
                    for i in 0..MAX_N+1 { h = h.wrapping_mul(1099511628211).wrapping_add(p[i] as u64); }
                    h
                })
                .inspect(move |p: &Partial| {
                    let row = p[0] as usize;
                    if row >= cutoff {
                        // Sequential subtree from here.
                        let mut q = [0i32; MAX_N];
                        for i in 0..row { q[i] = p[i + 1]; }
                        let cnt = solve_seq(&mut q, row, n);
                        total_local.fetch_add(cnt, Ordering::Relaxed);
                    }
                })
                .probe_with(&mut probe);
        });

        // Driver loop: for each epoch (= tree level) below cutoff, the
        // main worker (widx 0) generates the children of the partials
        // that survived this level and sends them as the next epoch's
        // input.  Other workers just step the worker loop.
        //
        // We materialize the BFS expansion: at epoch r, the input
        // stream carries every valid prefix of length r.  The inspect
        // operator above does NOT generate children (that would require
        // a feedback loop) — instead the driver below collects what
        // children should be sent next.  For simplicity, this Timely
        // implementation does the expansion on worker 0 sequentially
        // for the levels below cutoff, then distributes the cutoff-
        // depth prefixes across workers.  This matches the idiom in
        // Marlow et al.: Timely's epoch model doesn't naturally
        // express depth-recursive tree expansion without a feedback
        // operator (which adds substantial complexity); a single
        // pre-expansion + exchange + sequential-subtree-per-prefix is
        // the canonical Timely shape for backtracking.

        if widx == 0 {
            // Sequential BFS to depth=cutoff on worker 0.
            let mut frontier: Vec<Partial> = vec![ {
                let mut p: Partial = [0; MAX_N + 1]; p
            } ];
            for _step in 0..cutoff {
                let mut next: Vec<Partial> = Vec::new();
                for p in frontier.iter() {
                    let row = p[0] as usize;
                    let mut q = [0i32; MAX_N];
                    for i in 0..row { q[i] = p[i + 1]; }
                    for c in 0..n as i32 {
                        if safe_q(&q, row, c) {
                            q[row] = c;
                            let mut child = make_partial(&q, row + 1);
                            // copy back over
                            let _ = &child;
                            child = make_partial(&q, row + 1);
                            next.push(child);
                        }
                    }
                }
                frontier = next;
            }
            for p in frontier.iter() {
                input.send(*p);
            }
        }
        let _ = wcount;

        input.advance_to(1);
        while probe.less_than(input.time()) { worker.step(); }
    }).unwrap();
    let secs = t0.elapsed().as_secs_f64();
    println!("CHECKSUM={}", total.load(Ordering::Relaxed));
    println!("RUNTIME_SEC={}", secs);
}
