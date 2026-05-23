//! LCS sequential baseline in Rust — monolithic 2-row DP, identical to
//! the C and Haskell sequential baselines. Uses raw pointers + unsafe
//! to skip bounds checking on the inner loop, matching the per-block
//! Rust style used by the Ribault-Rust and Timely leaf compute. Same
//! LCG (seed=42, alpha=4) as every other variant.

use std::env;
use std::time::Instant;

const SEED: u64 = 42;
const ALPHA: u64 = 4;

#[inline(always)]
fn lcg_next(r: u64) -> u64 {
    (6364136223846793005_u64
        .wrapping_mul(r)
        .wrapping_add(1442695040888963407_u64))
        & 0x7FFFFFFFFFFFFFFF_u64
}

fn gen_seq(mut rng: u64, arr: &mut [i32]) -> u64 {
    for i in 0..arr.len() {
        rng = lcg_next(rng);
        arr[i] = ((rng >> 33) % ALPHA) as i32;
    }
    rng
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} N", args[0]);
        std::process::exit(2);
    }
    let n: usize = args[1].parse().expect("bad N");

    let mut sa = vec![0_i32; n];
    let mut sb = vec![0_i32; n];

    let rng = SEED;
    let rng = gen_seq(rng, &mut sa);
    let _ = gen_seq(rng, &mut sb);

    let mut prev: Vec<i32> = vec![0; n + 1];
    let mut cur: Vec<i32> = vec![0; n + 1];

    let t0 = Instant::now();

    unsafe {
        let sa_p: *const i32 = sa.as_ptr();
        let sb_p: *const i32 = sb.as_ptr();
        let mut prev_p: *mut i32 = prev.as_mut_ptr();
        let mut cur_p: *mut i32 = cur.as_mut_ptr();

        for i in 1..=n {
            *cur_p.add(0) = 0;
            let ai = *sa_p.add(i - 1);
            for j in 1..=n {
                let bj = *sb_p.add(j - 1);
                if ai == bj {
                    *cur_p.add(j) = *prev_p.add(j - 1) + 1;
                } else {
                    let u = *prev_p.add(j);
                    let l = *cur_p.add(j - 1);
                    *cur_p.add(j) = if u > l { u } else { l };
                }
            }
            // swap prev <-> cur
            let tmp = prev_p;
            prev_p = cur_p;
            cur_p = tmp;
        }

        let elapsed = t0.elapsed();
        let result = *prev_p.add(n);
        println!("RESULT={}", result);
        println!("RUNTIME_SEC={}", elapsed.as_secs_f64());
    }
}
