//! Sparse Cholesky on Timely Dataflow 0.13. One epoch per DAG level — each
//! worker pulls ops assigned to it for the current level, executes them
//! against a shared raw-pointer view of the matrix, then advances the
//! frontier (barrier). Same inner kernels (POTRF / TRSM / SYRK / GEMM) as
//! sc_seq.rs — bit-identical checksum.

use std::env;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;
use std::time::Instant;
use timely::dataflow::operators::{Input, Inspect, Probe};
use timely::dataflow::ProbeHandle;

#[derive(Clone, Copy)]
struct SendPtr(*mut f64);
unsafe impl Send for SendPtr {}
unsafe impl Sync for SendPtr {}

#[inline(always)]
fn block_idx(i: usize, j: usize) -> usize { i * (i + 1) / 2 + j }

#[derive(Clone, Copy)]
struct Op { kind: i32, ti: u32, tj: u32, s1i: u32, s1j: u32, s2i: u32, s2j: u32, level: u32 }

unsafe fn potrf_block(d: *mut f64, b: usize) {
    for j in 0..b {
        let mut s = *d.add(j*b + j);
        for kk in 0..j { let x = *d.add(j*b + kk); s -= x * x; }
        let sqrt_s = s.sqrt();
        *d.add(j*b + j) = sqrt_s;
        let inv = 1.0 / sqrt_s;
        for i in (j+1)..b {
            let mut t = *d.add(i*b + j);
            for kk in 0..j { t -= *d.add(i*b + kk) * *d.add(j*b + kk); }
            *d.add(i*b + j) = t * inv;
        }
    }
    for i in 0..b { for j in (i+1)..b { *d.add(i*b + j) = 0.0; } }
}
unsafe fn trsm_block(x: *mut f64, l: *const f64, b: usize) {
    for i in 0..b { for j in 0..b {
        let mut s = *x.add(i*b + j);
        for kk in 0..j { s -= *x.add(i*b + kk) * *l.add(j*b + kk); }
        *x.add(i*b + j) = s / *l.add(j*b + j);
    }}
}
unsafe fn syrk_block(c: *mut f64, a: *const f64, b: usize) {
    for i in 0..b { for j in 0..=i {
        let mut s = 0.0;
        for kk in 0..b { s += *a.add(i*b + kk) * *a.add(j*b + kk); }
        *c.add(i*b + j) -= s;
    }}
}
unsafe fn gemm_block(c: *mut f64, a: *const f64, b_: *const f64, b: usize) {
    for i in 0..b { for j in 0..b {
        let mut s = 0.0;
        for kk in 0..b { s += *a.add(i*b + kk) * *b_.add(j*b + kk); }
        *c.add(i*b + j) -= s;
    }}
}

fn load_config(dir: &str) -> (usize, usize) {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut nb = 0usize; let mut b = 64usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key { "NB" => nb = val, "B" => b = val, _ => {} }
    }
    (nb, b)
}

fn load_ops(dir: &str) -> Vec<Op> {
    let mut df = File::open(format!("{}/dag.bin", dir)).unwrap();
    let mut hdr4 = [0u8; 4];
    df.read_exact(&mut hdr4).unwrap();
    let n_ops = i32::from_le_bytes(hdr4) as usize;
    let mut ops = Vec::with_capacity(n_ops);
    for _ in 0..n_ops {
        let mut hdr = [0u8; 36];
        df.read_exact(&mut hdr).unwrap();
        let vals: [i32; 9] = std::array::from_fn(|i|
            i32::from_le_bytes([hdr[i*4], hdr[i*4+1], hdr[i*4+2], hdr[i*4+3]]));
        let n_deps = vals[8] as usize;
        if n_deps > 0 {
            let mut skip = vec![0u8; n_deps * 4];
            df.read_exact(&mut skip).unwrap();
        }
        ops.push(Op {
            kind: vals[0],
            ti: vals[1] as u32, tj: vals[2] as u32,
            s1i: vals[3].max(0) as u32, s1j: vals[4].max(0) as u32,
            s2i: vals[5].max(0) as u32, s2j: vals[6].max(0) as u32,
            level: vals[7] as u32,
        });
    }
    ops
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 { eprintln!("usage: {} W DATA_DIR", args[0]); std::process::exit(2); }
    let workers: usize = args[1].parse().unwrap();
    let dir = args[2].clone();

    let (nb, b) = load_config(&dir);
    let n_blocks = nb * (nb + 1) / 2;
    let block_size = b * b;

    // Mat: load once, leak the buffer so workers share a raw pointer with 'static lifetime.
    let path = format!("{}/A.bin", dir);
    let mut f = File::open(&path).unwrap();
    let mut buf = vec![0u8; n_blocks * block_size * 8];
    f.read_exact(&mut buf).unwrap();
    let a_vec: Vec<f64> = (0..n_blocks * block_size).map(|i| {
        let off = i * 8;
        f64::from_le_bytes([buf[off], buf[off+1], buf[off+2], buf[off+3],
                            buf[off+4], buf[off+5], buf[off+6], buf[off+7]])
    }).collect();
    let leaked: &'static mut [f64] = Box::leak(a_vec.into_boxed_slice());
    let a_ptr_send = SendPtr(leaked.as_mut_ptr());
    // Keep a non-Send copy for the post-run checksum (re-fetched via leaked).

    let ops = load_ops(&dir);
    let max_level = ops.iter().map(|o| o.level).max().unwrap_or(0);
    let ops_arc = Arc::new(ops);

    let t0 = Instant::now();
    let cfg = timely::Config::process(workers);
    timely::execute(cfg, move |worker| {
        let widx = worker.index();
        let wcount = worker.peers();
        let a_ptr_w = a_ptr_send.0;
        let ops_local = ops_arc.clone();

        let mut input = timely::dataflow::InputHandle::new();
        let mut probe = ProbeHandle::new();
        worker.dataflow(|scope| {
            input.to_stream(scope)
                .inspect(move |&op_idx: &usize| {
                    let op = ops_local[op_idx];
                    let tgt = unsafe { a_ptr_w.add(block_idx(op.ti as usize, op.tj as usize) * block_size) };
                    unsafe {
                        match op.kind {
                            0 => potrf_block(tgt, b),
                            1 => trsm_block(tgt, a_ptr_w.add(block_idx(op.s1i as usize, op.s1j as usize) * block_size), b),
                            2 => syrk_block(tgt, a_ptr_w.add(block_idx(op.s1i as usize, op.s1j as usize) * block_size), b),
                            3 => gemm_block(tgt,
                                            a_ptr_w.add(block_idx(op.s1i as usize, op.s1j as usize) * block_size),
                                            a_ptr_w.add(block_idx(op.s2i as usize, op.s2j as usize) * block_size), b),
                            _ => panic!("unknown op kind"),
                        }
                    }
                })
                .probe_with(&mut probe);
        });

        // One epoch per DAG level — barrier between levels.
        for lvl in 1..=max_level {
            for (i, op) in ops_arc.iter().enumerate() {
                if op.level == lvl && (i % wcount == widx) {
                    input.send(i);
                }
            }
            input.advance_to((lvl + 1) as usize);
            while probe.less_than(input.time()) { worker.step(); }
        }
    }).unwrap();
    let secs = t0.elapsed().as_secs_f64();

    // Checksum over leaked slice
    let mut cs: u64 = 0;
    for &v in leaked.iter() {
        let fixed = (v * 1e6) as i64;
        cs = (cs + (fixed as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
