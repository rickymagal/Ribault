//! Cascading Inference Pipeline — Timely Dataflow variant.
//!
//! Idiom: one epoch per stage (1..=4), exchange by chunk_id within an
//! epoch.  Barrier between epochs.  This is the natural Timely shape
//! for a DAG with a level structure — Timely has no native way to
//! express per-chunk pipelining across stages without giving up the
//! epoch/probe synchronization model.  This is the central
//! architectural argument we make against Timely in the paper.
//!
//! Inner kernels: identical to cip_seq.rs (raw `*const u8` / `*const f64`).

use std::env;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;
use std::time::Instant;
use timely::dataflow::operators::{Input, Exchange, Inspect, Probe};
use timely::dataflow::{InputHandle, ProbeHandle};

const D:        usize = 256;
const B2_SLOTS: usize = 256;
const E_DIM:    usize = 64;
const K3:       usize = 8;
const H_DIM:    usize = 128;
const C_CLS:    usize = 16;
const ACCEPT_BITMAP_BYTES: usize = 8192;

const ACCEPT_S1:      i32 = 1;
const REJECT_S2:      i32 = 2;
const ACCEPT_S3_BASE: i32 = 0x40;
const CLASS_BASE:     i32 = 0x80;


// SendPtr wrapper — same idiom as sc_timely.rs / ms_timely.rs.
#[derive(Clone, Copy)]
struct SendPtr<T>(*mut T);
unsafe impl<T> Send for SendPtr<T> {}
unsafe impl<T> Sync for SendPtr<T> {}
#[derive(Clone, Copy)]
struct ConstPtr<T>(*const T);
unsafe impl<T> Send for ConstPtr<T> {}
unsafe impl<T> Sync for ConstPtr<T> {}


fn load_config(dir: &str) -> (usize, usize, i32, f64) {
    let text = std::fs::read_to_string(format!("{}/config.txt", dir)).unwrap();
    let mut n = 0usize; let mut cs = 0usize; let mut t2 = 0i32; let mut t3 = 0.0f64;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val = it.next().unwrap_or("0");
        match key {
            "N"          => n  = val.parse().unwrap_or(0),
            "CHUNK_SIZE" => cs = val.parse().unwrap_or(512),
            "T2"         => t2 = val.parse().unwrap_or(0),
            "T3"         => t3 = val.parse().unwrap_or(0.0),
            _ => {}
        }
    }
    (n, cs, t2, t3)
}

struct Weights {
    accept_bitmap:  Vec<u8>,
    reject_weights: Vec<i16>,
    ref_vec:        Vec<f64>,
    w1_mat:         Vec<f64>,
    b1_vec:         Vec<f64>,
    w2_mat:         Vec<f64>,
    b2_vec:         Vec<f64>,
    cos_table:      Vec<f64>,
}

fn load_weights(dir: &str) -> Weights {
    let mut buf = Vec::new();
    File::open(format!("{}/weights.bin", dir)).unwrap().read_to_end(&mut buf).unwrap();
    let mut off = 0usize;
    fn u8_block(buf: &[u8], off: &mut usize, n: usize) -> Vec<u8> {
        let v = buf[*off..*off + n].to_vec();
        *off += n; v
    }
    fn i16_block(buf: &[u8], off: &mut usize, n: usize) -> Vec<i16> {
        let mut v = Vec::with_capacity(n);
        for i in 0..n { let p = *off + i*2;
            v.push(i16::from_le_bytes([buf[p],buf[p+1]])); }
        *off += n*2; v
    }
    fn f64_block(buf: &[u8], off: &mut usize, n: usize) -> Vec<f64> {
        let mut v = Vec::with_capacity(n);
        for i in 0..n { let p = *off + i*8;
            v.push(f64::from_le_bytes([buf[p],buf[p+1],buf[p+2],buf[p+3],
                                       buf[p+4],buf[p+5],buf[p+6],buf[p+7]])); }
        *off += n*8; v
    }
    Weights {
        accept_bitmap:  u8_block(&buf, &mut off, ACCEPT_BITMAP_BYTES),
        reject_weights: i16_block(&buf, &mut off, B2_SLOTS),
        ref_vec:        f64_block(&buf, &mut off, K3 * E_DIM),
        w1_mat:         f64_block(&buf, &mut off, H_DIM * E_DIM),
        b1_vec:         f64_block(&buf, &mut off, H_DIM),
        w2_mat:         f64_block(&buf, &mut off, C_CLS * H_DIM),
        b2_vec:         f64_block(&buf, &mut off, C_CLS),
        cos_table:      f64_block(&buf, &mut off, E_DIM * D),
    }
}


// Stage kernels — identical to cip_seq.rs.
unsafe fn stage1_decide(it: *const u8, accept_bitmap: *const u8) -> bool {
    let mut sig: u32 = 0;
    for i in 0..D { sig = sig.wrapping_add(*it.add(i) as u32); }
    sig &= 0xFFFF;
    (*accept_bitmap.add((sig >> 3) as usize) >> (sig & 7)) & 1 != 0
}
unsafe fn stage2_score(it: *const u8, reject_weights: *const i16) -> i32 {
    let mut hist = [0i32; B2_SLOTS];
    for i in 0..(D - 1) {
        let b = ((*it.add(i) as i32) * 7 + *it.add(i + 1) as i32) & 0xFF;
        hist[b as usize] += 1;
    }
    let mut s: i32 = 0;
    for k in 0..B2_SLOTS { s += hist[k] * (*reject_weights.add(k) as i32); }
    s
}
unsafe fn stage3_embed(it: *const u8, cos_table: *const f64, emb: *mut f64) {
    for j in 0..E_DIM {
        let mut s = 0.0f64;
        let row = cos_table.add(j * D);
        for i in 0..D { s += *row.add(i) * (*it.add(i) as f64 / 255.0); }
        *emb.add(j) = s;
    }
    let mut n2 = 0.0f64;
    for j in 0..E_DIM { let v = *emb.add(j); n2 += v * v; }
    if n2 > 0.0 {
        let inv = 1.0 / n2.sqrt();
        for j in 0..E_DIM { *emb.add(j) *= inv; }
    }
}
unsafe fn stage3_best(emb: *const f64, ref_vec: *const f64) -> (usize, f64) {
    let mut best = 0usize; let mut bs = -1e300f64;
    for k in 0..K3 {
        let r = ref_vec.add(k * E_DIM);
        let mut s = 0.0f64;
        for j in 0..E_DIM { s += *emb.add(j) * *r.add(j); }
        if s > bs { bs = s; best = k; }
    }
    (best, bs)
}
unsafe fn stage4_classify(emb: *const f64,
                          w1: *const f64, b1: *const f64,
                          w2: *const f64, b2: *const f64) -> usize {
    let mut hidden = [0.0f64; H_DIM];
    for h in 0..H_DIM {
        let mut s = *b1.add(h);
        let row = w1.add(h * E_DIM);
        for j in 0..E_DIM { s += *row.add(j) * *emb.add(j); }
        hidden[h] = if s > 0.0 { s } else { 0.0 };
    }
    let mut best = 0usize; let mut bs = -1e300f64;
    for c in 0..C_CLS {
        let mut s = *b2.add(c);
        let row = w2.add(c * H_DIM);
        for h in 0..H_DIM { s += *row.add(h) * hidden[h]; }
        if s > bs { bs = s; best = c; }
    }
    best
}


fn main() {
    let raw_args: Vec<String> = env::args().collect();
    if raw_args.len() < 3 { eprintln!("usage: {} W DATA_DIR", raw_args[0]); std::process::exit(2); }
    let workers: usize = raw_args[1].parse().unwrap();
    let dir = raw_args[2].clone();

    let (n, chunk_size, t2, t3) = load_config(&dir);
    let w = load_weights(&dir);
    let n_chunks = (n + chunk_size - 1) / chunk_size;

    // Load items into a leaked, raw-pointer-shareable buffer.
    let mut items_buf = vec![0u8; n * D];
    File::open(format!("{}/input.bin", dir)).unwrap().read_exact(&mut items_buf).unwrap();
    let leaked_items: &'static mut [u8] = Box::leak(items_buf.into_boxed_slice());
    let items_ptr = ConstPtr(leaked_items.as_ptr());

    // Per-item state arrays, all leaked-and-shared.
    let leaked_decis: &'static mut [i32] =
        Box::leak(vec![0i32; n].into_boxed_slice());
    let decis_ptr = SendPtr(leaked_decis.as_mut_ptr());
    let leaked_emb: &'static mut [f64] =
        Box::leak(vec![0.0f64; n * E_DIM].into_boxed_slice());
    let emb_ptr = SendPtr(leaked_emb.as_mut_ptr());

    // Wrap weights' pointers in ConstPtr (Send + Sync).
    let w_accept = ConstPtr(w.accept_bitmap.as_ptr());
    let w_reject = ConstPtr(w.reject_weights.as_ptr());
    let w_ref    = ConstPtr(w.ref_vec.as_ptr());
    let w_w1     = ConstPtr(w.w1_mat.as_ptr());
    let w_b1     = ConstPtr(w.b1_vec.as_ptr());
    let w_w2     = ConstPtr(w.w2_mat.as_ptr());
    let w_b2     = ConstPtr(w.b2_vec.as_ptr());
    let w_cos    = ConstPtr(w.cos_table.as_ptr());
    let _w_holder = Arc::new(w);  // keep weights alive

    let t0 = Instant::now();
    let cfg = timely::Config::process(workers);
    timely::execute(cfg, move |worker| {
        let widx = worker.index();
        let wcount = worker.peers();

        // Per-worker copies of the Send-able pointer wrappers (inner
        // closure captures by Copy, keeps !Sync raw ptr out of the
        // outer closure's type).
        let items_p = items_ptr;
        let decis_p = decis_ptr;
        let emb_p   = emb_ptr;
        let w_accept_p = w_accept;
        let w_reject_p = w_reject;
        let w_ref_p    = w_ref;
        let w_w1_p     = w_w1;
        let w_b1_p     = w_b1;
        let w_w2_p     = w_w2;
        let w_b2_p     = w_b2;
        let w_cos_p    = w_cos;

        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();
        worker.dataflow(|scope| {
            input.to_stream(scope)
                .exchange(|&(_stage, chunk): &(u32, u32)| chunk as u64)
                .inspect(move |&(stage, chunk_id)| {
                    let chunk_id = chunk_id as usize;
                    let lo = chunk_id * chunk_size;
                    let hi = std::cmp::min(lo + chunk_size, n);
                    unsafe {
                        match stage {
                            1 => {
                                for i in lo..hi {
                                    let it = items_p.0.add(i * D);
                                    if stage1_decide(it, w_accept_p.0) {
                                        *decis_p.0.add(i) = ACCEPT_S1;
                                    }
                                }
                            }
                            2 => {
                                for i in lo..hi {
                                    if *decis_p.0.add(i) != 0 { continue; }
                                    let it = items_p.0.add(i * D);
                                    let s = stage2_score(it, w_reject_p.0);
                                    if s > t2 { *decis_p.0.add(i) = REJECT_S2; }
                                }
                            }
                            3 => {
                                for i in lo..hi {
                                    if *decis_p.0.add(i) != 0 { continue; }
                                    let it  = items_p.0.add(i * D);
                                    let emb = emb_p.0.add(i * E_DIM);
                                    stage3_embed(it, w_cos_p.0, emb);
                                    let (best, bs) = stage3_best(emb, w_ref_p.0);
                                    if bs > t3 {
                                        *decis_p.0.add(i) = ACCEPT_S3_BASE | best as i32;
                                    }
                                }
                            }
                            4 => {
                                for i in lo..hi {
                                    if *decis_p.0.add(i) != 0 { continue; }
                                    let emb = emb_p.0.add(i * E_DIM);
                                    let cls = stage4_classify(emb,
                                                              w_w1_p.0, w_b1_p.0,
                                                              w_w2_p.0, w_b2_p.0);
                                    *decis_p.0.add(i) = CLASS_BASE | cls as i32;
                                }
                            }
                            _ => panic!("unknown stage {}", stage),
                        }
                    }
                })
                .probe_with(&mut probe);
        });

        // One epoch per stage.
        for stage in 1u32..=4u32 {
            for k in 0..n_chunks {
                if (k % wcount) == widx { input.send((stage, k as u32)); }
            }
            input.advance_to(stage as usize + 1);
            while probe.less_than(input.time()) { worker.step(); }
        }
    }).unwrap();
    let secs = t0.elapsed().as_secs_f64();

    let mut cs: u32 = 0;
    for &d in leaked_decis.iter() { cs = cs.wrapping_add(d as u32); }
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
