//! Cascading Inference Pipeline sequential baseline in Rust — mirrors
//! cip_seq.c byte-for-byte in algorithm.  Uses raw `*const u8` / `*const f64`
//! pointers and `unsafe` on every inner loop so that the denominator pays
//! the same per-access cost as the parallel Rust variants (ribault_rust /
//! cip_timely / sucuri), which all run pointer arithmetic with no bounds
//! checking.  Same fairness principle as ms_seq.rs / sc_seq.rs.

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

const D:        usize = 256;
const B2_SLOTS: usize = 256;
const E_DIM:    usize = 64;
const K3:       usize = 8;
const H_DIM:    usize = 128;
const C_CLS:    usize = 16;
const K1:       usize = 1024;

const ACCEPT_S1:      i32 = 1;
const REJECT_S2:      i32 = 2;
const ACCEPT_S3_BASE: i32 = 0x40;
const CLASS_BASE:     i32 = 0x80;


struct Config { n: usize, _chunk_size: usize, t2: i32, t3: f64 }

fn load_config(dir: &str) -> Config {
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
    Config { n, _chunk_size: cs, t2, t3 }
}


// Owning struct for the weights; raw pointers handed out via .as_ptr().
struct Weights {
    accept_table:   Vec<u32>,
    reject_weights: Vec<i16>,
    ref_vec:        Vec<f64>,
    w1_mat:         Vec<f64>,
    b1_vec:         Vec<f64>,
    w2_mat:         Vec<f64>,
    b2_vec:         Vec<f64>,
    cos_table:      Vec<f64>,
}

fn load_weights(dir: &str) -> Weights {
    let mut f = File::open(format!("{}/weights.bin", dir)).unwrap();
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();
    let mut off = 0usize;

    fn take_u32(buf: &[u8], off: &mut usize, n: usize) -> Vec<u32> {
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let p = *off + i*4;
            v.push(u32::from_le_bytes([buf[p],buf[p+1],buf[p+2],buf[p+3]]));
        }
        *off += n * 4; v
    }
    fn take_i16(buf: &[u8], off: &mut usize, n: usize) -> Vec<i16> {
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let p = *off + i*2;
            v.push(i16::from_le_bytes([buf[p],buf[p+1]]));
        }
        *off += n * 2; v
    }
    fn take_f64(buf: &[u8], off: &mut usize, n: usize) -> Vec<f64> {
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let p = *off + i*8;
            v.push(f64::from_le_bytes([buf[p],buf[p+1],buf[p+2],buf[p+3],
                                       buf[p+4],buf[p+5],buf[p+6],buf[p+7]]));
        }
        *off += n * 8; v
    }
    let accept_table   = take_u32(&buf, &mut off, K1);
    let reject_weights = take_i16(&buf, &mut off, B2_SLOTS);
    let ref_vec        = take_f64(&buf, &mut off, K3 * E_DIM);
    let w1_mat         = take_f64(&buf, &mut off, H_DIM * E_DIM);
    let b1_vec         = take_f64(&buf, &mut off, H_DIM);
    let w2_mat         = take_f64(&buf, &mut off, C_CLS * H_DIM);
    let b2_vec         = take_f64(&buf, &mut off, C_CLS);
    let cos_table      = take_f64(&buf, &mut off, E_DIM * D);
    Weights { accept_table, reject_weights, ref_vec, w1_mat, b1_vec, w2_mat, b2_vec, cos_table }
}


unsafe fn stage1_decide(it: *const u8, accept_table: *const u32) -> bool {
    let mut sig: u32 = 0;
    for i in 0..D { sig = sig.wrapping_add(*it.add(i) as u32); }
    sig &= 0xFFFF;
    let slot = (sig & 0x3FF) as usize;
    *accept_table.add(slot) == sig
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


unsafe fn decide_item(it: *const u8, w: &Weights, t2: i32, t3: f64) -> i32 {
    if stage1_decide(it, w.accept_table.as_ptr()) { return ACCEPT_S1; }
    let score = stage2_score(it, w.reject_weights.as_ptr());
    if score > t2 { return REJECT_S2; }
    let mut emb = [0.0f64; E_DIM];
    stage3_embed(it, w.cos_table.as_ptr(), emb.as_mut_ptr());
    let (best, best_sim) = stage3_best(emb.as_ptr(), w.ref_vec.as_ptr());
    if best_sim > t3 { return ACCEPT_S3_BASE | best as i32; }
    let cls = stage4_classify(emb.as_ptr(),
                              w.w1_mat.as_ptr(), w.b1_vec.as_ptr(),
                              w.w2_mat.as_ptr(), w.b2_vec.as_ptr());
    CLASS_BASE | cls as i32
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 { eprintln!("usage: {} DATA_DIR", args[0]); std::process::exit(2); }
    let dir = &args[1];
    let cfg = load_config(dir);
    let w = load_weights(dir);

    let mut items_buf = vec![0u8; cfg.n * D];
    File::open(format!("{}/input.bin", dir)).unwrap().read_exact(&mut items_buf).unwrap();

    let t0 = Instant::now();
    let mut cs: u32 = 0;
    unsafe {
        let base = items_buf.as_ptr();
        for idx in 0..cfg.n {
            let it = base.add(idx * D);
            let d = decide_item(it, &w, cfg.t2, cfg.t3);
            cs = cs.wrapping_add(d as u32);
        }
    }
    let secs = t0.elapsed().as_secs_f64();
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
