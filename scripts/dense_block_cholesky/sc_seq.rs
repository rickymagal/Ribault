//! Dense block Cholesky sequential baseline in Rust — raw `*mut f64` pointers,
//! `unsafe` inner loops, matches sc_seq.c algorithm byte-for-byte.

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

fn load_config(dir: &str) -> (usize, usize) {
    let path = format!("{}/config.txt", dir);
    let text = std::fs::read_to_string(&path).unwrap();
    let mut nb = 0usize; let mut b = 64usize;
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key { "NB" => nb = val, "B" => b = val, _ => {} }
    }
    (nb, b)
}

#[inline(always)] fn block_idx(i: usize, j: usize) -> usize { i * (i + 1) / 2 + j }

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
    // Zero upper triangle
    for i in 0..b { for j in (i+1)..b { *d.add(i*b + j) = 0.0; } }
}

unsafe fn trsm_block(x: *mut f64, l: *const f64, b: usize) {
    for i in 0..b {
        for j in 0..b {
            let mut s = *x.add(i*b + j);
            for kk in 0..j { s -= *x.add(i*b + kk) * *l.add(j*b + kk); }
            *x.add(i*b + j) = s / *l.add(j*b + j);
        }
    }
}

unsafe fn syrk_block(c: *mut f64, a: *const f64, b: usize) {
    for i in 0..b {
        for j in 0..=i {
            let mut s = 0.0;
            for kk in 0..b { s += *a.add(i*b + kk) * *a.add(j*b + kk); }
            *c.add(i*b + j) -= s;
        }
    }
}

unsafe fn gemm_block(c: *mut f64, a: *const f64, b_: *const f64, b: usize) {
    for i in 0..b {
        for j in 0..b {
            let mut s = 0.0;
            for kk in 0..b { s += *a.add(i*b + kk) * *b_.add(j*b + kk); }
            *c.add(i*b + j) -= s;
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 { eprintln!("usage: {} DATA_DIR", args[0]); std::process::exit(2); }
    let dir = &args[1];
    let (nb, b) = load_config(dir);
    let n_blocks = nb * (nb + 1) / 2;
    let block_size = b * b;

    // Load A.bin
    let path = format!("{}/A.bin", dir);
    let mut f = File::open(&path).unwrap();
    let mut buf = vec![0u8; n_blocks * block_size * 8];
    f.read_exact(&mut buf).unwrap();
    let mut a: Vec<f64> = (0..n_blocks * block_size).map(|i| {
        let off = i * 8;
        f64::from_le_bytes([buf[off], buf[off+1], buf[off+2], buf[off+3],
                            buf[off+4], buf[off+5], buf[off+6], buf[off+7]])
    }).collect();

    // Load DAG
    let dag_path = format!("{}/dag.bin", dir);
    let mut df = File::open(&dag_path).unwrap();
    let mut hdr4 = [0u8; 4];
    df.read_exact(&mut hdr4).unwrap();
    let n_ops = i32::from_le_bytes(hdr4) as usize;

    struct Op { kind: i32, ti: usize, tj: usize, s1i: usize, s1j: usize, s2i: usize, s2j: usize }
    let mut ops: Vec<Op> = Vec::with_capacity(n_ops);
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
            kind: vals[0], ti: vals[1] as usize, tj: vals[2] as usize,
            s1i: vals[3].max(0) as usize, s1j: vals[4].max(0) as usize,
            s2i: vals[5].max(0) as usize, s2j: vals[6].max(0) as usize,
        });
    }

    let t0 = Instant::now();
    unsafe {
        let a_ptr = a.as_mut_ptr();
        for op in &ops {
            let tgt = a_ptr.add(block_idx(op.ti, op.tj) * block_size);
            match op.kind {
                0 => potrf_block(tgt, b),
                1 => trsm_block(tgt, a_ptr.add(block_idx(op.s1i, op.s1j) * block_size), b),
                2 => syrk_block(tgt, a_ptr.add(block_idx(op.s1i, op.s1j) * block_size), b),
                3 => gemm_block(tgt,
                                a_ptr.add(block_idx(op.s1i, op.s1j) * block_size),
                                a_ptr.add(block_idx(op.s2i, op.s2j) * block_size), b),
                _ => panic!("unknown op kind"),
            }
        }
    }
    let secs = t0.elapsed().as_secs_f64();

    // Checksum
    let mut cs: u64 = 0;
    for v in &a {
        let fixed = (*v * 1e6) as i64;
        cs = (cs + (fixed as u32 as u64)) & 0xFFFFFFFF_u64;
    }
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);
}
