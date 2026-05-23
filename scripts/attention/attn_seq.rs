//! Attention sequential baseline in Rust — monolithic single-thread
//! end-to-end transformer block. Uses raw pointers + `unsafe` on every
//! inner kernel to match the parallel Rust leaf compute used by
//! ribault_rust (gen_attn_rust.py) and timely (gen_attn_rs_timely.py).
//!
//! CRITICAL FAIRNESS NOTE: the parallel Rust leaf kernels run with bounds
//! checking DISABLED (raw `*const f64` / `*mut f64` arithmetic). If
//! seq_rust kept Rust's default bounds checking on the O(N^2) inner loops
//! it would be artificially slow vs the parallel numerator and would
//! inflate the Rust-tier speedups. seq_rust therefore mirrors the leaf
//! compute exactly: `unsafe` block, raw pointers, `.add(..)` arithmetic.
//!
//! Build:   cargo build --release   (with the Cargo.toml in this directory)
//! Run:     attn_seq_rust DATA_DIR
//! Output:  CHECKSUM=...  RUNTIME_SEC=...
//!
//! This is the Rust-tier denominator: ribault_rust, timely, timely_*
//! and sucuri (if/when re-enabled) are all measured against this baseline
//! so the reported speedups isolate the dataflow scheduler from the
//! language compute backend.

use std::env;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

fn read_doubles(dir: &str, name: &str, count: usize) -> Vec<f64> {
    let path = format!("{}/{}", dir, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count * 8];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    let mut out = vec![0.0_f64; count];
    for i in 0..count {
        let mut b = [0u8; 8];
        b.copy_from_slice(&buf[i*8..(i+1)*8]);
        out[i] = f64::from_le_bytes(b);
    }
    out
}

fn read_bytes(dir: &str, name: &str, count: usize) -> Vec<u8> {
    let path = format!("{}/{}", dir, name);
    let mut f = File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut buf = vec![0u8; count];
    f.read_exact(&mut buf).unwrap_or_else(|e| panic!("read {}: {}", path, e));
    buf
}

struct Config {
    n: usize, d: usize, n_heads: usize, head_dim: usize,
    d_ff: usize, vocab: usize,
}

fn load_config(dir: &str) -> Config {
    let path = format!("{}/config.txt", dir);
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("open {}: {}", path, e));
    let mut cfg = Config { n:0, d:0, n_heads:0, head_dim:0, d_ff:0, vocab:0 };
    for line in text.lines() {
        let mut it = line.split_whitespace();
        let key = it.next().unwrap_or("");
        let val: usize = it.next().unwrap_or("0").parse().unwrap_or(0);
        match key {
            "N"        => cfg.n = val,
            "D"        => cfg.d = val,
            "N_HEADS"  => cfg.n_heads = val,
            "HEAD_DIM" => cfg.head_dim = val,
            "D_FF"     => cfg.d_ff = val,
            "VOCAB"    => cfg.vocab = val,
            _ => {}
        }
    }
    cfg
}

unsafe fn matmul(a: *const f64, b: *const f64, c: *mut f64,
                 m_dim: usize, k_dim: usize, n_dim: usize) {
    // c[m_dim, n_dim] = a[m_dim, k_dim] @ b[k_dim, n_dim]
    for m in 0..m_dim {
        let arow = a.add(m * k_dim);
        let crow = c.add(m * n_dim);
        for n in 0..n_dim { *crow.add(n) = 0.0; }
        for k in 0..k_dim {
            let av = *arow.add(k);
            let brow = b.add(k * n_dim);
            for n in 0..n_dim { *crow.add(n) += av * *brow.add(n); }
        }
    }
}

unsafe fn layer_norm(x: *const f64, w: *const f64, b: *const f64,
                     out: *mut f64, rows: usize, dim: usize) {
    let eps = 1e-5;
    for i in 0..rows {
        let xr = x.add(i * dim);
        let or_ = out.add(i * dim);
        let mut mean = 0.0;
        for j in 0..dim { mean += *xr.add(j); }
        mean /= dim as f64;
        let mut var = 0.0;
        for j in 0..dim { let d = *xr.add(j) - mean; var += d * d; }
        var /= dim as f64;
        let inv = 1.0 / (var + eps).sqrt();
        for j in 0..dim { *or_.add(j) = (*xr.add(j) - mean) * inv * *w.add(j) + *b.add(j); }
    }
}

unsafe fn multihead_attention(
    q: *const f64, k_buf: *const f64, v: *const f64,
    attn: *mut f64, n: usize, d: usize, n_heads: usize, head_dim: usize,
) {
    let inv = 1.0 / (head_dim as f64).sqrt();
    let mut scores = vec![0.0_f64; n];
    for i in 0..n {
        let ai = attn.add(i * d);
        for j in 0..d { *ai.add(j) = 0.0; }
    }
    for i in 0..n {
        for h in 0..n_heads {
            let qhi = q.add(i * d + h * head_dim);
            for j in 0..n {
                let khj = k_buf.add(j * d + h * head_dim);
                let mut s = 0.0;
                for kk in 0..head_dim { s += *qhi.add(kk) * *khj.add(kk); }
                scores[j] = s * inv;
            }
            let mut m = scores[0];
            for j in 1..n { if scores[j] > m { m = scores[j]; } }
            let mut sum = 0.0;
            for j in 0..n { scores[j] = (scores[j] - m).exp(); sum += scores[j]; }
            for j in 0..n { scores[j] /= sum; }
            let ahi = attn.add(i * d + h * head_dim);
            for j in 0..n {
                let a = scores[j];
                let vhj = v.add(j * d + h * head_dim);
                for kk in 0..head_dim { *ahi.add(kk) += a * *vhj.add(kk); }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} DATA_DIR", args[0]);
        std::process::exit(2);
    }
    let dir = &args[1];
    let cfg = load_config(dir);
    let (n, d, n_heads, head_dim, d_ff, vocab) =
        (cfg.n, cfg.d, cfg.n_heads, cfg.head_dim, cfg.d_ff, cfg.vocab);

    // Load weights / inputs
    let e            = read_doubles(dir, "E.bin",      vocab * d);
    let pe           = read_doubles(dir, "PE.bin",     n * d);
    let w_q          = read_doubles(dir, "W_Q.bin",    d * d);
    let w_k          = read_doubles(dir, "W_K.bin",    d * d);
    let w_v          = read_doubles(dir, "W_V.bin",    d * d);
    let w_o          = read_doubles(dir, "W_O.bin",    d * d);
    let w_1          = read_doubles(dir, "W_1.bin",    d * d_ff);
    let w_2          = read_doubles(dir, "W_2.bin",    d_ff * d);
    let w_u          = read_doubles(dir, "W_U.bin",    d * vocab);
    let ln1w         = read_doubles(dir, "LN_1_w.bin", d);
    let ln1b         = read_doubles(dir, "LN_1_b.bin", d);
    let ln2w         = read_doubles(dir, "LN_2_w.bin", d);
    let ln2b         = read_doubles(dir, "LN_2_b.bin", d);
    let input_tokens = read_bytes(dir, "input_tokens.bin", n);

    let mut x_buf      = vec![0.0_f64; n * d];
    let mut xa_buf     = vec![0.0_f64; n * d];
    let mut q_buf      = vec![0.0_f64; n * d];
    let mut k_vec      = vec![0.0_f64; n * d];
    let mut v_buf      = vec![0.0_f64; n * d];
    let mut attn_buf   = vec![0.0_f64; n * d];
    let mut xb_buf     = vec![0.0_f64; n * d];
    let mut ffn_h      = vec![0.0_f64; n * d_ff];
    let mut logits_buf = vec![0.0_f64; n * vocab];
    let mut output_tokens = vec![0_u8; n];

    let t0 = Instant::now();

    unsafe {
        let ep   = e.as_ptr();
        let pep  = pe.as_ptr();
        let wqp  = w_q.as_ptr();
        let wkp  = w_k.as_ptr();
        let wvp  = w_v.as_ptr();
        let wop  = w_o.as_ptr();
        let w1p  = w_1.as_ptr();
        let w2p  = w_2.as_ptr();
        let wup  = w_u.as_ptr();
        let l1wp = ln1w.as_ptr();
        let l1bp = ln1b.as_ptr();
        let l2wp = ln2w.as_ptr();
        let l2bp = ln2b.as_ptr();
        let itp  = input_tokens.as_ptr();

        let xp   = x_buf.as_mut_ptr();
        let xap  = xa_buf.as_mut_ptr();
        let qp   = q_buf.as_mut_ptr();
        let kp   = k_vec.as_mut_ptr();
        let vp   = v_buf.as_mut_ptr();
        let attp = attn_buf.as_mut_ptr();
        let xbp  = xb_buf.as_mut_ptr();
        let ffp  = ffn_h.as_mut_ptr();
        let lgp  = logits_buf.as_mut_ptr();
        let otp  = output_tokens.as_mut_ptr();

        // embed + sinusoidal pos: x = E[tok] + PE
        for i in 0..n {
            let tok = *itp.add(i) as usize;
            let ei = ep.add(tok * d);
            let pi = pep.add(i * d);
            let xi = xp.add(i * d);
            for j in 0..d { *xi.add(j) = *ei.add(j) + *pi.add(j); }
        }

        // LN_1: xa = LayerNorm(x; ln1w, ln1b)
        layer_norm(xp, l1wp, l1bp, xap, n, d);

        // Q/K/V projections
        matmul(xap, wqp, qp, n, d, d);
        matmul(xap, wkp, kp, n, d, d);
        matmul(xap, wvp, vp, n, d, d);

        // Multi-head attention -> attn
        multihead_attention(qp, kp, vp, attp, n, d, n_heads, head_dim);

        // attn @ W_O + residual
        matmul(attp, wop, xap, n, d, d);
        for i in 0..n*d { *xp.add(i) += *xap.add(i); }

        // LN_2
        layer_norm(xp, l2wp, l2bp, xbp, n, d);

        // FFN: xb @ W_1 -> ReLU -> @ W_2 + residual
        matmul(xbp, w1p, ffp, n, d, d_ff);
        for i in 0..n*d_ff { if *ffp.add(i) < 0.0 { *ffp.add(i) = 0.0; } }
        matmul(ffp, w2p, xap, n, d_ff, d);
        for i in 0..n*d { *xp.add(i) += *xap.add(i); }

        // Unembed + argmax
        matmul(xp, wup, lgp, n, d, vocab);
        for i in 0..n {
            let li = lgp.add(i * vocab);
            let mut best = 0usize;
            let mut bv = *li;
            for v in 1..vocab {
                let lv = *li.add(v);
                if lv > bv { bv = lv; best = v; }
            }
            *otp.add(i) = best as u8;
        }
    }

    let secs = t0.elapsed().as_secs_f64();

    // Checksum: sum_i ((i+1) * out[i]) mod 2^32
    let mut cs: u64 = 0;
    for i in 0..n {
        cs = (cs + (i as u64 + 1) * (output_tokens[i] as u64)) & 0xFFFFFFFF_u64;
    }
    println!("CHECKSUM={}", cs);
    println!("RUNTIME_SEC={}", secs);

    // Persist output_tokens.bin for cross-validation
    let out_path = format!("{}/output_tokens.bin", dir);
    if let Ok(mut f) = std::fs::File::create(&out_path) {
        use std::io::Write;
        let _ = f.write_all(&output_tokens);
    }
}
