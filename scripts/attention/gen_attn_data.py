#!/usr/bin/env python3
"""Generate full transformer-block end-to-end data: tokens, weights,
positional encoding lookup, and reference output (via numpy).

Algorithm (one transformer block, GPT-style, lifted from llama2.c
but simplified — see description.txt):

    x[i] = E[input_tokens[i]] + PE[i]                # embed + sinusoidal pos
    x_a = LN(x; LN_1_w, LN_1_b)                      # LayerNorm
    Q = x_a @ W_Q                                    # N x D
    K = x_a @ W_K                                    # N x D
    V = x_a @ W_V                                    # N x D
    # Multi-head attention (n_heads heads, head_dim = D / n_heads)
    A_h[i,j] = softmax_j(Q_h[i] . K_h[j] / sqrt(head_dim))
    attn[i] = concat over h: sum_j A_h[i,j] V_h[j]    # N x D
    x = x + attn @ W_O                               # residual
    x_b = LN(x; LN_2_w, LN_2_b)
    h = ReLU(x_b @ W_1) @ W_2                        # FFN
    x = x + h                                        # residual
    logits[i] = x[i] @ W_U                           # N x VOCAB
    output_tokens[i] = argmax_v(logits[i, v])
    checksum = sum_i((i+1) * output_tokens[i]) mod 2**32

All tensors are float64 little-endian, row-major. Input/output tokens
are uint8. Weights are LCG-derived deterministic (seeded). Reference
output is computed by this script in numpy and saved alongside the
weights for cross-validation.
"""

import argparse, os, struct, math
import numpy as np


def lcg_seq(seed, count):
    """LCG sequence yielding count uniform [0, 1) doubles. Python int math."""
    out = np.empty(count, dtype=np.float64)
    r = int(seed) & 0x7FFFFFFFFFFFFFFF
    if r == 0:
        r = 1
    M = 0x7FFFFFFFFFFFFFFF
    A = 6364136223846793005
    C = 1442695040888963407
    for i in range(count):
        r = (A * r + C) & M
        out[i] = float(r >> 33) / float(1 << 30)
    return out


def lcg_gauss(seed, count, scale=0.02):
    """LCG-derived Gaussian-like via Box-Muller, scaled."""
    n = (count + 1) & ~1  # even
    u = lcg_seq(seed, n)
    u = u.reshape(n // 2, 2)
    # Avoid log(0)
    u[:, 0] = np.maximum(u[:, 0], 1e-12)
    r = np.sqrt(-2.0 * np.log(u[:, 0]))
    theta = 2.0 * math.pi * u[:, 1]
    z = np.empty(n, dtype=np.float64)
    z[0::2] = r * np.cos(theta)
    z[1::2] = r * np.sin(theta)
    return z[:count] * scale


def sinusoidal_pos_encoding(N, D):
    """Standard sinusoidal positional encoding (Attention Is All You Need)."""
    pe = np.zeros((N, D), dtype=np.float64)
    pos = np.arange(N, dtype=np.float64).reshape(-1, 1)
    i = np.arange(D // 2, dtype=np.float64)
    div = np.exp(-(2.0 * i / D) * math.log(10000.0))
    pe[:, 0::2] = np.sin(pos * div)
    pe[:, 1::2] = np.cos(pos * div)
    return pe


def layer_norm(x, w, b, eps=1e-5):
    mu = x.mean(axis=-1, keepdims=True)
    var = x.var(axis=-1, keepdims=True)
    return (x - mu) / np.sqrt(var + eps) * w + b


def softmax_rows(x):
    m = x.max(axis=-1, keepdims=True)
    e = np.exp(x - m)
    return e / e.sum(axis=-1, keepdims=True)


def forward(input_tokens, E, PE, W_Q, W_K, W_V, W_O,
            LN_1_w, LN_1_b, LN_2_w, LN_2_b,
            W_1, W_2, W_U, n_heads):
    N, D = PE.shape
    head_dim = D // n_heads
    inv_sqrt = 1.0 / math.sqrt(head_dim)

    # embed + pos
    x = E[input_tokens] + PE                                    # N x D

    # LN_1 + Q/K/V
    x_a = layer_norm(x, LN_1_w, LN_1_b)
    Q = x_a @ W_Q                                                # N x D
    K = x_a @ W_K                                                # N x D
    V = x_a @ W_V                                                # N x D

    # Multi-head attention
    # Reshape (N, D) -> (N, n_heads, head_dim) -> (n_heads, N, head_dim)
    Qh = Q.reshape(N, n_heads, head_dim).transpose(1, 0, 2)
    Kh = K.reshape(N, n_heads, head_dim).transpose(1, 0, 2)
    Vh = V.reshape(N, n_heads, head_dim).transpose(1, 0, 2)
    # Scores: (n_heads, N, N)
    scores = np.einsum('hnk,hmk->hnm', Qh, Kh) * inv_sqrt
    # Softmax over keys
    A = softmax_rows(scores)
    # Output: (n_heads, N, head_dim)
    attn_h = np.einsum('hnm,hmk->hnk', A, Vh)
    # Concat heads back to (N, D)
    attn = attn_h.transpose(1, 0, 2).reshape(N, D)

    # Output projection + residual
    x = x + attn @ W_O

    # LN_2 + FFN
    x_b = layer_norm(x, LN_2_w, LN_2_b)
    h = np.maximum(x_b @ W_1, 0.0)
    x = x + h @ W_2

    # Unembed + argmax
    logits = x @ W_U                                             # N x VOCAB
    out_tokens = np.argmax(logits, axis=-1).astype(np.uint8)
    return out_tokens, logits


def checksum_tokens(toks):
    # sum_i ((i+1) * toks[i]) mod 2**32
    n = len(toks)
    idx = np.arange(1, n + 1, dtype=np.uint64)
    return int((np.array(toks, dtype=np.uint64) * idx).sum() & np.uint64(0xFFFFFFFF))


def write_bin(path, arr):
    arr.tofile(path)


def gen(out_dir, N, D, n_heads, vocab, d_ff, seed):
    assert D % n_heads == 0, "D must be divisible by n_heads"
    os.makedirs(out_dir, exist_ok=True)
    head_dim = D // n_heads

    # Weights (deterministic LCG-derived Gaussian-like)
    # Use distinct seed offsets so different tensors don't collide.
    SCALE = 0.02
    LN_SCALE_INIT = 1.0
    LN_BIAS_INIT = 0.0

    E       = lcg_gauss(seed + 0,  vocab * D, SCALE).reshape(vocab, D)
    W_Q     = lcg_gauss(seed + 1,  D * D, SCALE).reshape(D, D)
    W_K     = lcg_gauss(seed + 2,  D * D, SCALE).reshape(D, D)
    W_V     = lcg_gauss(seed + 3,  D * D, SCALE).reshape(D, D)
    W_O     = lcg_gauss(seed + 4,  D * D, SCALE).reshape(D, D)
    W_1     = lcg_gauss(seed + 5,  D * d_ff, SCALE).reshape(D, d_ff)
    W_2     = lcg_gauss(seed + 6,  d_ff * D, SCALE).reshape(d_ff, D)
    W_U     = lcg_gauss(seed + 7,  D * vocab, SCALE).reshape(D, vocab)
    LN_1_w  = np.full(D, LN_SCALE_INIT, dtype=np.float64)
    LN_1_b  = np.full(D, LN_BIAS_INIT,  dtype=np.float64)
    LN_2_w  = np.full(D, LN_SCALE_INIT, dtype=np.float64)
    LN_2_b  = np.full(D, LN_BIAS_INIT,  dtype=np.float64)

    PE = sinusoidal_pos_encoding(N, D)

    # Input tokens: deterministic LCG bytes (Python int math)
    tok_seed = (int(seed) * 1099511628211) & 0xFFFFFFFFFFFFFFFF
    r = tok_seed if tok_seed != 0 else 1
    M = 0x7FFFFFFFFFFFFFFF
    A = 6364136223846793005
    C = 1442695040888963407
    input_tokens = np.empty(N, dtype=np.uint8)
    for i in range(N):
        r = (A * r + C) & M
        b = (r >> 33) & 0xFF
        if vocab < 256:
            b %= vocab
        input_tokens[i] = b

    # Reference forward
    print(f"[gen_attn_data] computing reference forward in numpy (N={N}, D={D}, n_heads={n_heads})...")
    out_tokens, _logits = forward(
        input_tokens, E, PE, W_Q, W_K, W_V, W_O,
        LN_1_w, LN_1_b, LN_2_w, LN_2_b, W_1, W_2, W_U, n_heads)
    cs = checksum_tokens(out_tokens)

    # Write everything
    for name, arr in [
        ("E.bin",        E),
        ("W_Q.bin",      W_Q),
        ("W_K.bin",      W_K),
        ("W_V.bin",      W_V),
        ("W_O.bin",      W_O),
        ("W_1.bin",      W_1),
        ("W_2.bin",      W_2),
        ("W_U.bin",      W_U),
        ("LN_1_w.bin",   LN_1_w),
        ("LN_1_b.bin",   LN_1_b),
        ("LN_2_w.bin",   LN_2_w),
        ("LN_2_b.bin",   LN_2_b),
        ("PE.bin",       PE),
        ("input_tokens.bin",          input_tokens),
        ("expected_output_tokens.bin", out_tokens),
    ]:
        write_bin(os.path.join(out_dir, name), np.ascontiguousarray(arr))

    with open(os.path.join(out_dir, "config.txt"), "w") as f:
        f.write(f"N {N}\n")
        f.write(f"D {D}\n")
        f.write(f"N_HEADS {n_heads}\n")
        f.write(f"HEAD_DIM {head_dim}\n")
        f.write(f"D_FF {d_ff}\n")
        f.write(f"VOCAB {vocab}\n")
        f.write(f"SEED {seed}\n")
    with open(os.path.join(out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{cs}\n")

    print(f"[gen_attn_data] wrote {out_dir}/ — expected checksum = {cs}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--D", type=int, default=512)
    ap.add_argument("--n-heads", type=int, default=8)
    ap.add_argument("--vocab", type=int, default=256)
    ap.add_argument("--d-ff", type=int, default=2048)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    gen(args.out_dir, args.N, args.D, args.n_heads, args.vocab, args.d_ff, args.seed)


if __name__ == "__main__":
    main()
