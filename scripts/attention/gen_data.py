#!/usr/bin/env python3
"""Generate binary input files for attention benchmark.

Writes Q.bin, K.bin, V.bin as flat row-major arrays of float64.
  Q, K, V: each N x D
"""

import argparse, os, array


def lcg(seed, idx):
    m = 2147483647
    a = 1103515245
    c = 12345
    val = (a * (seed + idx) + c) % m
    return val / m


def gen(out_dir, N, D):
    os.makedirs(out_dir, exist_ok=True)

    for name, seed in [("Q", 42), ("K", 137), ("V", 271)]:
        arr = array.array('d', [0.0] * (N * D))
        for i in range(N):
            for j in range(D):
                arr[i * D + j] = lcg(seed, i * D + j)
        path = os.path.join(out_dir, f"{name}.bin")
        with open(path, "wb") as f:
            arr.tofile(f)
        print(f"[gen_attn_data] wrote {path}  ({N}x{D}, {os.path.getsize(path)} bytes)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True, help="Sequence length")
    ap.add_argument("--D", type=int, default=512, help="Model dimension")
    args = ap.parse_args()
    gen(args.out_dir, args.N, args.D)


if __name__ == "__main__":
    main()
