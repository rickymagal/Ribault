#!/usr/bin/env python3
"""Generate binary matrix files for file-IO matmul benchmark.

Writes A.bin and BT.bin (B transposed) as flat row-major arrays of
IEEE-754 float64 (8 bytes each).  Same LCG as the on-the-fly version
so checksums can be cross-validated.
"""

import argparse, os, array


def lcg(seed, idx):
    m = 2147483647
    a = 1103515245
    c = 12345
    val = (a * (seed + idx) + c) % m
    return val / m


def gen(out_dir, N):
    os.makedirs(out_dir, exist_ok=True)

    # A[i][j] = lcg(42, i*N + j)
    a = array.array('d', [0.0] * (N * N))
    for i in range(N):
        for j in range(N):
            a[i * N + j] = lcg(42, i * N + j)

    a_path = os.path.join(out_dir, "A.bin")
    with open(a_path, "wb") as f:
        a.tofile(f)
    print(f"[gen_matrices] wrote {a_path}  ({N}x{N}, {os.path.getsize(a_path)} bytes)")

    # BT[k][j] = B[k][j] = lcg(137, k*N + j)  (B^T stored row-major = B row-major)
    bt = array.array('d', [0.0] * (N * N))
    for k in range(N):
        for j in range(N):
            bt[k * N + j] = lcg(137, k * N + j)

    bt_path = os.path.join(out_dir, "BT.bin")
    with open(bt_path, "wb") as f:
        bt.tofile(f)
    print(f"[gen_matrices] wrote {bt_path}  ({N}x{N}, {os.path.getsize(bt_path)} bytes)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    args = ap.parse_args()
    gen(args.out_dir, args.N)


if __name__ == "__main__":
    main()
