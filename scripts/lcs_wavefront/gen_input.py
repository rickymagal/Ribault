#!/usr/bin/env python3
"""Generate deterministic LCS wavefront input and compute expected score.

Single LCS between two sequences of length N over alphabet of size ALPHA.
Sequences generated deterministically from seed using LCG.

Output:
  <out-dir>/params.txt   — "N ALPHA SEED"
  <out-dir>/expected.txt  — single integer: LCS length
"""

import argparse, os, sys


def lcg_stream(seed):
    a, c, m = 6364136223846793005, 1442695040888963407, 2**63
    r = seed
    while True:
        r = (a * r + c) % m
        yield r


def gen_string(rng, length, alpha):
    return [(next(rng) >> 33) % alpha for _ in range(length)]


def lcs_dp(xs, ys):
    m, n = len(xs), len(ys)
    prev = [0] * (n + 1)
    for i in range(m):
        curr = [0] * (n + 1)
        xi = xs[i]
        for j in range(n):
            if xi == ys[j]:
                curr[j + 1] = prev[j] + 1
            else:
                curr[j + 1] = max(prev[j + 1], curr[j])
        prev = curr
        if (i + 1) % 1000 == 0 or i == m - 1:
            print(f"  row {i+1}/{m}  score_so_far={prev[n]}", file=sys.stderr)
    return prev[n]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--N", type=int, required=True, help="Sequence length")
    ap.add_argument("--alphabet", type=int, default=4)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--out-dir", required=True)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    with open(os.path.join(args.out_dir, "params.txt"), "w") as f:
        f.write(f"{args.N} {args.alphabet} {args.seed}\n")

    # For large N, skip the O(N^2) Python DP — just write params
    # The benchmark script will cross-validate all 3 strategies instead
    if args.N > 10000:
        print(f"[gen_lcs_wf] N={args.N} (skipping Python DP, too large)")
        # Write empty expected so the script knows to cross-validate
        with open(os.path.join(args.out_dir, "expected.txt"), "w") as f:
            f.write("SKIP\n")
        return

    rng = lcg_stream(args.seed)
    seqA = gen_string(rng, args.N, args.alphabet)
    seqB = gen_string(rng, args.N, args.alphabet)

    print(f"[gen_lcs_wf] computing LCS for N={args.N}...", file=sys.stderr)
    score = lcs_dp(seqA, seqB)

    with open(os.path.join(args.out_dir, "expected.txt"), "w") as f:
        f.write(f"{score}\n")

    print(f"[gen_lcs_wf] N={args.N} alphabet={args.alphabet} seed={args.seed} "
          f"expected_score={score}")


if __name__ == "__main__":
    main()
