#!/usr/bin/env python3
"""Generate deterministic LCS input and compute expected sum of LCS lengths via DP.

The benchmark computes LCS for N_PAIRS independent string pairs.
Each pair has two strings of length STR_LEN over an alphabet of size ALPHABET.
Strings are generated deterministically from a seed using LCG.

Output:
  <out-dir>/params.txt     — "n_pairs str_len alphabet seed"
  <out-dir>/expected.txt   — single integer: sum of all LCS lengths
"""

import argparse, os, sys


def lcg_stream(seed):
    """Deterministic PRNG using LCG (same constants as knapsack gen_items.py)."""
    a, c, m = 6364136223846793005, 1442695040888963407, 2**63
    r = seed
    while True:
        r = (a * r + c) % m
        yield r


def gen_string(rng, length, alphabet_size):
    """Generate a string as a list of ints in [0, alphabet_size)."""
    return [next(rng) % alphabet_size for _ in range(length)]


def lcs_dp(xs, ys):
    """Standard DP for LCS length (rolling row, O(n) space)."""
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
    return prev[n]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n-pairs", type=int, required=True)
    ap.add_argument("--str-len", type=int, required=True)
    ap.add_argument("--alphabet", type=int, default=4,
                    help="Alphabet size (default: 4, DNA-like)")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--out-dir", required=True)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    rng = lcg_stream(args.seed)
    total = 0
    for i in range(args.n_pairs):
        a = gen_string(rng, args.str_len, args.alphabet)
        b = gen_string(rng, args.str_len, args.alphabet)
        l = lcs_dp(a, b)
        total += l
        if (i + 1) % 10 == 0 or i == args.n_pairs - 1:
            print(f"  pair {i+1}/{args.n_pairs}  lcs={l}  running_sum={total}",
                  file=sys.stderr)

    with open(os.path.join(args.out_dir, "params.txt"), "w") as f:
        f.write(f"{args.n_pairs} {args.str_len} {args.alphabet} {args.seed}\n")

    with open(os.path.join(args.out_dir, "expected.txt"), "w") as f:
        f.write(f"{total}\n")

    print(f"[gen_lcs_input] n_pairs={args.n_pairs} str_len={args.str_len} "
          f"alphabet={args.alphabet} seed={args.seed} expected_sum={total}")


if __name__ == "__main__":
    main()
