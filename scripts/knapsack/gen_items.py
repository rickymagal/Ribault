#!/usr/bin/env python3
"""Generate deterministic knapsack items and compute expected optimal value via DP.

Output:
  <out-dir>/items.txt    — one line per item: "weight value"
  <out-dir>/expected.txt — single integer: optimal value
"""

import argparse, os


def lcg_items(n, seed=42):
    """Deterministic item generation using LCG."""
    a, c, m = 6364136223846793005, 1442695040888963407, 2**63
    r = seed
    items = []
    for _ in range(n):
        r = (a * r + c) % m
        w = (r >> 33) % 100 + 1       # weight in [1, 100]
        r = (a * r + c) % m
        v = (r >> 33) % 200 + 1       # value in [1, 200]
        items.append((int(w), int(v)))
    return items


def dp_knapsack(items, capacity):
    """Standard 0/1 knapsack DP. Returns optimal value."""
    n = len(items)
    dp = [0] * (capacity + 1)
    for i in range(n):
        w, v = items[i]
        for c in range(capacity, w - 1, -1):
            if dp[c - w] + v > dp[c]:
                dp[c] = dp[c - w] + v
    return dp[capacity]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n-items", type=int, required=True)
    ap.add_argument("--capacity", type=int, default=0,
                    help="Knapsack capacity (default: 60%% of total weight)")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--out-dir", required=True)
    args = ap.parse_args()

    items = lcg_items(args.n_items, args.seed)
    total_w = sum(w for w, _ in items)
    capacity = args.capacity if args.capacity > 0 else total_w * 60 // 100

    os.makedirs(args.out_dir, exist_ok=True)

    items_path = os.path.join(args.out_dir, "items.txt")
    with open(items_path, "w") as f:
        for w, v in items:
            f.write(f"{w} {v}\n")

    expected = dp_knapsack(items, capacity)
    exp_path = os.path.join(args.out_dir, "expected.txt")
    with open(exp_path, "w") as f:
        f.write(f"{expected}\n")

    cap_path = os.path.join(args.out_dir, "capacity.txt")
    with open(cap_path, "w") as f:
        f.write(f"{capacity}\n")

    print(f"[gen_items] n={args.n_items} capacity={capacity} total_weight={total_w} "
          f"expected={expected} -> {args.out_dir}")


if __name__ == "__main__":
    main()
