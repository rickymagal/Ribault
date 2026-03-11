#!/usr/bin/env python3
"""Generate input parameters for merge sort benchmark.

Writes params.txt with N.
For validation, expected result is always 1 (sorted verification).
"""

import argparse, os


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--out-dir", required=True)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    with open(os.path.join(args.out_dir, "params.txt"), "w") as f:
        f.write(f"{args.N}\n")

    with open(os.path.join(args.out_dir, "expected.txt"), "w") as f:
        f.write("1\n")

    print(f"[gen_ms_input] wrote params.txt  (N={args.N})")


if __name__ == "__main__":
    main()
