#!/usr/bin/env python3
"""Generate inputs for the N-Queens benchmark.

CRITICAL: this writes ONLY config.txt and expected_checksum.txt.  It
does NOT pre-expand the search tree.  Every parallel variant must
implement the full recursive backtracking from row=0 inside its own
binary, using its native parallelism idiom (Strategies recursive
parList rseq, Timely feedback / epoch-per-level, Ribault recursive
supers via callsnd/retsnd compiled from the .hss).  Pre-expanding
states to a fixed depth and parallelizing over the flat list of
leaf states would flatten the recursive structure into an
embarrassingly-parallel workload, which destroys exactly the
architectural property the benchmark is meant to expose.

OEIS A000170 supplies the canonical solution count Q(N):
   N=11: 2680   N=12: 14200   N=13: 73712   N=14: 365596   N=15: 2279184
"""
import argparse, os, sys


OEIS_A000170 = {
    1: 1, 2: 0, 3: 0, 4: 2, 5: 10, 6: 4, 7: 40, 8: 92,
    9: 352, 10: 724, 11: 2680, 12: 14200, 13: 73712,
    14: 365596, 15: 2279184,
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=5,
                    help="depth below which parallel variants drop to sequential. "
                         "Used by Strategies / Timely / Ribault — same value for all.")
    args = ap.parse_args()

    if args.N not in OEIS_A000170:
        sys.stderr.write(f"[gen_input] WARN: N={args.N} not in OEIS table\n")
    expected = OEIS_A000170.get(args.N, 0)

    os.makedirs(args.out_dir, exist_ok=True)
    with open(os.path.join(args.out_dir, "config.txt"), "w") as f:
        f.write(f"N {args.N}\n")
        f.write(f"CUTOFF {args.cutoff}\n")
        f.write(f"EXPECTED_COUNT {expected}\n")
    with open(os.path.join(args.out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{expected}\n")
    print(f"[gen_input] wrote {args.out_dir}/  N={args.N} cutoff={args.cutoff} expected={expected}")


if __name__ == "__main__":
    main()
