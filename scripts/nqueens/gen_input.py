#!/usr/bin/env python3
"""Generate input + expected checksum for the N-Queens benchmark.

Pre-expand the search tree to a fixed depth `cutoff` (default 5) by
sequential backtracking; emit one row per surviving partial placement
(a 5-column "queens prefix") to `states.bin`.  Each parallel variant
then takes that list of leaf states, distributes them across workers,
and runs a sequential `solve_subtree(prefix, cutoff)` per state — this
factoring keeps the parallel comparison fair (all schedulers see the
same set of K leaf states to execute) while remaining trivially
distributable: each leaf state's subtree count is independent.

OEIS A000170 gives the expected total count Q(N) used as the canonical
checksum:
    N  Q(N)
   11    2680
   12   14200
   13   73712
   14  365596
   15 2279184

Outputs in <out-dir>:
  states.bin             header [n_states i32, cutoff i32] then
                         n_states * cutoff * i32 prefix arrays
  config.txt             N CUTOFF n_states  + EXPECTED_COUNT (OEIS)
  expected_checksum.txt  OEIS A000170 count as a string
"""
import argparse, os, struct, sys


OEIS_A000170 = {
    1: 1, 2: 0, 3: 0, 4: 2, 5: 10, 6: 4, 7: 40, 8: 92,
    9: 352, 10: 724, 11: 2680, 12: 14200, 13: 73712,
    14: 365596, 15: 2279184,
}


def safe(queens, row, col):
    """Return True if a queen at (row, col) does not conflict with queens
    already placed in queens[0..row-1]."""
    for r in range(row):
        c = queens[r]
        if c == col: return False
        if c - r == col - row: return False     # /-diagonal
        if c + r == col + row: return False     # \-diagonal
    return True


def expand_prefix(n, cutoff):
    """Sequential backtracking down to depth `cutoff`, yielding every
    valid `cutoff`-long prefix as a tuple."""
    queens = [0] * cutoff
    out = []
    def rec(row):
        if row == cutoff:
            out.append(tuple(queens))
            return
        for c in range(n):
            if safe(queens, row, c):
                queens[row] = c
                rec(row + 1)
    rec(0)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=5)
    args = ap.parse_args()

    if args.N not in OEIS_A000170:
        sys.stderr.write(f"[gen_input] WARN: N={args.N} not in OEIS table; expected_checksum will be 0\n")
    expected = OEIS_A000170.get(args.N, 0)
    cutoff = min(args.cutoff, args.N)

    os.makedirs(args.out_dir, exist_ok=True)
    sys.stderr.write(f"[gen_input] expanding prefix to depth {cutoff}\n")
    states = expand_prefix(args.N, cutoff)
    n_states = len(states)

    states_path = os.path.join(args.out_dir, "states.bin")
    with open(states_path, "wb") as f:
        f.write(struct.pack("<2i", n_states, cutoff))
        for s in states:
            f.write(struct.pack(f"<{cutoff}i", *s))

    cfg_path = os.path.join(args.out_dir, "config.txt")
    with open(cfg_path, "w") as f:
        f.write(f"N {args.N}\n")
        f.write(f"CUTOFF {cutoff}\n")
        f.write(f"N_STATES {n_states}\n")
        f.write(f"EXPECTED_COUNT {expected}\n")

    with open(os.path.join(args.out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{expected}\n")

    print(f"[gen_input] wrote {args.out_dir}/  N={args.N} cutoff={cutoff} "
          f"n_states={n_states} expected=Q(N)={expected}")


if __name__ == "__main__":
    main()
