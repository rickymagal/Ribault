#!/usr/bin/env python3
"""Generate a Ribault .hss source for recursive N-Queens.

The .hss is compiled by Ribault's codegen tool into a .fl that
expresses the recursion as TALM callsnd / retsnd with multiplicative
tagging -- the canonical idiom for which TALM was designed (Arvind
& Nikhil 1987).  This is fundamentally different from the flat
"one super per prefix state" decomposition emitted by gen_nq_c.py
and friends; here the search tree is expanded dynamically at runtime
by the TALM dataflow, not precomputed in gen_input.py.

CUTOFF controls how deep the recursion stays in TALM dataflow before
dropping to a Haskell-coded sequential subtree super.  Above cutoff,
each `+` between two recursive calls fires the calls as independent
dataflow operations (implicit parallelism).
"""

import argparse, os


HSS_TEMPLATE = """-- N-Queens recursive solver (auto-generated).
-- N=__N__   CUTOFF=__CUTOFF__

-- Sequential subtree solver wrapped as a TALM super.  Drops out of
-- dataflow at the cutoff depth; below this point, every recursive
-- call stays inside Haskell on a single capability.
nq_seq queens row =
  super single input (queens, row) output (out)
#BEGINSUPER
    out = solveSeq queens row
      where
        n_const = __N__
        solveSeq qs r
          | r >= n_const = 1
          | otherwise    = solveCol qs r 0
        solveCol qs r c
          | c >= n_const          = 0
          | safeAt qs c 1         = solveSeq (c : qs) (r + 1)
                                  + solveCol qs r (c + 1)
          | otherwise             = solveCol qs r (c + 1)
        safeAt qs col offset =
          case qs of
            []     -> True
            (h:t)  -> if h == col              then False
                      else if (h - offset) == col then False
                      else if (h + offset) == col then False
                      else safeAt t col (offset + 1)
#ENDSUPER

-- Top-level safety check (also TALM-level, for branches above cutoff).
safe queens col offset =
  case queens of
    []    -> True
    (h:t) ->
      if h == col then False
      else if (h - offset) == col then False
      else if (h + offset) == col then False
      else safe t col (offset + 1)

-- Branch over columns at a given row: the two recursive calls
-- (`nq deeper` and `nqAtRow next column`) are independent -- TALM
-- fires them as two child supers with unique tags via callsnd.
nqAtRow queens row c =
  if c >= __N__
    then 0
    else
      if safe queens c 1
        then nq (c : queens) (row + 1) + nqAtRow queens row (c + 1)
        else nqAtRow queens row (c + 1)

-- Top-level recursive solver.  Above CUTOFF, the recursion stays in
-- TALM dataflow (tagged-token recursive calls).  At or below CUTOFF,
-- the residual subtree drops into the Haskell-coded `nq_seq` super.
nq queens row =
  if row >= __N__
    then 1
    else
      if row >= __CUTOFF__
        then nq_seq queens row
        else nqAtRow queens row 0

main = print (nq [] 0)
"""


def emit(out_path, N, CUTOFF):
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    src = HSS_TEMPLATE.replace("__N__", str(N)).replace("__CUTOFF__", str(CUTOFF))
    with open(out_path, "w") as f:
        f.write(src)
    print(f"[gen_nq_hss] wrote {out_path} (N={N}, cutoff={CUTOFF})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=5)
    args = ap.parse_args()
    emit(args.out, args.N, args.cutoff)


if __name__ == "__main__":
    main()
