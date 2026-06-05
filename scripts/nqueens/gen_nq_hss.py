#!/usr/bin/env python3
"""Generate a Ribault .hss source for recursive N-Queens.

The .hss is compiled by Ribault's codegen tool into a .fl that
expresses the recursion as TALM callsnd / retsnd with multiplicative
tagging.  The top-level `nq queens row` is plain .hss recursion; each
'+' between two recursive calls translates to two callsnds running in
parallel under TALM.  Below CUTOFF the recursion drops into the
sequential super `nq_seq queens`.

NOTE: Ribault .hss supers take a single input arg (no commas in
`input (...)`), so `nq_seq` takes only `queens` and the row index is
recovered inside the super from the list length.  Comments must be
ASCII-only (the codegen Haskell parser rejects non-ASCII bytes).
"""

import argparse, os


HSS_TEMPLATE = """-- N-Queens recursive solver (auto-generated).
-- N=__N__   CUTOFF=__CUTOFF__

-- Sequential subtree solver wrapped as a TALM super.  Single input
-- (queens); the row index is the length of the queens list.  Drops
-- out of dataflow at the cutoff depth; below this point every
-- recursive call stays inside Haskell on a single capability.
nq_seq queens =
  super single input (queens) output (out)
#BEGINSUPER
    out = solveSeq queens
      where
        n_const = __N__
        lenList lst = case lst of
                        []     -> 0
                        (_:t)  -> 1 + lenList t
        solveSeq qs
          | lenList qs >= n_const = 1
          | otherwise             = solveCol qs 0
        solveCol qs c
          | c >= n_const  = 0
          | otherwise     =
              if safeAt qs c 1
                then solveSeq (c : qs) + solveCol qs (c + 1)
                else solveCol qs (c + 1)
        safeAt qs col offset =
          case qs of
            []     -> True
            (h:t)  -> if h == col
                        then False
                        else if (h - offset) == col
                          then False
                          else if (h + offset) == col
                            then False
                            else safeAt t col (offset + 1)
#ENDSUPER

-- Top-level safety check (also TALM-level, for branches above cutoff).
safe queens col offset =
  case queens of
    []    -> True
    (h:t) ->
      if h == col
        then False
        else if (h - offset) == col
          then False
          else if (h + offset) == col
            then False
            else safe t col (offset + 1)

-- Branch over columns at a given row.  The two recursive calls
-- (`nq deeper` and `nqAtRow next column`) are independent: TALM
-- fires them as two child supers with unique tags via callsnd.
nqAtRow queens row c =
  if c >= __N__
    then 0
    else
      if safe queens c 1
        then nq (c : queens) (row + 1) + nqAtRow queens row (c + 1)
        else nqAtRow queens row (c + 1)

-- Top-level recursive solver.  Above CUTOFF the recursion stays in
-- TALM dataflow; at/below CUTOFF it drops into the sequential super.
nq queens row =
  if row >= __N__
    then 1
    else
      if row >= __CUTOFF__
        then nq_seq queens
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
