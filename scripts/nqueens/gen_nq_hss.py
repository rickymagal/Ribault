#!/usr/bin/env python3
"""Generate a Ribault .hss source for recursive N-Queens.

Pure .hss recursion -- no super.  Each `+` between two recursive
calls translates to two callsnds running in parallel under TALM,
with tag multiplication handled automatically by the codegen.

The binarization `nq queens row` (go-deeper) `+` `nqAtRow ...`
(go-sideways) means stride=2 in the tag space, which keeps the
multiplicative-tag depth well within 64 bits for N up to 15.

NOTE: comments must be ASCII-only -- the codegen Haskell parser
rejects non-ASCII bytes (e.g. em-dash, arrow).
"""

import argparse, os


HSS_TEMPLATE = """-- N-Queens recursive solver (auto-generated).
-- N=__N__   CUTOFF=__CUTOFF__ (unused: recursion stays in TALM all the way).

-- Top-level safety check.  Recurses over the list of placed queens.
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

-- Top-level recursive solver.  Each safe child becomes a callsnd
-- with a unique multiplicative tag; sibling branches run in
-- parallel.
nq queens row =
  if row >= __N__
    then 1
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
