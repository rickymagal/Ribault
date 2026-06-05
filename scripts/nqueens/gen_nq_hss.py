#!/usr/bin/env python3
"""Generate a Ribault .hss source for recursive N-Queens (TALM dataflow).

Design (post-debugging June 2026)
---------------------------------
The naive list-cons formulation (`safe (h:t)` with `case`) hits a
codegen lowering bug that drops the accumulated count in recursive
`+` between callsnds with list-handle args.  An integer-encoded
recursion using two mutually-aware top-level functions (`safe` and
`solve` both recursive) ALSO triggers a related codegen issue that
silently undercounts (verified: for N=5 returns 8 instead of 10).

The working recipe — verified for N=4..15 against OEIS A000170 —
is the following:

  1. Single recursive function `solve` in .hss.

  2. Column iteration at each row is UNROLLED at .hss-emit time
     (per-N codegen).  At each level we emit N let-bindings
     s_0..s_{N-1}, one conditional callsnd per column.  This means
     the only TALM-tag axis is depth (= row), with stride = next
     power-of-2 of N.  For N=15 that is 16, so tag at max depth
     CUTOFF=2 is 16^2 = 256, well inside the 64-bit tag space.

  3. `safeRec path currRow col` is a SUPER (Haskell-compiled),
     NOT another recursive .hss function.  Two top-level recursive
     .hss functions trigger the codegen bug above; one recursive
     .hss + supers is fine (verified).  The super body walks the
     path integer with `mod` / `div` and checks the three diagonal
     constraints.

  4. `solveRest path pow row` is also a super: once `solve` has
     recursed to depth CUTOFF in TALM, it dispatches the remainder
     of the search (rows CUTOFF..N-1) to a sequential Haskell
     solver running as a single super-call.  This bounds the
     TALM-tag depth and the deque size needed by the interpreter
     while still giving CUTOFF levels of parallel TALM
     decomposition (15^CUTOFF leaves for N=15).

CUTOFF per N
------------
The TALM interpreter's deque overflows ("Deque is full. Aborting.")
when total in-flight tokens exceed ~30k.  Empirically:

  N <= 12 : CUTOFF=3 works  (12^3 = 1728 max leaves)
  N >= 13 : CUTOFF=2 works  (15^2 =  225 max leaves)

For N=15 specifically: 15^2 = 225 leaves; each leaf super performs
sequential N-Queens for the remaining 13 rows.  Q(15) = 2,279,184
distributed across 225 leaves ~= 10K solutions / leaf.  Total
sequential work matches seq_haskell.  Parallel scaling comes from
TALM dispatching the 225 leaves to P workers.

Tag depth representability
--------------------------
With UNROLLED columns, the only tag axis is `solve`'s recursion
depth (= row).  Stride is 16 (next pow-2 of 15).  Max depth = CUTOFF.

  Tag space = 16^CUTOFF
    CUTOFF=3 -> 4096    (fits easily in 64-bit)
    CUTOFF=2 -> 256     (fits easily in 64-bit)

Compare to the naive recursion (column AND row axes both
recursive): tag = 4^(N + N*N) = 4^240 for N=15 = catastrophic
overflow.  Unrolling collapses the column axis to compile-time
let-bindings instead of runtime tag bits.

ASCII-only source
-----------------
The codegen Haskell parser rejects non-ASCII bytes (e.g. em-dash,
arrow), so comments stick to plain ASCII.
"""

import argparse, os


def default_cutoff(N):
    """Choose CUTOFF.  We use CUTOFF=2 uniformly across N=11..15:

    - For N>=13 CUTOFF=3 overflows the interpreter's deque
      ("Deque is full. Aborting.").
    - For N<=12 CUTOFF=3 fits the deque but yields too little
      work per leaf super to amortize TALM dispatch overhead
      (verified: P>1 does NOT speed up over P=1).
    - CUTOFF=2 keeps the methodology uniform and gives the
      cleanest speedup curve at N>=13 where the per-leaf work
      is meaningful (15^2 = 225 leaves for N=15, each leaf
      does ~10K sub-solutions of work in the Haskell super).
    """
    return 2


def emit_hss(N, cutoff):
    lines = []
    lines.append(f"-- N-Queens recursive solver, integer-encoded path.")
    lines.append(f"-- N={N}  CUTOFF={cutoff}")
    lines.append(f"-- solve recurses in TALM down to row={cutoff}, then dispatches")
    lines.append(f"-- the remainder to solveRest (a sequential Haskell super).")
    lines.append(f"-- safeRec is a super; column iteration at each row is UNROLLED")
    lines.append(f"-- into N let-bindings so the TALM-tag axis is row only.")
    lines.append("")

    # solveRest super: sequential N-Queens for the bottom of the search tree.
    lines.append("solveRest path pow row =")
    lines.append("  super solveRestImpl path pow row (")
    lines.append("    solveRestImpl path pow row =")
    lines.append("      let safeQ p curr col = goS p 0 curr col")
    lines.append("          goS p r curr col =")
    lines.append("            if r >= curr then True")
    lines.append("            else")
    lines.append(f"              let q = p `mod` {N}")
    lines.append("              in if q == col then False")
    lines.append("                 else if q + r == col + curr then False")
    lines.append("                 else if q - r == col - curr then False")
    lines.append(f"                 else goS (p `div` {N}) (r + 1) curr col")
    lines.append("          go p pw r =")
    lines.append(f"            if r >= {N} then 1")
    lines.append(f"            else sum [ go (p + c*pw) (pw*{N}) (r+1) | c <- [0..{N}-1], safeQ p r c ]")
    lines.append("      in go path pow row")
    lines.append("  )")
    lines.append("")

    # safeRec super: diagonal check, walks the encoded path.
    lines.append("safeRec path currRow col =")
    lines.append("  super safeImpl path currRow col (")
    lines.append("    safeImpl path currRow col =")
    lines.append("      let go p r")
    lines.append("            | r >= currRow = True")
    lines.append("            | otherwise =")
    lines.append(f"                let q = p `mod` {N}")
    lines.append("                in if q == col then False")
    lines.append("                   else if q + r == col + currRow then False")
    lines.append("                   else if q - r == col - currRow then False")
    lines.append(f"                   else go (p `div` {N}) (r + 1)")
    lines.append("      in if go path 0 then 1 else 0")
    lines.append("  )")
    lines.append("")

    # solve: TALM-recursive top, dispatches to solveRest at CUTOFF.
    lines.append("solve path row pow =")
    lines.append(f"  if row >= {cutoff}")
    lines.append("    then solveRest path pow row")
    lines.append("    else")
    indent = "      "
    for c in range(N):
        lines.append(f"{indent}let s{c} = if (safeRec path row {c}) == 1 then solve (path + {c} * pow) (row + 1) (pow * {N}) else 0")
        lines.append(f"{indent}in")
        indent += "  "
    lines.append(indent + " + ".join(f"s{c}" for c in range(N)))
    lines.append("")
    lines.append("main = print (solve 0 0 1)")
    lines.append("")
    return "\n".join(lines)


def emit(out_path, N, cutoff):
    dirpath = os.path.dirname(out_path)
    if dirpath:
        os.makedirs(dirpath, exist_ok=True)
    src = emit_hss(N, cutoff)
    with open(out_path, "w") as f:
        f.write(src)
    print(f"[gen_nq_hss] wrote {out_path} (N={N}, cutoff={cutoff})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=None,
                    help="If omitted, uses 3 for N<=12 and 2 for N>=13 (deque-limit safe).")
    args = ap.parse_args()
    cutoff = args.cutoff if args.cutoff is not None else default_cutoff(args.N)
    emit(args.out, args.N, cutoff)


if __name__ == "__main__":
    main()
