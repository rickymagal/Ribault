#!/usr/bin/env python3
"""Generate a Ribault .hss source for recursive N-Queens (TALM dataflow).

Design (June 2026 post-debug + post-perf-fix)
---------------------------------------------
1. Single TALM-recursive `solve` in .hss; column iteration UNROLLED
   at .hss-emit time into N let-bindings (one conditional callsnd
   per column).  Two earlier formulations (list-cons + case-of-cons;
   two mutually-aware recursive top-level functions) hit codegen
   lowering bugs that silently undercount -- this is the recipe
   that passes OEIS cross-validation for N=4..15.

2. `safeRec path currRow col` is a super: at depth < CUTOFF the
   path is small (<= CUTOFF queens placed) and the diagonal check
   walks the integer-encoded path with `mod`/`div`.  Cheap.

3. `solveRest path pow row` is the leaf super that does the bulk
   of the work (rows CUTOFF..N-1).  IMPORTANT: the inner solver
   here mirrors `nq_seq.hs` bit-for-bit -- it uses
   `Data.Vector.Unboxed Int` for the queens prefix with `V.snoc` /
   `V.!` exactly like the baseline.  No `sum [list]`, no `mod`/
   `div` in the inner loop; strict accumulator with BangPatterns.
   This matches the baseline's allocation + indexing profile so
   that Ribault's P=1 has the same constant overhead as
   nq_seq.hs's P=1.  Any residual gap is pure TALM dispatch
   overhead (small with CUTOFF=2 -- only N^2 leaves).

   This requires the super build to enable BangPatterns and link
   the `vector` package.  The runner (run_nqueens_paper_sweep.sh)
   sets:
       SUPERS_GHC_PACKAGES="vector"
       SUPERS_GHC_FLAGS="-XBangPatterns"
       SUPERS_EXTRA_IMPORTS="import qualified Data.Vector.Unboxed as V"
   before calling tools/build_supers.sh.  The SUPERS_EXTRA_IMPORTS
   env var is honored by the normalize_hs_module pass in
   build_supers.sh and injects the import right after `module
   Supers where`.

CUTOFF
------
gen_nq_hss.py defaults CUTOFF=2 uniformly.  Larger CUTOFF would
overflow the interpreter's deque ("Deque is full. Aborting.") at
N>=13 (see scripts/nqueens/description.txt for details).

ASCII-only source -- the codegen Haskell parser rejects non-ASCII.
"""

import argparse, os


def default_cutoff(N):
    """CUTOFF=2 uniform for N=11..15 -- larger overflows the deque
    at N>=13; smaller would have too few TALM leaves to amortize
    dispatch.  See module docstring."""
    return 2


def emit_hss(N, cutoff):
    lines = []
    lines.append(f"-- N-Queens recursive TALM solver (auto-gen, N={N}, CUTOFF={cutoff}).")
    lines.append(f"-- The TALM-recursive `solve` decomposes the top {cutoff} rows;")
    lines.append(f"-- below CUTOFF the `solveRest` super (Vector-Int-based, mirrors")
    lines.append(f"-- nq_seq.hs exactly) handles the remaining {N - cutoff} rows.")
    lines.append("")

    # solveRest super: sequential N-Queens for the bottom of the search tree.
    # Mirrors nq_seq.hs's `solve` + `safeQ` exactly using V.Vector Int.
    # `path` is decoded once at super entry into a V.Vector.
    lines.append("solveRest path pow row =")
    lines.append("  super solveRestImpl path pow row (")
    lines.append("    solveRestImpl path pow row =")
    lines.append(f"      let !nVal = {N} :: Int")
    lines.append("          !rowI = fromIntegral row :: Int")
    lines.append("          !pathI = fromIntegral path :: Int")
    lines.append("          -- Decode integer-encoded path (base nVal) into V.Vector Int")
    lines.append("          decode !acc !p !i")
    lines.append("            | i >= rowI = acc")
    lines.append("            | otherwise =")
    lines.append("                let !q = p `mod` nVal")
    lines.append("                in decode (V.snoc acc q) (p `div` nVal) (i + 1)")
    lines.append("          !queens0 = decode V.empty pathI 0")
    lines.append("          -- safeQ / solveSeq mirror nq_seq.hs bit-for-bit")
    lines.append("          safeQ !qs !r !col = goS 0")
    lines.append("            where")
    lines.append("              goS !i")
    lines.append("                | i >= r    = True")
    lines.append("                | otherwise =")
    lines.append("                    let !c = qs `V.unsafeIndex` i")
    lines.append("                    in if c == col then False")
    lines.append("                       else if c - i == col - r then False")
    lines.append("                       else if c + i == col + r then False")
    lines.append("                       else goS (i + 1)")
    lines.append("          solveSeq !qs !r")
    lines.append("            | r >= nVal = 1")
    lines.append("            | otherwise = go 0 0")
    lines.append("            where")
    lines.append("              go !c !acc")
    lines.append("                | c >= nVal = acc")
    lines.append("                | safeQ qs r c =")
    lines.append("                    let !q' = V.snoc qs c")
    lines.append("                        !sub = solveSeq q' (r + 1)")
    lines.append("                    in go (c + 1) (acc + sub)")
    lines.append("                | otherwise = go (c + 1) acc")
    lines.append("      in fromIntegral (solveSeq queens0 rowI) :: Int64")
    lines.append("  )")
    lines.append("")

    # safeRec super: diagonal check for TALM-level solve, depth <= CUTOFF-1.
    lines.append("safeRec path currRow col =")
    lines.append("  super safeImpl path currRow col (")
    lines.append("    safeImpl path currRow col =")
    lines.append("      let go !p !r")
    lines.append("            | r >= currRow = True")
    lines.append("            | otherwise =")
    lines.append(f"                let !q = p `mod` {N}")
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
    ap.add_argument("--cutoff", type=int, default=None)
    args = ap.parse_args()
    cutoff = args.cutoff if args.cutoff is not None else default_cutoff(args.N)
    emit(args.out, args.N, cutoff)


if __name__ == "__main__":
    main()
