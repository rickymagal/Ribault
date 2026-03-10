#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate a parametric MatMul .hsk for the TALM benchmark.

Uses Haskell supers (normal pipeline, compiled by GHC).
N_FUNCS independent block-multiply supers fire in parallel via dataflow.
Each super is a separate function (not reused) to match TALM's dataflow model.

Each super computes rows [lo..hi) of C = A * B^T,
returning a partial checksum. The dataflow sums all partial
checksums and a print super validates the result.
"""

import argparse, os


def emit_hsk(path, N, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    n_funcs = min(n_funcs, N)
    blocks = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            blocks.append((lo, hi))

    SHIFT = N + 1
    nblocks = len(blocks)

    # Generate one super per block
    super_defs = []
    for idx, (lo, hi) in enumerate(blocks):
        rows = hi - lo
        packed = lo * SHIFT + rows
        super_defs.append(f"""-- SUPER block_{idx}: rows [{lo}..{hi}) of C = A * B^T
block_{idx} dummy =
  super single input (dummy) output (cs)
#BEGINSUPER
    cs = let
        sh   = {SHIFT} :: Int64
        n    = {N} :: Int64
        packed = {packed} :: Int64
        s    = packed `div` sh
        rows = packed `mod` sh
        lcg seed idx =
          let m = 2147483647 :: Int64
              a = 1103515245 :: Int64
              c = 12345 :: Int64
              val = (a * (seed + idx) + c) `mod` m
          in fromIntegral val / fromIntegral m
        getA i j = lcg 42  (i * n + j)
        getB i j = lcg 137 (i * n + j)
        dot i k = sum [ getA (s + i) j * getB k j | j <- [0..n-1] ]
        blockCS = sum [ dot ri k | ri <- [0..rows-1], k <- [0..n-1] ]
      in truncate (blockCS * 1000000 :: Double)
#ENDSUPER
""")

    leaf_lets = []
    for i in range(nblocks):
        kw = "let" if i == 0 else "in let"
        leaf_lets.append(f"  {kw} b{i} = block_{i} 0")

    if nblocks == 1:
        sum_expr = "b0"
    else:
        sum_expr = " + ".join(f"b{i}" for i in range(nblocks))

    hsk = f"""-- matmul.hsk  (auto-generated, Haskell supers)
-- N={N}  N_FUNCS={nblocks}

{"".join(super_defs)}
-- SUPER: print final checksum
print_checksum cs =
  super single input (cs) output (out)
#BEGINSUPER
    out = unsafePerformIO
      (do
        putStrLn ("CHECKSUM=" ++ show cs)
        pure 0)
#ENDSUPER

main =
{chr(10).join(leaf_lets)}
  in let total = {sum_expr}
  in print_checksum total
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_matmul_talm] wrote {path} (N={N}, n_funcs={nblocks})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, default=None,
                    help="Deprecated, use --n-funcs instead")
    ap.add_argument("--n-funcs", type=int, default=14)
    args = ap.parse_args()
    n_funcs = args.n_funcs
    if args.P is not None:
        n_funcs = args.P
    emit_hsk(args.out, args.N, n_funcs)


if __name__ == "__main__":
    main()
