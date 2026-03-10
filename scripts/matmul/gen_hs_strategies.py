#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate matrix multiplication .hs using Control.Parallel.Strategies.

Equivalent structure to TALM: N_FUNCS independent block tasks, each computing
a contiguous range of rows of C = A * B^T using on-the-fly LCG matrix
generation.  Top-level parallelism via parMap rdeepseq over blocks.
Uses unsafePerformIO so block results can be evaluated in parallel.
"""

import argparse, os


def emit_hs(path, N, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    n_funcs = min(n_funcs, N)
    ranges = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            ranges.append((lo, hi))

    range_list = "[" + ", ".join(f"({lo}, {hi})" for lo, hi in ranges) + "]"

    src = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: Matrix Multiply (GHC Strategies)
-- N={N}  N_FUNCS={len(ranges)}

import Control.Parallel.Strategies (parMap, rdeepseq)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Int (Int64)

n :: Int64
n = {N}

-- Deterministic LCG: x = (a*seed + a*idx + c) mod m, scaled to [0,1)
lcg :: Int64 -> Int64 -> Double
lcg seed idx =
  let m = 2147483647 :: Int64
      a = 1103515245 :: Int64
      c = 12345 :: Int64
      val = (a * (seed + idx) + c) `mod` m
  in fromIntegral val / fromIntegral m

getA :: Int64 -> Int64 -> Double
getA i j = lcg 42  (i * n + j)

getB :: Int64 -> Int64 -> Double
getB i j = lcg 137 (i * n + j)

-- Process a block of rows [lo, hi), returning partial checksum.
-- Matches TALM computation: list-comprehension dot products.
-- Uses unsafePerformIO for parallel evaluation via Strategies.
processBlock :: (Int64, Int64) -> Int64
processBlock (lo, hi) = unsafePerformIO $ do
    let rows = hi - lo
        dot i k = sum [ getA i j * getB k j | j <- [0..n-1] ]
        blockCS = sum [ dot (lo + ri) k | ri <- [0..rows-1], k <- [0..n-1] ]
    return $! truncate (blockCS * 1000000 :: Double)

{{-# NOINLINE processBlock #-}}

main :: IO ()
main = do
  t0 <- getCurrentTime
  let ranges = {range_list}
      results = parMap rdeepseq processBlock ranges
      !total = sum results
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "CHECKSUM=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_matmul_strategies] wrote {path} (N={N}, n_funcs={len(ranges)})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.n_funcs)


if __name__ == "__main__":
    main()
