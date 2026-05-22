#!/usr/bin/env python3
"""Generate pure sequential Haskell for LCS wavefront benchmark.

Uses a simple 2-row DP technique (prev_row / curr_row) backed by
Data.Vector.Unboxed.Mutable — same idiom as the parallel Haskell
variants (TALM-Hs, STRAT, Repa). This ensures the speedup denominator
is measured on the same compute backend as the numerator, so the
reported speedups reflect the parallel scheduler/runtime rather than
a compute-backend swap.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (sequential baseline, 2-row, Vector)
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

-- ===== Constants =====

lcsSeqLen :: Int
lcsSeqLen = __N__

lcsAlpha :: Int
lcsAlpha = __ALPHA__

lcsSeed :: Word64
lcsSeed = __SEED__

-- ===== LCG PRNG =====

lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

lcsGenSeq :: Word64 -> Int -> Int -> IO (MV.IOVector Int, Word64)
lcsGenSeq !rng0 len_ alpha = do
  arr <- MV.replicate len_ 0
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha) :: Int
            MV.write arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  return (arr, rng')

-- ===== 2-row sequential LCS (Data.Vector.Unboxed.Mutable) =====

lcsSequential :: MV.IOVector Int -> MV.IOVector Int -> IO Int
lcsSequential !sa !sb = do
  let !n = lcsSeqLen
      !cols = n + 1
  prevRow <- MV.replicate cols 0
  curRow  <- MV.replicate cols 0
  let outerLoop !prev !cur !i
        | i > n = MV.read prev n
        | otherwise = do
            MV.write cur 0 (0 :: Int)
            !ai <- MV.read sa (i - 1)
            let innerLoop !j
                  | j > n = return ()
                  | otherwise = do
                      !bj <- MV.read sb (j - 1)
                      if ai == bj
                        then do
                          !d <- MV.read prev (j - 1)
                          MV.write cur j (d + 1)
                        else do
                          !u <- MV.read prev j
                          !l <- MV.read cur (j - 1)
                          MV.write cur j (max u l)
                      innerLoop (j + 1)
            innerLoop 1
            outerLoop cur prev (i + 1)
  outerLoop prevRow curRow 1

-- ===== Main =====

main :: IO ()
main = do
  (seqA, rng1) <- lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    lcsSeqLen lcsAlpha
  t0 <- getCurrentTime
  !score <- lcsSequential seqA seqB
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show score
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit_hs(path, input_dir, dim_rows, dim_cols):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])

    src = HS_TMPL
    src = src.replace("__N__", str(seq_len))
    src = src.replace("__ALPHA__", str(alphabet))
    src = src.replace("__SEED__", str(seed))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_wf_seq] wrote {path}  (N={seq_len}, 2-row sequential, Vector)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim-rows", type=int, default=None)
    ap.add_argument("--dim-cols", type=int, default=None)
    ap.add_argument("--dim", type=int, default=6)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
