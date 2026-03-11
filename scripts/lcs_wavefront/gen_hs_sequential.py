#!/usr/bin/env python3
"""Generate pure sequential Haskell for LCS wavefront benchmark.

Uses a simple 2-row DP technique (prev_row / curr_row).
No blocking, no wavefront — just the standard LCS recurrence
row by row.  Memory: 2 * (N+1) * 8 bytes.

Used as the P=1 baseline for speedup calculation.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (sequential baseline, 2-row)
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Storable (peekElemOff, pokeElemOff)
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

lcsGenSeq :: Word64 -> Int -> Int -> IO (Ptr Int, Word64)
lcsGenSeq !rng0 len_ alpha = do
  arr <- callocBytes (len_ * 8)
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha) :: Int
            pokeElemOff arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  return (arr, rng')

-- ===== 2-row sequential LCS =====

lcsSequential :: Ptr Int -> Ptr Int -> IO Int
lcsSequential !sa !sb = do
  let !n = lcsSeqLen
      !cols = n + 1
  prevRow <- callocBytes (cols * 8)   -- zeroed
  curRow  <- callocBytes (cols * 8)   -- zeroed
  let outerLoop !prev !cur !i
        | i > n = do
            !score <- peekElemOff prev n
            free prev
            free cur
            return score
        | otherwise = do
            pokeElemOff cur 0 (0 :: Int)
            !ai <- peekElemOff sa (i - 1)
            let innerLoop !j
                  | j > n = return ()
                  | otherwise = do
                      !bj <- peekElemOff sb (j - 1)
                      if ai == bj
                        then do
                          !d <- peekElemOff prev (j - 1)
                          pokeElemOff cur j (d + 1)
                        else do
                          !u <- peekElemOff prev j
                          !l <- peekElemOff cur (j - 1)
                          pokeElemOff cur j (max u l)
                      innerLoop (j + 1)
            innerLoop 1
            outerLoop cur prev (i + 1)   -- swap: cur becomes prev
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
    print(f"[gen_lcs_wf_seq] wrote {path}  (N={seq_len}, 2-row sequential)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    # Accept but ignore dim args (kept for CLI compatibility with run_validated.sh)
    ap.add_argument("--dim-rows", type=int, default=None)
    ap.add_argument("--dim-cols", type=int, default=None)
    ap.add_argument("--dim", type=int, default=6)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
