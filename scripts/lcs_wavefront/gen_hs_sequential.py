#!/usr/bin/env python3
"""Generate pure sequential Haskell for LCS wavefront benchmark.

Same blocked wavefront computation as all other variants, but
purely sequential — no threading, no parallelism framework.
Used as the P=1 baseline for speedup calculation.

Supports rectangular grids (DIM_ROWS × DIM_COLS).
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (sequential baseline)
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (callocBytes)
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

lcsDimRows :: Int
lcsDimRows = __DIM_ROWS__

lcsDimCols :: Int
lcsDimCols = __DIM_COLS__

lcsStride :: Int
lcsStride = __STRIDE__

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

-- ===== Block computation =====

computeBlock :: Ptr Int -> Ptr Int -> Ptr Int -> Int -> Int -> IO ()
computeBlock !sa !sb !mat !bi !bj = do
  let !n = lcsSeqLen
      !str = lcsStride
      !chunkR = n `div` lcsDimRows
      !chunkC = n `div` lcsDimCols
      !rowStart = bi * chunkR + 1
      !rowEnd   = if bi == lcsDimRows - 1 then n else (bi + 1) * chunkR
      !colStart = bj * chunkC + 1
      !colEnd   = if bj == lcsDimCols - 1 then n else (bj + 1) * chunkC
  let outerLoop !i
        | i > rowEnd = return ()
        | otherwise = do
            !ai <- peekElemOff sa (i - 1)
            let innerLoop !j
                  | j > colEnd = return ()
                  | otherwise = do
                      !bj' <- peekElemOff sb (j - 1)
                      if ai == bj'
                        then do
                          !d <- peekElemOff mat ((i-1)*str + (j-1))
                          pokeElemOff mat (i*str + j) (d + 1)
                        else do
                          !u <- peekElemOff mat ((i-1)*str + j)
                          !l <- peekElemOff mat (i*str + (j-1))
                          pokeElemOff mat (i*str + j) (max u l)
                      innerLoop (j + 1)
            innerLoop colStart
            outerLoop (i + 1)
  outerLoop rowStart

-- ===== Sequential wavefront (no threading) =====

wavefront :: Ptr Int -> Ptr Int -> Ptr Int -> IO ()
wavefront !sa !sb !mat = do
  let !dr = lcsDimRows
      !dc = lcsDimCols
  let loop !d
        | d >= dr + dc - 1 = return ()
        | otherwise = do
            let go !i
                  | i > min d (dr - 1) = return ()
                  | otherwise = do
                      let !j = d - i
                      if j >= 0 && j < dc
                        then computeBlock sa sb mat i j
                        else return ()
                      go (i + 1)
            go (max 0 (d - dc + 1))
            loop (d + 1)
  loop 0

-- ===== Main =====

main :: IO ()
main = do
  (seqA, rng1) <- lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    lcsSeqLen lcsAlpha
  mat <- callocBytes (lcsStride * lcsStride * 8)
  t0 <- getCurrentTime
  wavefront seqA seqB mat
  !score <- peekElemOff mat (lcsSeqLen * lcsStride + lcsSeqLen) :: IO Int
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
    src = src.replace("__DIM_ROWS__", str(dim_rows))
    src = src.replace("__DIM_COLS__", str(dim_cols))
    src = src.replace("__STRIDE__", str(seq_len + 1))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_wf_seq] wrote {path}  (N={seq_len}, {dim_rows}x{dim_cols})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim-rows", type=int, default=None)
    ap.add_argument("--dim-cols", type=int, default=None)
    ap.add_argument("--dim", type=int, default=6)
    args = ap.parse_args()
    dim_rows = args.dim_rows if args.dim_rows is not None else args.dim
    dim_cols = args.dim_cols if args.dim_cols is not None else args.dim
    emit_hs(args.out, args.input_dir, dim_rows, dim_cols)


if __name__ == "__main__":
    main()
