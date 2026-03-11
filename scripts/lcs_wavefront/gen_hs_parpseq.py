#!/usr/bin/env python3
"""Generate manual par/pseq .hs for LCS wavefront benchmark.

Same blocked wavefront as TALM and Strategies versions. Uses raw
Control.Parallel (par/pseq) to spark blocks on each anti-diagonal,
WITHOUT the Strategies library.

Memory-efficient: stores only block boundary rows/columns instead of the
full N*N matrix.  Memory: O((DIM_ROWS + DIM_COLS) * N) instead of O(N^2).

Supports rectangular grids (DIM_ROWS x DIM_COLS).
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (manual par/pseq, boundary arrays)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel (par, pseq)
import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (callocBytes, allocaBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

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

-- ===== Block computation (boundary arrays) =====

computeBlock :: Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Int -> Int -> IO ()
computeBlock !sa !sb !hB !vB !bi !bj = do
  let !n = lcsSeqLen
      !cols = n + 1
      !chunkR = n `div` lcsDimRows
      !chunkC = n `div` lcsDimCols
      !rowStart = bi * chunkR + 1
      !rowEnd   = if bi == lcsDimRows - 1 then n else (bi + 1) * chunkR
      !colStart = bj * chunkC + 1
      !colEnd   = if bj == lcsDimCols - 1 then n else (bj + 1) * chunkC
      !localCols = colEnd - colStart + 1
      -- Pre-computed offsets
      !hBReadBase  = bi * cols + colStart - 1
      !hBWriteBase = (bi + 1) * cols + colStart - 1
      !vBReadBase  = bj * cols
      !vBWriteBase = (bj + 1) * cols
      !sbBase      = colStart - 2
      !rowBytes    = (localCols + 1) * 8
  allocaBytes rowBytes $ \buf1 ->
    allocaBytes rowBytes $ \buf2 -> do
      let initPrev !lj
            | lj > localCols = return ()
            | otherwise = do
                !v <- peekElemOff hB (hBReadBase + lj)
                pokeElemOff buf1 lj v
                initPrev (lj + 1)
      initPrev 0
      let outerLoop !prev !cur !i
            | i > rowEnd = do
                let writeFinal !lj
                      | lj > localCols = return ()
                      | otherwise = do
                          !v <- peekElemOff prev lj
                          pokeElemOff hB (hBWriteBase + lj) v
                          writeFinal (lj + 1)
                writeFinal 0
            | otherwise = do
                !leftVal <- peekElemOff vB (vBReadBase + i)
                pokeElemOff cur 0 leftVal
                !ai <- peekElemOff sa (i - 1)
                let innerLoop !lj
                      | lj > localCols = return ()
                      | otherwise = do
                          !bj' <- peekElemOff sb (sbBase + lj)
                          if ai == bj'
                            then do
                              !d <- peekElemOff prev (lj - 1)
                              pokeElemOff cur lj (d + 1)
                            else do
                              !u <- peekElemOff prev lj
                              !l <- peekElemOff cur (lj - 1)
                              pokeElemOff cur lj (max u l)
                          innerLoop (lj + 1)
                innerLoop 1
                !rightVal <- peekElemOff cur localCols
                pokeElemOff vB (vBWriteBase + i) rightVal
                outerLoop cur prev (i + 1)
      outerLoop buf1 buf2 rowStart

-- ===== Pure wrapper for par/pseq sparking =====

-- NOINLINE prevents GHC from floating out or sharing the unsafePerformIO
{-# NOINLINE evalBlock #-}
evalBlock :: Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Int -> Int -> ()
evalBlock sa sb hB vB bi bj = unsafePerformIO $ do
  computeBlock sa sb hB vB bi bj
  return ()

-- ===== Wavefront with manual par/pseq =====

-- Spark all blocks on a diagonal, then force them all via pseq
sparkBlocks :: Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> [(Int, Int)] -> ()
sparkBlocks _  _  _  _  []           = ()
sparkBlocks sa sb hB vB [(bi, bj)]   = evalBlock sa sb hB vB bi bj
sparkBlocks sa sb hB vB ((bi,bj):rest) =
  let !x = evalBlock sa sb hB vB bi bj
      !xs = sparkBlocks sa sb hB vB rest
  in x `par` (xs `pseq` x `pseq` ())

wavefront :: Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
wavefront !sa !sb !hB !vB = do
  let !dr = lcsDimRows
      !dc = lcsDimCols
  let loop !d
        | d >= dr + dc - 1 = return ()
        | otherwise = do
            let blocks = [(i, d - i) | i <- [max 0 (d - dc + 1) .. min d (dr - 1)],
                                        let j = d - i, j >= 0, j < dc]
            let !_ = sparkBlocks sa sb hB vB blocks
            loop (d + 1)
  loop 0

-- ===== Main =====

main :: IO ()
main = do
  let !n = lcsSeqLen
      !cols = n + 1
  (seqA, rng1) <- lcsGenSeq lcsSeed n lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    n lcsAlpha
  hBound <- callocBytes ((lcsDimRows + 1) * cols * 8)
  vBound <- callocBytes ((lcsDimCols + 1) * cols * 8)
  t0 <- getCurrentTime
  wavefront seqA seqB hBound vBound
  !score <- peekElemOff hBound (lcsDimRows * cols + n) :: IO Int
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

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_wf_parpseq] wrote {path}  (N={seq_len}, {dim_rows}x{dim_cols})")


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
