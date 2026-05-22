#!/usr/bin/env python3
"""Generate a monad-par LCS wavefront benchmark (.hs).

Uses Control.Monad.Par with IVars to express the wavefront dependencies
explicitly: block (i,j) waits on IVars from (i-1,j) and (i,j-1) before
firing, signals its own IVar when done. No diagonal barriers — pure
data-driven firing, conceptually equivalent to TALM's firing rule.

Compute backend: Haskell-with-raw-pointers (same as STRAT/PARPSEQ/TALM-Hs),
for fair language-level comparison.
"""

import argparse, os


HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (monad-par, IVar wavefront)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forM_, when, foldM)
import Control.Monad.Par
import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map

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

-- ===== Block computation =====
-- Same natural Haskell idiom as the Ribault Haskell super: mutable unboxed
-- vector with read/write. Identical algorithm to TALM-Hs supers_inject.hs.

computeBlock :: MV.IOVector Int -> MV.IOVector Int
             -> MV.IOVector Int -> MV.IOVector Int
             -> Int -> Int -> IO ()
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
      !hBReadBase  = bi * cols + colStart - 1
      !hBWriteBase = (bi + 1) * cols + colStart - 1
      !vBReadBase  = bj * cols
      !vBWriteBase = (bj + 1) * cols
      !sbBase      = colStart - 2
  buf1 <- MV.replicate (localCols + 1) 0
  buf2 <- MV.replicate (localCols + 1) 0
  let initPrev !lj
        | lj > localCols = return ()
        | otherwise = do
            !v <- MV.read hB (hBReadBase + lj)
            MV.write buf1 lj v
            initPrev (lj + 1)
  initPrev 0
  let outerLoop !prev !cur !i
        | i > rowEnd = do
            let writeFinal !lj
                  | lj > localCols = return ()
                  | otherwise = do
                      !v <- MV.read prev lj
                      MV.write hB (hBWriteBase + lj) v
                      writeFinal (lj + 1)
            writeFinal 0
        | otherwise = do
            !leftVal <- MV.read vB (vBReadBase + i)
            MV.write cur 0 leftVal
            !ai <- MV.read sa (i - 1)
            let innerLoop !lj
                  | lj > localCols = return ()
                  | otherwise = do
                      !bj' <- MV.read sb (sbBase + lj)
                      if ai == bj'
                        then do
                          !d <- MV.read prev (lj - 1)
                          MV.write cur lj (d + 1)
                        else do
                          !u <- MV.read prev lj
                          !l <- MV.read cur (lj - 1)
                          MV.write cur lj (max u l)
                      innerLoop (lj + 1)
            innerLoop 1
            !rightVal <- MV.read cur localCols
            MV.write vB (vBWriteBase + i) rightVal
            outerLoop cur prev (i + 1)
  outerLoop buf1 buf2 rowStart

-- ===== Wavefront via monad-par IVars =====
--
-- For each block (i,j), an IVar () serves as a "done" token. Block (i,j) is
-- forked as a Par task that:
--   1. Waits on IVars of (i-1, j) and (i, j-1) (whichever exist).
--   2. Calls computeBlock (via unsafePerformIO since computeBlock is in IO).
--   3. Puts () to its own IVar.
-- The Par scheduler distributes ready tasks across capabilities.

{-# NOINLINE evalBlock #-}
evalBlock :: MV.IOVector Int -> MV.IOVector Int -> MV.IOVector Int -> MV.IOVector Int -> Int -> Int -> ()
evalBlock sa sb hB vB bi bj = unsafePerformIO $ do
  computeBlock sa sb hB vB bi bj
  return ()

wavefront :: MV.IOVector Int -> MV.IOVector Int
          -> MV.IOVector Int -> MV.IOVector Int -> IO ()
wavefront !sa !sb !hB !vB = do
  let dr = lcsDimRows
      dc = lcsDimCols
  runParIO $ do
    -- Create an IVar for each block, indexed by (i, j).
    let mkIVarFor (acc :: Map.Map (Int,Int) (IVar ())) (i, j) = do
          v <- new
          return (Map.insert (i, j) v acc)
        coords = [(i, j) | i <- [0..dr-1], j <- [0..dc-1]]
    ivars <- foldM mkIVarFor Map.empty coords
    let getIV i j = ivars Map.! (i, j)
    -- Fork each block as a Par task that waits on its dependencies.
    forM_ coords $ \(i, j) -> fork $ do
      when (i > 0) $ get (getIV (i-1) j) >> return ()
      when (j > 0) $ get (getIV i (j-1)) >> return ()
      let !() = evalBlock sa sb hB vB i j
      put_ (getIV i j) ()
    -- Wait for the last block.
    get (getIV (dr-1) (dc-1))

-- ===== Main =====

main :: IO ()
main = do
  (seqA, rng1) <- lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    lcsSeqLen lcsAlpha
  let !cols = lcsSeqLen + 1
  hBound <- MV.replicate ((lcsDimRows + 1) * cols) 0
  vBound <- MV.replicate ((lcsDimCols + 1) * cols) 0

  t0 <- getCurrentTime
  wavefront seqA seqB hBound vBound
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  !score <- MV.read hBound (lcsDimRows * (lcsSeqLen + 1) + lcsSeqLen)
  putStrLn $ "RESULT=" ++ show (score :: Int)
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit(path, input_dir, dim_rows, dim_cols):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])
    src = (HS_TMPL
           .replace("__N__", str(seq_len))
           .replace("__ALPHA__", str(alphabet))
           .replace("__SEED__", str(seed))
           .replace("__DIM_ROWS__", str(dim_rows))
           .replace("__DIM_COLS__", str(dim_cols)))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_wf_monadpar] wrote {path}  (N={seq_len}, {dim_rows}x{dim_cols})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim-rows", type=int, required=True)
    ap.add_argument("--dim-cols", type=int, required=True)
    args = ap.parse_args()
    emit(args.out, args.input_dir, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
