#!/usr/bin/env python3
"""Generate GHC Strategies .hs for LCS wavefront benchmark.

Same blocked wavefront as TALM version. Uses parMap rseq for
blocks within each anti-diagonal.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS wavefront benchmark (GHC forkIO)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, forM)
import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray)
import qualified Data.Array.ST as ST
import Control.Monad.ST (ST, runST)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

-- ===== Constants =====

lcsSeqLen :: Int
lcsSeqLen = __N__

lcsAlpha :: Int
lcsAlpha = __ALPHA__

lcsSeed :: Word64
lcsSeed = __SEED__

lcsDim :: Int
lcsDim = __DIM__

lcsIters :: Int
lcsIters = __ITERS__

-- ===== LCG PRNG =====

lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

lcsGenSeq :: Word64 -> Int -> Int -> (UArray Int Int, Word64)
lcsGenSeq !rng0 len_ alpha = runST $ do
  arr <- ST.newArray (0, len_ - 1) 0 :: ST s (STUArray s Int Int)
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha)
            ST.writeArray arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  frozen <- unsafeFreeze arr
  return (frozen, rng')

-- ===== Block computation =====

computeBlock :: UArray Int Int -> UArray Int Int -> IOUArray (Int,Int) Int
             -> Int -> Int -> IO ()
computeBlock !sa !sb !mat !bi !bj = do
  let !n = lcsSeqLen
      !chunkR = n `div` lcsDim
      !chunkC = n `div` lcsDim
      !rowStart = bi * chunkR + 1
      !rowEnd   = if bi == lcsDim - 1 then n else (bi + 1) * chunkR
      !colStart = bj * chunkC + 1
      !colEnd   = if bj == lcsDim - 1 then n else (bj + 1) * chunkC
  let doOnce = do
        let outerLoop !i
              | i > rowEnd = return ()
              | otherwise = do
                  let innerLoop !j
                        | j > colEnd = return ()
                        | otherwise = do
                            let !ai = sa ! (i - 1)
                                !bj' = sb ! (j - 1)
                            if ai == bj'
                              then do
                                !d <- readArray mat (i-1, j-1)
                                writeArray mat (i, j) (d + 1)
                              else do
                                !u <- readArray mat (i-1, j)
                                !l <- readArray mat (i, j-1)
                                writeArray mat (i, j) (max u l)
                            innerLoop (j + 1)
                  innerLoop colStart
                  outerLoop (i + 1)
        outerLoop rowStart
  let iterLoop !k
        | k >= lcsIters = return ()
        | otherwise = do
            doOnce
            iterLoop (k + 1)
  iterLoop 0

-- ===== Wavefront with forkIO + MVar =====

wavefront :: UArray Int Int -> UArray Int Int -> IOUArray (Int,Int) Int -> IO ()
wavefront !sa !sb !mat = do
  let !dim = lcsDim
  let loop !d
        | d >= 2 * dim - 1 = return ()
        | otherwise = do
            let blocks = [(i, d - i) | i <- [max 0 (d - dim + 1) .. min d (dim - 1)]]
            -- Fork one thread per block in this anti-diagonal
            dones <- forM blocks $ \(!bi, !bj) -> do
              mv <- newEmptyMVar
              forkIO $ do
                computeBlock sa sb mat bi bj
                putMVar mv ()
              return mv
            -- Wait for all blocks in this diagonal to finish
            forM_ dones takeMVar
            loop (d + 1)
  loop 0

-- ===== Main =====

main :: IO ()
main = do
  let (seqA, rng1) = lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
      (seqB, _)    = lcsGenSeq rng1    lcsSeqLen lcsAlpha
  mat <- newArray ((0,0), (lcsSeqLen, lcsSeqLen)) 0
  t0 <- getCurrentTime
  wavefront seqA seqB mat
  !score <- readArray mat (lcsSeqLen, lcsSeqLen)
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show score
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit_hs(path, input_dir, dim, iters=1):
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
    src = src.replace("__DIM__", str(dim))
    src = src.replace("__ITERS__", str(iters))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_wf_ghc] wrote {path}  (N={seq_len}, DIM={dim}, ITERS={iters})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim", type=int, default=6)
    ap.add_argument("--iters", type=int, default=1)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.dim, args.iters)


if __name__ == "__main__":
    main()
