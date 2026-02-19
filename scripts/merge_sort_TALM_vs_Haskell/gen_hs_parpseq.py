#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate par/pseq Merge Sort .hs using STUArray (array-based)."""

import argparse, os

HS_TMPL = r"""-- Auto-generated: parallel Merge Sort (par/pseq, Array-based)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel (par, pseq)
import Control.DeepSeq (force, NFData(..))
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray_, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, listArray, elems, bounds, (!))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

instance NFData (UArray Int Int) where
  rnf a = a `seq` ()

-- In-place mergesort on STUArray, returns UArray
sortLeaf :: UArray Int Int -> Int -> Int -> UArray Int Int
sortLeaf src lo len = runSTUArray $ do
  arr <- newArray_ (0, len-1) :: ST s (STUArray s Int Int)
  tmp <- newArray_ (0, len-1) :: ST s (STUArray s Int Int)
  -- copy slice
  let copyIn i | i >= len  = return ()
               | otherwise = do writeArray arr i (src ! (lo + i)); copyIn (i+1)
  copyIn 0
  msortRec arr tmp 0 len
  return arr

msortRec :: STUArray s Int Int -> STUArray s Int Int -> Int -> Int -> ST s ()
msortRec arr tmp lo hi
  | hi - lo <= 1 = return ()
  | otherwise = do
      let mid = lo + (hi - lo) `div` 2
      msortRec arr tmp lo mid
      msortRec arr tmp mid hi
      mergeImpl arr tmp lo mid hi

mergeImpl :: STUArray s Int Int -> STUArray s Int Int -> Int -> Int -> Int -> ST s ()
mergeImpl arr tmp lo mid hi = do
  let go i j k
        | i >= mid && j >= hi = return ()
        | i >= mid  = do x <- readArray arr j; writeArray tmp k x; go i (j+1) (k+1)
        | j >= hi   = do x <- readArray arr i; writeArray tmp k x; go (i+1) j (k+1)
        | otherwise = do
            a <- readArray arr i
            b <- readArray arr j
            if a <= b
              then do writeArray tmp k a; go (i+1) j (k+1)
              else do writeArray tmp k b; go i (j+1) (k+1)
  go lo mid lo
  -- copy back
  let copy i | i >= hi   = return ()
             | otherwise = do x <- readArray tmp i; writeArray arr i x; copy (i+1)
  copy lo

-- Merge two sorted UArrays into one
mergePair :: UArray Int Int -> UArray Int Int -> UArray Int Int
mergePair !left !right = runSTUArray $ do
  let (_, lhi) = bounds left;  lsize = lhi + 1
      (_, rhi) = bounds right; rsize = rhi + 1
  merged <- newArray_ (0, lsize + rsize - 1) :: ST s (STUArray s Int Int)
  let go i j k
        | i >= lsize && j >= rsize = return ()
        | i >= lsize = do writeArray merged k (right ! j); go i (j+1) (k+1)
        | j >= rsize = do writeArray merged k (left ! i); go (i+1) j (k+1)
        | otherwise  = do
            let a = left ! i; b = right ! j
            if a <= b
              then do writeArray merged k a; go (i+1) j (k+1)
              else do writeArray merged k b; go i (j+1) (k+1)
  go 0 0 0
  return merged

-- Parallel mergesort with par/pseq
msortGo :: Int -> UArray Int Int -> Int -> Int -> UArray Int Int
msortGo cutoff src lo len
  | len <= 1      = sortLeaf src lo len
  | len <= cutoff = sortLeaf src lo len
  | otherwise =
      let mid    = len `div` 2
          leftA  = msortGo cutoff src lo mid
          rightA = msortGo cutoff src (lo + mid) (len - mid)
          goA    = force leftA
          goB    = force rightA
      in goA `par` goB `pseq` mergePair goA goB

main :: IO ()
main = do
  let n = __N__
      !v = listArray (0, n-1) [n, n-1 .. 1] :: UArray Int Int
  t0 <- getCurrentTime
  let !ys = force (msortGo __CUTOFF__ v 0 n)
  t1 <- getCurrentTime
  let secs    = realToFrac (diffUTCTime t1 t0) :: Double
      (_, hi) = bounds ys
      nout    = hi + 1
      sorted  = nout <= 1 || and [ys ! i <= ys ! (i+1) | i <- [0..nout-2]]
  putStrLn $ "SORTED=" ++ show sorted
  putStrLn $ "SORTED_HEAD=" ++ show [ys ! i | i <- [0..min 9 (nout-1)]]
  putStrLn $ "N_OUT=" ++ show nout
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""

def emit_hs(path, n, cutoff=0):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if cutoff <= 0:
        cutoff = max(256, n // 64)
    src = HS_TMPL.replace("__N__", str(n)).replace("__CUTOFF__", str(cutoff))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[hs_gen_parpseq] wrote {path} (N={n}, cutoff={cutoff})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range", "rand"])
    ap.add_argument("--cutoff", type=int, default=0, help="Parallel cutoff (0=auto: N//64)")
    args = ap.parse_args()
    emit_hs(args.out, args.N, cutoff=args.cutoff)

if __name__ == "__main__":
    main()
