#!/usr/bin/env python3
"""Generate GHC Strategies .hs for 0/1 Knapsack via parallel merge-sort aggregation.

Same algorithm as TALM version: enumerate all 2^N subsets, sort values
descending with parallel merge sort, take head = optimal.
Uses STUArray for efficient array-based sorting (same as merge sort benchmark).
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: 0/1 Knapsack via parallel merge sort (GHC Strategies)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel.Strategies (runEval, rpar, rseq)
import Control.DeepSeq (force, NFData(..))
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray_, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, listArray, elems, bounds, (!))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

instance NFData (UArray Int Int) where
  rnf a = a `seq` ()

items :: [(Int, Int)]
items = __ITEMS__

capacity :: Int
capacity = __CAPACITY__

totalSubsets :: Int
totalSubsets = __TOTAL_SUBSETS__

-- Evaluate one subset (identical algorithm to TALM)
evalSubset :: Int -> Int
evalSubset s = go items s 0 0
  where
    go [] _ !tw !tv | tw <= capacity = tv | otherwise = 0
    go _ 0 !tw !tv | tw <= capacity = tv | otherwise = 0
    go ((w,v):rest) !bits !tw !tv
      | bits `mod` 2 == 1 = go rest (bits `div` 2) (tw+w) (tv+v)
      | otherwise = go rest (bits `div` 2) tw tv

-- In-place mergesort on STUArray (DESCENDING), returns UArray
sortLeaf :: UArray Int Int -> Int -> Int -> UArray Int Int
sortLeaf src lo len = runSTUArray $ do
  arr <- newArray_ (0, len-1) :: ST s (STUArray s Int Int)
  tmp <- newArray_ (0, len-1) :: ST s (STUArray s Int Int)
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
            if a >= b  -- DESCENDING
              then do writeArray tmp k a; go (i+1) j (k+1)
              else do writeArray tmp k b; go i (j+1) (k+1)
  go lo mid lo
  let copy i | i >= hi   = return ()
             | otherwise = do x <- readArray tmp i; writeArray arr i x; copy (i+1)
  copy lo

-- Merge two sorted (descending) UArrays into one
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
            if a >= b  -- DESCENDING
              then do writeArray merged k a; go (i+1) j (k+1)
              else do writeArray merged k b; go i (j+1) (k+1)
  go 0 0 0
  return merged

-- Parallel mergesort with Strategies (DESCENDING)
msortGo :: Int -> UArray Int Int -> Int -> Int -> UArray Int Int
msortGo cutoff src lo len
  | len <= 1  = sortLeaf src lo len
  | len <= cutoff = sortLeaf src lo len
  | otherwise =
      let mid    = len `div` 2
          leftA  = msortGo cutoff src lo mid
          rightA = msortGo cutoff src (lo + mid) (len - mid)
      in runEval $ do
           a' <- rpar (force leftA)
           b' <- rseq (force rightA)
           _ <- rseq a'
           return (mergePair a' b')

main :: IO ()
main = do
  let n = totalSubsets
      !v = listArray (0, n-1) [evalSubset i | i <- [0..n-1]] :: UArray Int Int
  t0 <- getCurrentTime
  let !ys = force (msortGo __CUTOFF__ v 0 n)
  t1 <- getCurrentTime
  let secs    = realToFrac (diffUTCTime t1 t0) :: Double
      maxVal  = if n > 0 then ys ! 0 else 0  -- head of descending sort
  putStrLn $ "RESULT=" ++ show maxVal
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""


def emit_hs(path, items_dir, n_items, nparts):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    items = []
    with open(os.path.join(items_dir, "items.txt")) as f:
        for line in f:
            w, v = line.split()
            items.append((int(w), int(v)))
    with open(os.path.join(items_dir, "capacity.txt")) as f:
        capacity = int(f.read().strip())

    total_subsets = 2 ** n_items
    cutoff = max(256, total_subsets // nparts)
    items_literal = "[" + ", ".join(f"({w},{v})" for w, v in items) + "]"

    src = HS_TMPL.replace("__ITEMS__", items_literal)
    src = src.replace("__CAPACITY__", str(capacity))
    src = src.replace("__TOTAL_SUBSETS__", str(total_subsets))
    src = src.replace("__CUTOFF__", str(cutoff))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ks_ms_ghc] wrote {path}  (n_items={n_items}, total={total_subsets}, cutoff={cutoff})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--items-dir", required=True)
    ap.add_argument("--n-items", type=int, required=True)
    ap.add_argument("--nparts", type=int, default=64)
    args = ap.parse_args()
    emit_hs(args.out, args.items_dir, args.n_items, args.nparts)


if __name__ == "__main__":
    main()
