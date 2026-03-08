#!/usr/bin/env python3
"""Generate manual par/pseq Haskell merge sort (array-based) for benchmark.

Same algorithm as all other variants: top-down merge sort on flat C array.
Uses Control.Parallel (par, pseq) at the recursive split point.
Cutoff at n <= N/P -> sequential merge sort below.
"""

import argparse, os

HS_TMPL = r"""{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel (par, pseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)

seqLen :: Int
seqLen = __N__

parP :: Int
parP = __P__

mergeArr :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> IO ()
mergeArr arr tmp lo mid hi = do
  let go !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do
            v <- peekElemOff arr j
            pokeElemOff tmp k v
            go i (j+1) (k+1)
        | j >= hi = do
            v <- peekElemOff arr i
            pokeElemOff tmp k v
            go (i+1) j (k+1)
        | otherwise = do
            vi <- peekElemOff arr i
            vj <- peekElemOff arr j
            if (vi :: Int) <= vj
              then pokeElemOff tmp k vi >> go (i+1) j (k+1)
              else pokeElemOff tmp k vj >> go i (j+1) (k+1)
  go lo mid lo
  let copy !k
        | k >= hi = return ()
        | otherwise = peekElemOff tmp k >>= pokeElemOff arr k >> copy (k+1)
  copy lo

mergeSortSeq :: Ptr Int -> Ptr Int -> Int -> Int -> IO ()
mergeSortSeq arr tmp lo hi
  | hi - lo <= 1 = return ()
  | otherwise = do
      let mid = lo + (hi - lo) `div` 2
      mergeSortSeq arr tmp lo mid
      mergeSortSeq arr tmp mid hi
      mergeArr arr tmp lo mid hi

{-# NOINLINE mergeSortPar #-}
mergeSortPar :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> Int -> ()
mergeSortPar arr tmp n0 p lo hi
  | hi - lo <= 1 = ()
  | hi - lo <= n0 `div` p = unsafePerformIO (mergeSortSeq arr tmp lo hi)
  | otherwise =
      let mid = lo + (hi - lo) `div` 2
          left  = mergeSortPar arr tmp n0 p lo mid
          right = mergeSortPar arr tmp n0 p mid hi
      in left `par` (right `pseq` (left `pseq`
           unsafePerformIO (mergeArr arr tmp lo mid hi)))

main :: IO ()
main = do
  let !n = seqLen
  arr <- mallocBytes (n * 8)
  tmp <- mallocBytes (n * 8)
  let fill !i
        | i >= n = return ()
        | otherwise = pokeElemOff arr i (n - i :: Int) >> fill (i + 1)
  fill 0
  t0 <- getCurrentTime
  let !_ = mergeSortPar arr tmp n parP 0 n
  ok <- let go !i !ok_ !prev
              | i >= n    = return ok_
              | otherwise = do
                  v <- peekElemOff arr i :: IO Int
                  go (i+1) (ok_ && v >= prev) v
         in go 0 True minBound
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show (if ok then 1 else 0 :: Int)
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit_hs(path, input_dir, p):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(os.path.join(input_dir, "params.txt")) as f:
        n = int(f.read().strip())
    src = HS_TMPL.replace("__N__", str(n)).replace("__P__", str(p))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_parpseq] wrote {path}  (N={n}, P={p})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--P", type=int, required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.P)


if __name__ == "__main__":
    main()
