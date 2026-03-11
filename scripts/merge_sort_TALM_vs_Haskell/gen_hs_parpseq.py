#!/usr/bin/env python3
"""Generate manual par/pseq Haskell merge sort (array-based) for benchmark.

Same algorithm as all other variants: top-down merge sort on flat C array.
Uses Control.Parallel (par, pseq) at the recursive split point.
Cutoff at n <= N/P -> sequential merge sort below.
"""

import argparse, os

HS_TMPL = r"""{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Main where

import Control.Parallel (par, pseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc (mallocBytes)
import GHC.Ptr (Ptr(..))
import GHC.IO (IO(..))
import GHC.Exts (readIntOffAddr#, writeIntOffAddr#, Int(..), Int#, Addr#, isTrue#, (+#), (-#), (<=#), quotInt#)

{-# INLINE peekI #-}
peekI :: Ptr Int -> Int -> IO Int
peekI (Ptr addr) (I# i) = IO $ \s ->
  case readIntOffAddr# addr i s of (# s', v #) -> (# s', I# v #)

{-# INLINE pokeI #-}
pokeI :: Ptr Int -> Int -> Int -> IO ()
pokeI (Ptr addr) (I# i) (I# v) = IO $ \s ->
  case writeIntOffAddr# addr i v s of s' -> (# s', () #)

seqLen :: Int
seqLen = __N__

parCutoff :: Int
parCutoff = __CUTOFF__

{-# INLINE mergeArr #-}
mergeArr :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> IO ()
mergeArr arr tmp lo mid hi = do
  let go !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do
            v <- peekI arr j
            pokeI tmp k v
            go i (j+1) (k+1)
        | j >= hi = do
            v <- peekI arr i
            pokeI tmp k v
            go (i+1) j (k+1)
        | otherwise = do
            vi <- peekI arr i
            vj <- peekI arr j
            if (vi :: Int) <= vj
              then pokeI tmp k vi >> go (i+1) j (k+1)
              else pokeI tmp k vj >> go i (j+1) (k+1)
  go lo mid lo
  let copy !k
        | k >= hi   = return ()
        | otherwise = peekI tmp k >>= pokeI arr k >> copy (k+1)
  copy lo

mergeSortSeq :: Ptr Int -> Ptr Int -> Int -> Int -> IO ()
mergeSortSeq (Ptr arr) (Ptr tmp) (I# lo) (I# hi) = sortSeqW arr tmp lo hi
{-# INLINE mergeSortSeq #-}

sortSeqW :: Addr# -> Addr# -> Int# -> Int# -> IO ()
sortSeqW arr tmp lo hi
  | isTrue# (hi -# lo <=# 1#) = return ()
  | otherwise = do
      let !mid = lo +# quotInt# (hi -# lo) 2#
      sortSeqW arr tmp lo mid
      sortSeqW arr tmp mid hi
      mergeArr (Ptr arr) (Ptr tmp) (I# lo) (I# mid) (I# hi)
{-# NOINLINE sortSeqW #-}

{-# NOINLINE mergeSortPar #-}
mergeSortPar :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> ()
mergeSortPar arr tmp n0 lo hi
  | hi - lo <= 1 = ()
  | hi - lo <= n0 `div` parCutoff = unsafePerformIO (mergeSortSeq arr tmp lo hi)
  | otherwise =
      let mid = lo + (hi - lo) `div` 2
          left  = mergeSortPar arr tmp n0 lo mid
          right = mergeSortPar arr tmp n0 mid hi
      in left `par` (right `pseq` (left `pseq`
           unsafePerformIO (mergeArr arr tmp lo mid hi)))

main :: IO ()
main = do
  let !n = seqLen
  arr <- mallocBytes (n * 8)
  tmp <- mallocBytes (n * 8)
  let fill !i
        | i >= n = return ()
        | otherwise = pokeI arr i (n - i :: Int) >> pokeI tmp i (0 :: Int) >> fill (i + 1)
  fill 0
  t0 <- getCurrentTime
  let !_ = mergeSortPar arr tmp n 0 n
  ok <- let go !i !ok_ !prev
              | i >= n    = return ok_
              | otherwise = do
                  v <- peekI arr i :: IO Int
                  go (i+1) (ok_ && v >= prev) v
         in go 0 True minBound
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show (if ok then 1 else 0 :: Int)
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit_hs(path, n, p, g):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    cutoff = p * g
    src = HS_TMPL.replace("__N__", str(n)).replace("__CUTOFF__", str(cutoff))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_parpseq] wrote {path}  (N={n}, P={p}, G={g}, cutoff=N/{cutoff})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True, help="Array size")
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--G", type=int, default=1, help="Granularity multiplier (cutoff = N/(P*G))")
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.P, args.G)


if __name__ == "__main__":
    main()
