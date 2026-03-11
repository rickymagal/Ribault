#!/usr/bin/env python3
"""Generate sequential Haskell merge sort (array-based) for benchmark baseline.

Top-down merge sort on a flat C array (Ptr Int) with scratch buffer.
No parallelism.  Same algorithm as all parallel variants.
"""

import argparse, os

HS_TMPL = r"""{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
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
  mergeSortSeq arr tmp 0 n
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


def emit_hs(path, n):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    src = HS_TMPL.replace("__N__", str(n))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_seq] wrote {path}  (N={n})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True, help="Array size")
    args = ap.parse_args()
    emit_hs(args.out, args.N)


if __name__ == "__main__":
    main()
