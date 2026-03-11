#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate parallel Merge Sort .hs using C qsort FFI + Strategies."""

import argparse, os

HS_TMPL = r"""-- Auto-generated: parallel Merge Sort (GHC Strategies, C qsort FFI)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel.Strategies (runEval, rpar, rseq)
import Control.DeepSeq (NFData(..))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek, poke, peekElemOff, pokeElemOff)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)

foreign import ccall "sort_int64" c_sort_int64 :: Ptr Int64 -> Int64 -> IO ()
foreign import ccall "merge_int64" c_merge_int64 :: Ptr Int64 -> Int64 -> Ptr Int64 -> Int64 -> Ptr Int64 -> IO ()

instance NFData (Ptr a) where
  rnf !_ = ()

arrAlloc :: Int -> IO (Ptr Int64)
arrAlloc !n = do
  raw <- mallocBytes ((n + 1) * 8)
  poke (castPtr raw :: Ptr Int64) (fromIntegral n :: Int64)
  return (raw `plusPtr` 8)
{-# INLINE arrAlloc #-}

arrSizeOf :: Ptr Int64 -> IO Int
arrSizeOf p = do
  s <- peek (p `plusPtr` (-8) :: Ptr Int64) :: IO Int64
  return (fromIntegral s)
{-# INLINE arrSizeOf #-}

fillDescending :: Ptr Int64 -> Int -> Int -> IO ()
fillDescending !p !n !i
  | i >= n    = return ()
  | otherwise = do
      pokeElemOff p i (fromIntegral (n - i) :: Int64)
      fillDescending p n (i + 1)

sortLeaf :: Ptr Int64 -> Int -> Ptr Int64
sortLeaf !src !size = unsafePerformIO $ do
  out <- arrAlloc size
  copyBytes (castPtr out) (castPtr src) (size * 8)
  c_sort_int64 out (fromIntegral size)
  return out
{-# NOINLINE sortLeaf #-}

mergePair :: Ptr Int64 -> Ptr Int64 -> Ptr Int64
mergePair !lptr !rptr = unsafePerformIO $ do
  !lsize <- arrSizeOf lptr
  !rsize <- arrSizeOf rptr
  let !total = lsize + rsize
  out <- arrAlloc total
  c_merge_int64 lptr (fromIntegral lsize) rptr (fromIntegral rsize) out
  return out
{-# NOINLINE mergePair #-}

msort :: Int -> Ptr Int64 -> Int -> Ptr Int64
msort !cutoff !ptr !size
  | size <= cutoff = sortLeaf ptr size
  | otherwise =
      let !lsize = size `div` 2
          !rsize = size - lsize
          !rptr  = ptr `plusPtr` (lsize * 8)
          leftR  = msort cutoff ptr lsize
          rightR = msort cutoff rptr rsize
      in runEval $ do
           l <- rpar leftR
           r <- rseq rightR
           _ <- rseq l
           return (mergePair l r)

verifyLoop :: Ptr Int64 -> Int -> Int -> IO Bool
verifyLoop !arr !size !i
  | i >= size = return True
  | otherwise = do
      !a <- peekElemOff arr (i-1)
      !b <- peekElemOff arr i
      if b < a then return False
               else verifyLoop arr size (i+1)

main :: IO ()
main = do
  let !n = __N__
      !cutoff = __CUTOFF__
  ptr <- mallocBytes (n * 8)
  let !p = castPtr ptr :: Ptr Int64
  fillDescending p n 0
  t0 <- getCurrentTime
  let !result = msort cutoff p n
  !rsize <- arrSizeOf result
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  !sorted <- verifyLoop result rsize 1
  let !ok = sorted && rsize == n
  putStrLn $ "SORTED=" ++ show ok
  putStrLn $ "N_OUT=" ++ show rsize
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""

MSORT_HELPERS_C = """\
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

static int cmp_int64(const void *a, const void *b) {
    int64_t va = *(const int64_t*)a;
    int64_t vb = *(const int64_t*)b;
    return (va > vb) - (va < vb);
}

void sort_int64(int64_t *arr, int64_t n) {
    qsort(arr, (size_t)n, sizeof(int64_t), cmp_int64);
}

void merge_int64(const int64_t *left, int64_t lsize,
                 const int64_t *right, int64_t rsize,
                 int64_t *out) {
    int64_t i = 0, j = 0, k = 0;
    while (i < lsize && j < rsize) {
        if (left[i] <= right[j]) out[k++] = left[i++];
        else out[k++] = right[j++];
    }
    if (i < lsize) memcpy(out + k, left + i, (lsize - i) * sizeof(int64_t));
    if (j < rsize) memcpy(out + k, right + j, (rsize - j) * sizeof(int64_t));
}
"""

def emit_hs(path, n, cutoff=0):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if cutoff <= 0:
        cutoff = max(256, n // 64)
    src = HS_TMPL.replace("__N__", str(n)).replace("__CUTOFF__", str(cutoff))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    # Write C helper next to the .hs
    c_path = os.path.join(os.path.dirname(path), "msort_helpers.c")
    with open(c_path, "w", encoding="utf-8") as fc:
        fc.write(MSORT_HELPERS_C)
    print(f"[hs_gen_input] wrote {path} (N={n}, cutoff={cutoff})")

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
