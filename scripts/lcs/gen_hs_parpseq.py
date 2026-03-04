#!/usr/bin/env python3
"""Generate GHC par/pseq .hs for LCS benchmark.

Same algorithm as TALM and Strategies versions.
Uses a balanced par/pseq tree for parallel reduction of chunk results.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: LCS benchmark (par/pseq)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel (par, pseq)
import Data.Bits (shiftR)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, listArray, (!))
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- ===== Constants =====

lcsTotalPairs :: Int
lcsTotalPairs = __N_PAIRS__

lcsStrLen :: Int
lcsStrLen = __STR_LEN__

lcsAlphabetSize :: Int
lcsAlphabetSize = __ALPHABET__

lcsNFuncs :: Int
lcsNFuncs = __N_FUNCS__

lcsSeed :: Integer
lcsSeed = __SEED__

-- ===== LCG PRNG (matches Python gen_input.py) =====

lcsNextRng :: Integer -> Integer
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) `mod` (2^63)

lcsSkipRng :: Integer -> Int -> Integer
lcsSkipRng !r 0 = r
lcsSkipRng !r n = lcsSkipRng (lcsNextRng r) (n - 1)

lcsGenString :: Integer -> Int -> Int -> ([Int], Integer)
lcsGenString !rng 0 _ = ([], rng)
lcsGenString !rng len_ alpha =
  let !rng' = lcsNextRng rng
      !c    = fromIntegral ((rng' `shiftR` 33) `mod` fromIntegral alpha)
      (rest, rng'') = lcsGenString rng' (len_ - 1) alpha
  in (c : rest, rng'')

-- ===== LCS DP (STUArray, zero GC pressure) =====

lcsLen :: [Int] -> [Int] -> Int
lcsLen [] _ = 0
lcsLen _ [] = 0
lcsLen xs ys = runST $ do
  let !m = length xs
      !n = length ys
      !xarr = listArray (0, m-1) xs :: UArray Int Int
      !yarr = listArray (0, n-1) ys :: UArray Int Int
  a0 <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  a1 <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  pRef <- newSTRef a0
  cRef <- newSTRef a1
  let outer !i
        | i >= m    = do p <- readSTRef pRef; readArray p n
        | otherwise = do
            p <- readSTRef pRef
            c <- readSTRef cRef
            writeArray c 0 0
            let !xi = xarr ! i
            let inner !j
                  | j >= n    = return ()
                  | otherwise = do
                      if xi == yarr ! j
                        then do !d <- readArray p j
                                writeArray c (j+1) (d + 1)
                        else do !a <- readArray p (j+1)
                                !l <- readArray c j
                                writeArray c (j+1) (max a l)
                      inner (j+1)
            inner 0
            writeSTRef pRef c
            writeSTRef cRef p
            outer (i+1)
  outer 0

-- ===== Chunk computation =====

computeChunk :: Int -> Int
computeChunk chunkIdx =
  let lo  = chunkIdx * lcsTotalPairs `div` lcsNFuncs
      hi  = (chunkIdx + 1) * lcsTotalPairs `div` lcsNFuncs
      rng0 = lcsSkipRng lcsSeed (lo * 2 * lcsStrLen)
      go !_ 0 !acc = acc
      go !rng np !acc =
        let (a, rng1) = lcsGenString rng  lcsStrLen lcsAlphabetSize
            (b, rng2) = lcsGenString rng1 lcsStrLen lcsAlphabetSize
            !l        = lcsLen a b
        in go rng2 (np - 1) (acc + l)
  in go rng0 (hi - lo) 0

-- ===== Parallel reduction via balanced par/pseq tree =====

parFoldSum :: [Int] -> Int
parFoldSum []  = 0
parFoldSum [x] = x
parFoldSum xs  =
  let (l, r) = splitAt (length xs `div` 2) xs
      sl = parFoldSum l
      sr = parFoldSum r
  in sl `par` (sr `pseq` (sl + sr))

-- ===== Main =====

main :: IO ()
main = do
  t0 <- getCurrentTime
  let chunks = map computeChunk [0 .. lcsNFuncs - 1]
      !total = parFoldSum chunks
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""


def emit_hs(path, input_dir, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        n_pairs = int(parts[0])
        str_len = int(parts[1])
        alphabet = int(parts[2])
        seed = int(parts[3])

    n_funcs = min(n_funcs, n_pairs)

    src = HS_TMPL
    src = src.replace("__N_PAIRS__", str(n_pairs))
    src = src.replace("__STR_LEN__", str(str_len))
    src = src.replace("__ALPHABET__", str(alphabet))
    src = src.replace("__N_FUNCS__", str(n_funcs))
    src = src.replace("__SEED__", str(seed))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_lcs_parpseq] wrote {path}  (n_pairs={n_pairs}, n_funcs={n_funcs})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--n-funcs", type=int, default=32)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.n_funcs)


if __name__ == "__main__":
    main()
