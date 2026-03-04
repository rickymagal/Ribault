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
import Data.List (foldl')
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

-- ===== LCS DP (same algorithm as TALM inject) =====

lcsLen :: [Int] -> [Int] -> Int
lcsLen [] _ = 0
lcsLen _ [] = 0
lcsLen xs ys =
  let !n  = length ys
      row0 = replicate (n + 1) (0 :: Int)
      step prev x = scanl f 0 (zip ys (zip prev (tail prev)))
        where f !left (y, (diag, above))
                | x == y    = diag + 1
                | otherwise = max left above
  in last (foldl' step row0 xs)

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
