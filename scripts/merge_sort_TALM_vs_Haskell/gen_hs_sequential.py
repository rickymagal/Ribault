#!/usr/bin/env python3
"""Generate sequential Haskell merge sort for benchmark baseline.

Simple recursive merge sort, no parallelism, no cutoff logic.
Uses split/merge on Haskell lists.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: merge sort benchmark (sequential baseline)
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

seqLen :: Int
seqLen = __N__

split :: [Int] -> ([Int], [Int])
split []         = ([], [])
split [x]        = ([x], [])
split (x:y:rest) = let (xs, ys) = split rest in (x:xs, y:ys)

merge :: [Int] -> [Int] -> [Int]
merge [] ys        = ys
merge xs []        = xs
merge (x:xt) (y:yt)
  | x <= y         = x : merge xt (y:yt)
  | otherwise       = y : merge (x:xt) yt

mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (l, r) = split xs
                in merge (mergeSort l) (mergeSort r)

-- Strict tail-recursive verification (forces spine + checks sorted)
verify :: [Int] -> Int -> (Bool, Int)
verify xs expected = go xs 0 True minBound
  where
    go [] !cnt !ok _ = (ok && cnt == expected, cnt)
    go (x:rest) !cnt !ok !prev =
      go rest (cnt + 1) (ok && x >= prev) x

main :: IO ()
main = do
  let !input = [seqLen, seqLen - 1 .. 1]
  t0 <- getCurrentTime
  let sorted = mergeSort input
      !(ok, _) = verify sorted seqLen
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show (if ok then 1 else 0 :: Int)
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit_hs(path, input_dir):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        n = int(f.read().strip())

    src = HS_TMPL.replace("__N__", str(n))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_seq] wrote {path}  (N={n})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir)


if __name__ == "__main__":
    main()
