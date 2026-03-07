#!/usr/bin/env python3
"""Generate GHC Strategies Haskell merge sort for benchmark.

Same algorithm as the TALM version: recursive merge sort with
splitCount for size tracking, cutoff at n <= n0/p, then
sequential merge sort below the cutoff.

Uses Control.Parallel.Strategies (Eval monad, rpar/rseq) with
Control.DeepSeq (force) to deeply evaluate each subtree.
"""

import argparse, os

HS_TMPL = r"""-- Auto-generated: merge sort benchmark (GHC Strategies)
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Parallel.Strategies (runEval, rpar, rseq)
import Control.DeepSeq (force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

seqLen :: Int
seqLen = __N__

parP :: Int
parP = __P__

split :: [Int] -> ([Int], [Int])
split []         = ([], [])
split [x]        = ([x], [])
split (x:y:rest) = let (xs, ys) = split rest in (x:xs, y:ys)

splitCount :: [Int] -> ([Int], [Int], Int, Int)
splitCount []         = ([], [], 0, 0)
splitCount [x]        = ([x], [], 1, 0)
splitCount (x:y:rest) = let (xs, ys, nl, nr) = splitCount rest
                         in (x:xs, y:ys, nl + 1, nr + 1)

merge :: [Int] -> [Int] -> [Int]
merge [] ys        = ys
merge xs []        = xs
merge (x:xt) (y:yt)
  | x <= y         = x : merge xt (y:yt)
  | otherwise       = y : merge (x:xt) yt

-- Sequential merge sort (below cutoff)
msSeq :: [Int] -> [Int]
msSeq []  = []
msSeq [x] = [x]
msSeq xs  = let (l, r) = split xs
            in merge (msSeq l) (msSeq r)

-- Parallel merge sort with Strategies (Eval monad)
mergeSortStrat :: Int -> Int -> [Int] -> [Int]
mergeSortStrat _  _ []  = []
mergeSortStrat _  _ [x] = [x]
mergeSortStrat n0 n xs
  | n <= n0 `div` parP = msSeq xs
  | otherwise =
      let (l, r, nl, nr) = splitCount xs
      in runEval $ do
        sl <- rpar (force (mergeSortStrat n0 nl l))
        sr <- rseq (force (mergeSortStrat n0 nr r))
        return (merge sl sr)

-- Strict tail-recursive verification
verify :: [Int] -> Int -> (Bool, Int)
verify xs expected = go xs 0 True minBound
  where
    go [] !cnt !ok _ = (ok && cnt == expected, cnt)
    go (x:rest) !cnt !ok !prev =
      go rest (cnt + 1) (ok && x >= prev) x

main :: IO ()
main = do
  let !input = [seqLen, seqLen - 1 .. 1]
      !n0 = seqLen
  t0 <- getCurrentTime
  let sorted = mergeSortStrat n0 n0 input
      !(ok, _) = verify sorted seqLen
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

    src = HS_TMPL
    src = src.replace("__N__", str(n))
    src = src.replace("__P__", str(p))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_strat] wrote {path}  (N={n}, P={p})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--P", type=int, required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.input_dir, args.P)


if __name__ == "__main__":
    main()
