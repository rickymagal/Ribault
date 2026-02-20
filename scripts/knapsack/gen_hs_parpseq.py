#!/usr/bin/env python3
"""Generate a GHC par/pseq 0/1 knapsack brute-force benchmark (standalone .hs).

Equivalent structure to TALM: K independent range tasks, each searching
a contiguous range of subset indices.  Top-level parallelism via
explicit par/pseq.  Uses identical list-based algorithm.
"""

import argparse, os


def emit_hs(path, items_dir, n_items, n_funcs=14):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    # Read items and capacity
    items = []
    with open(os.path.join(items_dir, "items.txt")) as f:
        for line in f:
            w, v = line.split()
            items.append((int(w), int(v)))
    with open(os.path.join(items_dir, "capacity.txt")) as f:
        capacity = int(f.read().strip())

    total_subsets = 2 ** n_items
    n_funcs = min(n_funcs, total_subsets)

    items_literal = "[" + ", ".join(f"({w},{v})" for w, v in items) + "]"

    hs = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: 0/1 Knapsack brute-force (GHC par/pseq)
-- N_ITEMS={n_items}  CAPACITY={capacity}  TOTAL_SUBSETS={total_subsets}
-- Uses explicit par/pseq for top-level parallelism over K range tasks.

import Control.Parallel (par, pseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

ksItems :: [(Int, Int)]
ksItems = {items_literal}

ksCapacity :: Int
ksCapacity = {capacity}

ksTotalSubsets :: Int
ksTotalSubsets = {total_subsets}

ksNFuncs :: Int
ksNFuncs = {n_funcs}

-- Evaluate one subset by walking bits against the item list.
-- Deliberately list-based for allocation pressure.
ksEvalSubset :: Int -> Int
ksEvalSubset s = go ksItems s 0 0
  where
    go [] _ !tw !tv
      | tw <= ksCapacity = tv
      | otherwise        = 0
    go _ 0 !tw !tv
      | tw <= ksCapacity = tv
      | otherwise        = 0
    go ((w,v):rest) !bits !tw !tv
      | bits `mod` 2 == 1 = go rest (bits `div` 2) (tw+w) (tv+v)
      | otherwise          = go rest (bits `div` 2) tw tv

-- Search range [lo, hi), return max value found.
ksSearchRange :: Int -> Int -> Int
ksSearchRange lo hi = go lo 0
  where
    go !i !best
      | i >= hi   = best
      | otherwise = let !v = ksEvalSubset i
                        !b = if v > best then v else best
                    in go (i + 1) b

-- Compute lo/hi from chunk index, then search.
ksSearchChunk :: Int -> Int
ksSearchChunk idx =
  let lo = idx * ksTotalSubsets `div` ksNFuncs
      hi = (idx + 1) * ksTotalSubsets `div` ksNFuncs
  in ksSearchRange lo hi

-- Parallel map using par/pseq.
parMapPP :: (a -> b) -> [a] -> [b]
parMapPP _ []     = []
parMapPP f (x:xs) = let r = f x; rs = parMapPP f xs
                    in r `par` rs `pseq` (r : rs)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let chunks = [0 .. ksNFuncs - 1]
      results = parMapPP ksSearchChunk chunks
      !best = maximum results
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show best
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hs)
    print(f"[gen_ks_parpseq] wrote {path} (n_items={n_items}, n_funcs={n_funcs})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--items-dir", required=True)
    ap.add_argument("--n-items", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    args = ap.parse_args()
    emit_hs(args.out, args.items_dir, args.n_items, args.n_funcs)


if __name__ == "__main__":
    main()
