#!/usr/bin/env python3
"""Generate a GHC par/pseq text-search benchmark (standalone .hs).

Equivalent structure to TALM: K independent range tasks, each processing
a contiguous range of files sequentially.  Top-level parallelism via
explicit par/pseq.  Each range task uses unsafePerformIO for readFile —
the standard GHC approach for parallel IO.
"""

import argparse, os


def emit_hs(path, n_files, keyword, corpus_dir, n_funcs=12):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    abs_corpus = os.path.abspath(corpus_dir)
    kw_bytes = list(keyword.encode("ascii"))

    n_funcs = min(n_funcs, n_files, 14)
    ranges = []
    for i in range(n_funcs):
        lo = i * n_files // n_funcs
        hi = (i + 1) * n_files // n_funcs
        if hi > lo:
            ranges.append((lo, hi))

    range_list = "[" + ", ".join(f"({lo}, {hi})" for lo, hi in ranges) + "]"

    hs = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: text search (GHC par/pseq)
-- N_FILES={n_files}  KEYWORD="{keyword}"  N_FUNCS={len(ranges)}
-- Uses explicit par/pseq for top-level parallelism over K range tasks.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Parallel (par, pseq)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

corpusDir :: String
corpusDir = "{abs_corpus}"

kwBytes :: BS.ByteString
kwBytes = BS.pack {kw_bytes}

padInt :: Int -> Int -> String
padInt w n = let s = show n in replicate (w - length s) '0' ++ s

filePath :: Int -> FilePath
filePath i = corpusDir ++ "/file_" ++ padInt 4 i ++ ".txt"

countOcc :: BS.ByteString -> BS.ByteString -> Int
countOcc buf kw = go 0 0
  where
    kwLen  = BS.length kw
    bufLen = BS.length buf
    end    = bufLen - kwLen + 1
    go !i !acc
      | i >= end    = acc
      | matchAt i 0 = go (i + kwLen) (acc + 1)
      | otherwise   = go (i + 1) acc
    matchAt !off !k
      | k >= kwLen = True
      | BSU.unsafeIndex buf (off + k) /= BSU.unsafeIndex kw k = False
      | otherwise  = matchAt off (k + 1)

-- Process a range of files [lo, hi), returning sum of keyword counts.
-- Uses unsafePerformIO so it can be used with par/pseq (pure interface).
processRange :: (Int, Int) -> Int
processRange (lo, hi) = unsafePerformIO $ go lo 0
  where
    go :: Int -> Int -> IO Int
    go !i !acc
      | i >= hi   = return acc
      | otherwise = do
          contents <- BS.readFile (filePath i)
          let !c = countOcc contents kwBytes
          go (i + 1) (acc + c)

-- Parallel map using par/pseq.
parMapPP :: (a -> b) -> [a] -> [b]
parMapPP _ []     = []
parMapPP f (x:xs) = let r = f x; rs = parMapPP f xs
                    in r `par` rs `pseq` (r : rs)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let ranges = {range_list}
      results = parMapPP processRange ranges
      !total = sum results
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hs)
    print(f"[gen_ts_parpseq] wrote {path} (n_files={n_files}, n_funcs={len(ranges)})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--n-files", type=int, required=True)
    ap.add_argument("--keyword", default="FINDME")
    ap.add_argument("--corpus-dir", required=True)
    ap.add_argument("--n-funcs", type=int, default=12)
    args = ap.parse_args()
    emit_hs(args.out, args.n_files, args.keyword, args.corpus_dir, args.n_funcs)


if __name__ == "__main__":
    main()
