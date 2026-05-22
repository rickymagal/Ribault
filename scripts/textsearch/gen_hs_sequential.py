#!/usr/bin/env python3
"""Generate a pure-sequential GHC text-search benchmark (standalone .hs).

This is the speedup denominator (T_seq). It performs the same work as the
parallel variants (read every file in [0, n_files) and count keyword
occurrences) but with NO parallelism: no parMap, no Strategies, no sparks,
no -threaded RTS. A single sequential foldl/strict accumulator.

Compile flags (driven by the runner): -O2 -rtsopts. Crucially:
  * NO -threaded   -> single-capability RTS, no spark machinery
  * NO -package parallel
  * NO -fPIC, NO -dynamic (matches paper's "fairest possible denominator")

Same byte-counting algorithm as gen_hs_strategies.py / gen_hs_parpseq.py
(zero-allocation matchAt loop on strict ByteStrings).
"""

import argparse, os


def emit_hs(path, n_files, keyword, corpus_dir, pad_width=None):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    abs_corpus = os.path.abspath(corpus_dir)
    kw_bytes = list(keyword.encode("ascii"))

    if pad_width is None:
        pad_width = max(4, len(str(n_files - 1)))

    hs = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: text search (pure sequential baseline)
-- N_FILES={n_files}  KEYWORD="{keyword}"

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Time.Clock (getCurrentTime, diffUTCTime)

corpusDir :: String
corpusDir = "{abs_corpus}"

kwBytes :: BS.ByteString
kwBytes = BS.pack {kw_bytes}

nFiles :: Int
nFiles = {n_files}

padInt :: Int -> String
padInt n = let s = show n in replicate ({pad_width} - length s) '0' ++ s

filePath :: Int -> FilePath
filePath i = corpusDir ++ "/file_" ++ padInt i ++ ".txt"

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

-- Pure sequential scan over all files [0, nFiles).
processAll :: IO Int
processAll = go 0 0
  where
    go :: Int -> Int -> IO Int
    go !i !acc
      | i >= nFiles = return acc
      | otherwise = do
          contents <- BS.readFile (filePath i)
          let !c = countOcc contents kwBytes
          go (i + 1) (acc + c)

main :: IO ()
main = do
  t0 <- getCurrentTime
  !total <- processAll
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hs)
    print(f"[gen_ts_seq] wrote {path} (n_files={n_files}, pure sequential)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--n-files", type=int, required=True)
    ap.add_argument("--keyword", default="FINDME")
    ap.add_argument("--corpus-dir", required=True)
    ap.add_argument("--pad-width", type=int, default=None)
    args = ap.parse_args()
    emit_hs(args.out, args.n_files, args.keyword, args.corpus_dir,
            pad_width=args.pad_width)


if __name__ == "__main__":
    main()
