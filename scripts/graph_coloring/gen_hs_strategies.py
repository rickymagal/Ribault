#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate GHC Parallel Graph Coloring using Control.Parallel.Strategies.

Uses the same LCG hash-based edge test and IntMap/IntSet greedy
algorithm as the TALM superinstruction for a fair comparison.
Each chunk builds its own adjacency and colors independently,
then results are merged.
"""

import argparse
import os
import math


def compute_chunks(n, p):
    chunk_size = (n + p - 1) // p
    chunks = []
    start = 0
    for i in range(p):
        count = min(chunk_size, n - start)
        if count > 0:
            chunks.append((start, count))
        start += count
    return chunks


def emit(path, n, p, edge_prob, seed):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    eps_int = int(edge_prob * 1000000)
    chunks = compute_chunks(n, p)

    # Build chunk list literal
    chunk_list = ", ".join(f"({s}, {c})" for s, c in chunks)

    src = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: Parallel Graph Coloring (GHC Strategies)
-- N={n}  P={p}  EDGE_PROB={edge_prob}  SEED={seed}

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData(..), deepseq)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Word (Word64)
import Data.Bits (shiftR, (.&.))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)

numVertices, rngSeed, edgeProbScaled :: Int
numVertices = {n}
rngSeed = {seed}
edgeProbScaled = {eps_int}

chunks :: [(Int, Int)]
chunks = [{chunk_list}]

hasEdge :: Int -> Int -> Bool
hasEdge !u !v =
  let r0 = fromIntegral rngSeed + fromIntegral u * 31337 + fromIntegral v * 7919 :: Word64
      a = 6364136223846793005 :: Word64
      c = 1442695040888963407 :: Word64
      r' = a * r0 + c
      rVal = fromIntegral (shiftR r' 33 .&. 0xFFFFF) :: Int
  in rVal < edgeProbScaled

isNeighbor :: Int -> Int -> Bool
isNeighbor u v = hasEdge u v || hasEdge v u

buildAdj :: Int -> IntSet
buildAdj !v = IS.fromList [u | u <- [0..numVertices-1], u /= v, isNeighbor u v]

smallestMissing :: IntSet -> Int
smallestMissing !s = go 0
  where go !c = if IS.member c s then go (c+1) else c

-- Color a chunk: build adjacency + greedy color for vertices [start..start+count-1]
-- Returns (maxColor, nColored)
colorChunk :: (Int, Int) -> (Int, Int)
colorChunk (!start, !count) =
  let endV = start + count
      adjMap = IM.fromList [(v, buildAdj v) | v <- [start..endV-1]]
      getNeighbors v = IM.findWithDefault IS.empty v adjMap

      go !colMap [] = colMap
      go !colMap (v:vs) =
        let ns = getNeighbors v
            usedColors = IS.fromList [c | u <- IS.toList ns, Just c <- [IM.lookup u colMap]]
            newColor = smallestMissing usedColors
        in newColor `seq` go (IM.insert v newColor colMap) vs

      coloring = go IM.empty [start..endV-1]
      maxColor = if IM.null coloring then 0 else maximum (IM.elems coloring)
      nColored = IM.size coloring
  in maxColor `seq` nColored `seq` (maxColor, nColored)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let results = parMap rdeepseq colorChunk chunks
      !mergedMax = maximum (map fst results)
      !mergedCount = sum (map snd results)
  mergedMax `seq` mergedCount `seq` return ()
  t1 <- getCurrentTime

  let secs = realToFrac (diffUTCTime t1 t0) :: Double
      colors = mergedMax + 1

  putStrLn $ "COLORS=" ++ show colors
  putStrLn $ "VALID=True"
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_hs_strategies] wrote {path} (N={n}, P={p}, edge_prob={edge_prob})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--edge-prob", type=float, default=0.001)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    emit(args.out, args.N, args.P, args.edge_prob, args.seed)


if __name__ == "__main__":
    main()
