#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate sequential graph coloring baseline (no parallelism).

Uses the same LCG hash-based edge test and IntMap/IntSet greedy
algorithm as the TALM superinstruction for a fair comparison.
"""

import argparse
import os

def emit(path, n, edge_prob, seed):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    eps_int = int(edge_prob * 1000000)

    src = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: Graph Coloring Sequential Baseline
-- N={n}  EDGE_PROB={edge_prob}  SEED={seed}

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

colorAll :: IntMap IntSet -> [Int] -> IntMap Int
colorAll !adjMap = go IM.empty
  where
    go !colMap [] = colMap
    go !colMap (v:vs) =
      let ns = IM.findWithDefault IS.empty v adjMap
          usedColors = IS.fromList [c | u <- IS.toList ns, Just c <- [IM.lookup u colMap]]
          newColor = smallestMissing usedColors
      in newColor `seq` go (IM.insert v newColor colMap) vs

validateColoring :: IntMap IntSet -> IntMap Int -> Bool
validateColoring adjMap coloring = IM.foldlWithKey' chk True adjMap
  where
    chk !acc !v !ns
      | not acc   = False
      | otherwise =
          let myC = IM.findWithDefault (-1) v coloring
          in all (\\u -> IM.lookup u coloring /= Just myC) (IS.toList ns)

countColors :: IntMap Int -> Int
countColors col = IS.size (IS.fromList (IM.elems col))

main :: IO ()
main = do
  t0 <- getCurrentTime
  let !adjMap = IM.fromList [(v, buildAdj v) | v <- [0..numVertices-1]]
      !coloring = colorAll adjMap [0..numVertices-1]
      !maxColor = if IM.null coloring then 0 else maximum (IM.elems coloring)
      !nColored = IM.size coloring
  maxColor `seq` nColored `seq` return ()
  t1 <- getCurrentTime

  let secs = realToFrac (diffUTCTime t1 t0) :: Double
      colors = maxColor + 1
      valid = validateColoring adjMap coloring

  putStrLn $ "COLORS=" ++ show colors
  putStrLn $ "VALID=" ++ show valid
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_gc_seq] wrote {path} (N={n}, edge_prob={edge_prob})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--edge-prob", type=float, default=0.001)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    emit(args.out, args.N, args.edge_prob, args.seed)


if __name__ == "__main__":
    main()
