#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate GHC Parallel Graph Coloring using Control.Parallel.Strategies.

Graph generation uses the SAME LCG-hash-based edge test as the Ribault
superinstruction, ensuring identical graphs across all three systems.
"""

import argparse
import os

TMPL = r"""{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Parallel Graph Coloring (GHC Strategies)
-- N=__N__  P=__P__  EDGE_PROB=__EDGE_PROB__  SEED=__SEED__
-- Graph: LCG-hash edge test matching Ribault superinstruction

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData(..), deepseq, force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl')

-- Parameters
numVertices, numProcs, edgeProbScaled, graphSeed :: Int
numVertices = __N__
numProcs = __P__
edgeProbScaled = __EDGE_PROB_SCALED__
graphSeed = __SEED__

-- ── Graph generation (identical to Ribault superinstruction) ──

-- LCG-hash edge test: each (u,v) pair hashed independently
-- Matches: rng_seed + u * 31337 + v * 7919 → LCG → threshold
hasEdge :: Int -> Int -> Bool
hasEdge !u !v =
  let r0 = toInteger graphSeed + toInteger u * 31337 + toInteger v * 7919
      a  = 6364136223846793005 :: Integer
      c  = 1442695040888963407 :: Integer
      r' = (a * r0 + c) `mod` (2^(63 :: Int))
      rVal = (r' `div` (2^(33 :: Int))) `mod` 1000000
  in rVal < toInteger edgeProbScaled

-- Symmetric edge: matches Ribault's "hasEdge u v || hasEdge v u"
isNeighbor :: Int -> Int -> Bool
isNeighbor u v = hasEdge u v || hasEdge v u

type Graph = IntMap IntSet

buildGraph :: Int -> Graph
buildGraph n = symmetrize halfG
  where
    halfG = foldl' (\g v -> IM.insert v (fwdNs v) g) IM.empty [0..n-1]
    fwdNs v = IS.fromList [u | u <- [v+1..n-1], isNeighbor v u]
    symmetrize g = IM.foldlWithKey' addRev g g
    addRev !acc !v !ns =
      IS.foldl' (\a u -> IM.insertWith IS.union u (IS.singleton v) a) acc ns

-- ── Coloring ──

type Coloring = IntMap Int

smallestMissing :: IntSet -> Int
smallestMissing s = go 0
  where go !c = if IS.member c s then go (c + 1) else c

getNeighbors :: Graph -> Int -> IntSet
getNeighbors g v = IM.findWithDefault IS.empty v g

greedyColor :: Graph -> Coloring -> Int -> Int
greedyColor g col v =
  let ns = getNeighbors g v
      usedColors = IS.fromList
        [ c | u <- IS.toList ns, Just c <- [IM.lookup u col] ]
  in smallestMissing usedColors

colorChunk :: Graph -> [Int] -> Coloring
colorChunk g vs = foldl' colorOne IM.empty vs
  where
    colorOne !col !v = IM.insert v (greedyColor g col v) col

partitionList :: Int -> [a] -> [[a]]
partitionList _ [] = []
partitionList 1 xs = [xs]
partitionList k xs =
  let len = length xs
      sz  = (len + k - 1) `div` k
      (chunk, rest) = splitAt sz xs
  in chunk : partitionList (k - 1) rest

-- Parallel coloring: parMap over chunks, merge, resolve conflicts
parallelColor :: Graph -> Int -> [Int] -> Coloring
parallelColor g p allVs =
  let chunks   = partitionList p allVs
      partials = parMap rdeepseq (colorChunk g) chunks
      merged   = foldl' IM.union IM.empty partials
  in resolveConflicts g merged allVs

resolveConflicts :: Graph -> Coloring -> [Int] -> Coloring
resolveConflicts g col vs = foldl' fix col vs
  where
    fix !c !v =
      let myC = IM.findWithDefault (-1) v c
          ns  = getNeighbors g v
          bad = any (\u -> IM.lookup u c == Just myC) (IS.toList ns)
      in if bad then IM.insert v (greedyColor g c v) c else c

validateColoring :: Graph -> Coloring -> Bool
validateColoring g col = IM.foldlWithKey' chk True g
  where
    chk !acc !v !ns
      | not acc   = False
      | otherwise =
          let myC = IM.findWithDefault (-1) v col
          in all (\u -> IM.lookup u col /= Just myC) (IS.toList ns)

countColors :: Coloring -> Int
countColors col = IS.size (IS.fromList (IM.elems col))

main :: IO ()
main = do
  let !g = force (buildGraph numVertices)
  g `deepseq` return ()

  t0 <- getCurrentTime
  let !coloring = force (parallelColor g numProcs [0..numVertices-1])
  coloring `deepseq` return ()
  t1 <- getCurrentTime

  let secs   = realToFrac (diffUTCTime t1 t0) :: Double
      colors = countColors coloring
      valid  = validateColoring g coloring

  putStrLn $ "COLORS=" ++ show colors
  putStrLn $ "VALID=" ++ show valid
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--edge-prob", type=float, default=0.001)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    src = (TMPL
           .replace("__N__", str(args.N))
           .replace("__P__", str(args.P))
           .replace("__EDGE_PROB__", str(args.edge_prob))
           .replace("__EDGE_PROB_SCALED__", str(int(args.edge_prob * 1000000)))
           .replace("__SEED__", str(args.seed)))
    with open(args.out, "w") as f:
        f.write(src)
    print(f"[gen_hs_strategies] wrote {args.out} (N={args.N}, P={args.P}, edge_prob={args.edge_prob})")


if __name__ == "__main__":
    main()
