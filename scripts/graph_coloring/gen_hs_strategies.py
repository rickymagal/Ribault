#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate GHC Parallel Graph Coloring using Control.Parallel.Strategies
=======================================================================

This script generates pure GHC Haskell code that implements parallel graph
coloring using GHC's spark-based parallelism (Control.Parallel.Strategies).

This serves as a baseline comparison for Ribault's dataflow parallelism.

Parallelism Model:
------------------
- Uses `parMap rdeepseq` to create sparks for parallel chunk coloring
- Sparks are hints to the GHC runtime (speculative parallelism)
- Runtime uses work-stealing to execute sparks on available capabilities

Known Limitations:
------------------
- Sparks may be garbage collected before execution (work too small)
- Sparks may "fizzle" (already evaluated when worker tries to steal)
- Sequential phases (merge, conflict resolution) limit speedup (Amdahl's Law)

To diagnose parallelism issues, run with RTS statistics:
    ./graph_color +RTS -N8 -s -RTS

Look for the SPARKS line:
    SPARKS: 16 (2 converted, 0 overflowed, 0 dud, 12 GC'd, 2 fizzled)
    - "converted": actually ran in parallel (good)
    - "GC'd": garbage collected before execution (work too small)
    - "fizzled": already evaluated when worker checked (wasted effort)

Usage:
------
    python3 gen_hs_strategies.py --out graph.hs --N 1000 --P 8 --edge-prob 0.01

Compilation:
------------
    ghc -O2 -threaded -rtsopts graph.hs -o graph

Execution:
----------
    ./graph +RTS -N8 -RTS

Author: Graph Coloring Benchmark for Ribault Project
"""

import argparse
import os

TMPL = r"""{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Parallel Graph Coloring (GHC Strategies)
-- N=__N__  P=__P__  EDGE_PROB=__EDGE_PROB__  SEED=__SEED__

import Control.Parallel.Strategies (parMap, rdeepseq, using, parList)
import Control.DeepSeq (NFData(..), deepseq, force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.Word (Word64, Word32)

-- Parameters
numVertices, numProcs, seed :: Int
numVertices = __N__
numProcs = __P__
seed = __SEED__

edgeProb :: Double
edgeProb = __EDGE_PROB__

-- Simple LCG random number generator (deterministic)
type RNG = Word64

initRNG :: Int -> RNG
initRNG s = fromIntegral s

nextRNG :: RNG -> (Double, RNG)
nextRNG !r =
  let a = 6364136223846793005 :: Word64
      c = 1442695040888963407 :: Word64
      r' = a * r + c
      val = fromIntegral (r' `shiftR` 33) / 4294967295.0
  in (val, r')

-- Generate graph as adjacency list
-- For vertex v, we only check edges to vertices > v to avoid duplicates
type Graph = IntMap IntSet

generateGraph :: Int -> Double -> Int -> Graph
generateGraph n p s = foldl' addVertex IM.empty [0..n-1]
  where
    addVertex :: Graph -> Int -> Graph
    addVertex !g !v =
      let neighbors = generateNeighbors v (initRNG (s + v * 31337))
      in IM.insert v neighbors g

    generateNeighbors :: Int -> RNG -> IntSet
    generateNeighbors v rng0 = go (v + 1) rng0 IS.empty
      where
        go !u !rng !acc
          | u >= n = acc
          | otherwise =
              let (r, rng') = nextRNG rng
              in if r < p
                 then go (u + 1) rng' (IS.insert u acc)
                 else go (u + 1) rng' acc

-- Make graph symmetric (if u->v exists, add v->u)
symmetrize :: Graph -> Graph
symmetrize g = IM.foldlWithKey' addReverse g g
  where
    addReverse :: Graph -> Int -> IntSet -> Graph
    addReverse !acc !v !neighbors =
      IS.foldl' (\a u -> IM.adjust (IS.insert v) u a) acc neighbors

-- Coloring: vertex -> color
type Coloring = IntMap Int

-- Find smallest non-negative integer not in the set
smallestMissing :: IntSet -> Int
smallestMissing s = go 0
  where
    go !c = if IS.member c s then go (c + 1) else c

-- Get neighbors of a vertex
neighbors :: Graph -> Int -> IntSet
neighbors g v = IM.findWithDefault IS.empty v g

-- Greedy color a single vertex given current coloring
greedyColor :: Graph -> Coloring -> Int -> Int
greedyColor g coloring v =
  let ns = neighbors g v
      neighborColors = IS.fromList
        [ c | u <- IS.toList ns
            , Just c <- [IM.lookup u coloring] ]
  in smallestMissing neighborColors

-- Color a chunk of vertices sequentially
colorChunk :: Graph -> Coloring -> [Int] -> Coloring
colorChunk g initial vs = foldl' colorOne initial vs
  where
    colorOne !col !v =
      let c = greedyColor g col v
      in IM.insert v c col

-- Partition list into n roughly equal chunks
partition :: Int -> [a] -> [[a]]
partition n xs = go n (length xs) xs
  where
    go _ _ [] = []
    go 1 _ ys = [ys]
    go k len ys =
      let chunkSize = (len + k - 1) `div` k
          (chunk, rest) = splitAt chunkSize ys
      in chunk : go (k - 1) (len - chunkSize) rest

-- Parallel coloring with conflict resolution
parallelColor :: Graph -> Int -> [Int] -> Coloring
parallelColor g p allVertices =
  let chunks = partition p allVertices
      -- Phase 1: Color each chunk in parallel (each starts from empty coloring)
      partials = parMap rdeepseq (colorChunk g IM.empty) chunks
                 `using` parList rdeepseq
      -- Merge all partial colorings
      merged = foldl' IM.union IM.empty partials
      -- Phase 2: Resolve conflicts (sequential pass)
  in resolveConflicts g merged allVertices

-- Resolve conflicts: re-color vertices that conflict with neighbors
resolveConflicts :: Graph -> Coloring -> [Int] -> Coloring
resolveConflicts g coloring vs = foldl' fixVertex coloring vs
  where
    fixVertex !col !v =
      let myColor = IM.findWithDefault (-1) v col
          ns = neighbors g v
          hasConflict = any (\u -> IM.lookup u col == Just myColor) (IS.toList ns)
      in if hasConflict
         then IM.insert v (greedyColor g col v) col
         else col

-- Validate coloring: no two adjacent vertices have same color
validateColoring :: Graph -> Coloring -> Bool
validateColoring g coloring = IM.foldlWithKey' checkVertex True g
  where
    checkVertex !acc !v !ns
      | not acc = False
      | otherwise =
          let myColor = IM.findWithDefault (-1) v coloring
          in all (\u -> IM.lookup u coloring /= Just myColor) (IS.toList ns)

-- Count colors used
countColors :: Coloring -> Int
countColors col = IS.size (IS.fromList (IM.elems col))

main :: IO ()
main = do
  -- Generate graph
  let !g = force (symmetrize (generateGraph numVertices edgeProb seed))
  -- Force graph evaluation before timing
  g `deepseq` return ()

  t0 <- getCurrentTime
  let !coloring = force (parallelColor g numProcs [0..numVertices-1])
  coloring `deepseq` return ()
  t1 <- getCurrentTime

  let secs = realToFrac (diffUTCTime t1 t0) :: Double
      colors = countColors coloring
      valid = validateColoring g coloring

  putStrLn $ "COLORS=" ++ show colors
  putStrLn $ "VALID=" ++ show valid
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="Output .hs file path")
    ap.add_argument("--N", type=int, required=True, help="Number of vertices")
    ap.add_argument("--P", type=int, required=True, help="Number of processors")
    ap.add_argument("--edge-prob", type=float, default=0.001, help="Edge probability")
    ap.add_argument("--seed", type=int, default=42, help="RNG seed")
    args = ap.parse_args()

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)

    src = (TMPL
           .replace("__N__", str(args.N))
           .replace("__P__", str(args.P))
           .replace("__EDGE_PROB__", str(args.edge_prob))
           .replace("__SEED__", str(args.seed)))

    with open(args.out, "w", encoding="utf-8") as f:
        f.write(src)

    print(f"[gen_hs_strategies] wrote {args.out} (N={args.N}, P={args.P}, edge_prob={args.edge_prob})")


if __name__ == "__main__":
    main()
