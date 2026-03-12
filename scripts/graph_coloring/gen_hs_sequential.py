#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate sequential graph coloring baseline (no parallelism).

Same IntMap/IntSet greedy algorithm as GHC strategies, but purely
sequential: build graph, color all vertices in order, report result.
This is the shared baseline for computing speedup of all variants.
"""

import argparse
import os

TMPL = r"""{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Graph Coloring Sequential Baseline
-- N=__N__  EDGE_PROB=__EDGE_PROB__  SEED=__SEED__

import Control.DeepSeq (NFData(..), deepseq, force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl')
import Data.Bits (shiftR)
import Data.Word (Word64)
import System.IO (hFlush, stdout)

numVertices, seed :: Int
numVertices = __N__
seed = __SEED__

edgeProb :: Double
edgeProb = __EDGE_PROB__

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

type Graph = IntMap IntSet

generateGraph :: Int -> Double -> Int -> Graph
generateGraph n p s = foldl' addVertex IM.empty [0..n-1]
  where
    addVertex !g !v =
      let ns = generateNeighbors v (initRNG (s + v * 31337))
      in IM.insert v ns g
    generateNeighbors v rng0 = go (v + 1) rng0 IS.empty
      where
        go !u !rng !acc
          | u >= n = acc
          | otherwise =
              let (r, rng') = nextRNG rng
              in if r < p
                 then go (u + 1) rng' (IS.insert u acc)
                 else go (u + 1) rng' acc

symmetrize :: Graph -> Graph
symmetrize g = IM.foldlWithKey' addReverse g g
  where
    addReverse !acc !v !ns =
      IS.foldl' (\a u -> IM.adjust (IS.insert v) u a) acc ns

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

-- Color ALL vertices sequentially (single pass, no chunking)
colorAll :: Graph -> [Int] -> Coloring
colorAll g vs = foldl' colorOne IM.empty vs
  where
    colorOne !col !v = IM.insert v (greedyColor g col v) col

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
  let !g = force (symmetrize (generateGraph numVertices edgeProb seed))
  g `deepseq` return ()

  t0 <- getCurrentTime
  let !coloring = force (colorAll g [0..numVertices-1])
  coloring `deepseq` return ()
  t1 <- getCurrentTime

  let secs = realToFrac (diffUTCTime t1 t0) :: Double
      colors = countColors coloring
      valid = validateColoring g coloring

  putStrLn $ "COLORS=" ++ show colors
  putStrLn $ "VALID=" ++ show valid
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--edge-prob", type=float, default=0.001)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    src = (TMPL
           .replace("__N__", str(args.N))
           .replace("__EDGE_PROB__", str(args.edge_prob))
           .replace("__SEED__", str(args.seed)))
    with open(args.out, "w") as f:
        f.write(src)
    print(f"[gen_gc_seq] wrote {args.out} (N={args.N}, edge_prob={args.edge_prob})")


if __name__ == "__main__":
    main()
