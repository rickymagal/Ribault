{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Parallel Graph Coloring (GHC par/pseq)
-- N=450  P=8  EDGE_PROB=0.5  SEED=42
-- Graph: LCG-hash edge test matching Ribault superinstruction

import Control.Parallel (par, pseq)
import Control.DeepSeq (NFData(..), deepseq, force)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl')

-- Parameters
numVertices, numProcs, edgeProbScaled, graphSeed :: Int
numVertices = 450
numProcs = 8
edgeProbScaled = 500000
graphSeed = 42

-- ── Graph generation (identical to Ribault superinstruction) ──

hasEdge :: Int -> Int -> Bool
hasEdge !u !v =
  let r0 = toInteger graphSeed + toInteger u * 31337 + toInteger v * 7919
      a  = 6364136223846793005 :: Integer
      c  = 1442695040888963407 :: Integer
      r' = (a * r0 + c) `mod` (2^(63 :: Int))
      rVal = (r' `div` (2^(33 :: Int))) `mod` 1000000
  in rVal < toInteger edgeProbScaled

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

-- Divide-and-conquer parallel coloring using par/pseq
parallelColorDC :: Graph -> Int -> [Int] -> Coloring
parallelColorDC g threshold vs
  | length vs <= threshold = colorChunk g vs
  | otherwise =
      let mid = length vs `div` 2
          (left, right) = splitAt mid vs
          leftCol  = parallelColorDC g threshold left
          rightCol = parallelColorDC g threshold right
      in leftCol `par` (rightCol `pseq` IM.union leftCol rightCol)

parallelColor :: Graph -> Int -> [Int] -> Coloring
parallelColor g p allVs =
  let threshold = max 1 (length allVs `div` p)
      merged = parallelColorDC g threshold allVs
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
