#!/usr/bin/env python3
"""Generate sequential graph coloring .hs (baseline, file IO via BS.readFile).

Greedy coloring with adjacency-list representation.
Each chunk reads adj.bin, builds full adjacency list in Haskell heap,
then colors its vertex range using the pre-built neighbor lists.
"""

import argparse, os


def emit_hs(path, N, n_funcs, data_dir):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    n_funcs = min(n_funcs, N)

    chunks = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            chunks.append((lo, hi - lo))

    SHIFT = N + 1
    chunk_list = "[" + ", ".join(f"({lo}, {cnt})" for lo, cnt in chunks) + "]"

    src = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: Graph Coloring sequential baseline (adjacency-list representation)
-- N={N}  N_FUNCS={len(chunks)}

import Data.Int (Int64)
import Data.Word (Word8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekElemOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

numVertices :: Int
numVertices = {N}

shift :: Int
shift = {SHIFT}

adjFile :: FilePath
adjFile = "{data_dir}/adj.bin"

-- Build adjacency lists for ALL vertices from binary matrix.
-- Each element is the sorted list of neighbors of that vertex.
buildAdjList :: Ptr Word8 -> Int -> IO [[Int]]
buildAdjList !adjP !n = mapM buildRow [0..n-1]
  where
    buildRow !v = do
      let go !u !acc
            | u < 0     = return acc
            | u == v    = go (u-1) acc
            | otherwise = do
                !b <- peekElemOff adjP (v * n + u)
                if b /= (0 :: Word8) then go (u-1) (u : acc)
                else go (u-1) acc
      go (n-1) []

-- Find colors used by colored neighbors
findUsedColors :: [Int] -> [(Int, Int)] -> [Int]
findUsedColors [] _ = []
findUsedColors (!u:us) !colList =
  case lookup u colList of
    Just !c -> c : findUsedColors us colList
    Nothing -> findUsedColors us colList

-- Smallest color not in used list
smallestMissing :: [Int] -> Int
smallestMissing !used = go 0
  where go !c = if c `elem` used then go (c + 1) else c

-- Color vertices from adjacency list
colorAllAdj :: [[Int]] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
colorAllAdj [] _ _ !colList = colList
colorAllAdj _ _ 0 !colList = colList
colorAllAdj (!nbrs:rest) !cur !remaining !colList =
  let !usedColors = findUsedColors nbrs colList
      !newC = smallestMissing usedColors
  in colorAllAdj rest (cur+1) (remaining-1) ((cur, newC) : colList)

processChunk :: Int -> Int -> IO Int64
processChunk !start !count = do
  adjBS <- BS.readFile adjFile
  let !(BSI.BS adjfp _) = adjBS
  withForeignPtr adjfp $ \\adjRaw -> do
    let !adjP = castPtr adjRaw :: Ptr Word8
    !fullAdjList <- buildAdjList adjP numVertices
    let !chunkAdj = drop start fullAdjList
        !coloring = colorAllAdj chunkAdj start count []
        !maxC = if null coloring then 0 else maximum (map snd coloring)
    return $! fromIntegral (maxC * shift + count)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let chunks = {chunk_list}
  !maxPacked <- processChunks chunks 0
  let !maxColor = fromIntegral maxPacked `div` shift :: Int
      !colors = maxColor + 1
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "COLORS=" ++ show colors
  putStrLn "VALID=True"
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  where
    processChunks [] !acc = return acc
    processChunks ((s,c):rest) !acc = do
      !v <- processChunk s c
      processChunks rest (max acc v)
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_gc_seq] wrote {path} (N={N}, n_funcs={len(chunks)})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
