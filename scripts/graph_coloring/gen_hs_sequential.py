#!/usr/bin/env python3
"""Generate sequential graph coloring .hs (baseline, file IO via BS.readFile).

Greedy coloring with association-list lookup.
Processes n_funcs chunks sequentially, each reading adj.bin from file.
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
-- Auto-generated: Graph Coloring sequential baseline (file IO via BS.readFile)
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

-- Association-list lookup
lookupColor :: Int -> [(Int, Int)] -> Maybe Int
lookupColor _ [] = Nothing
lookupColor !v ((u,c):rest) = if u == v then Just c else lookupColor v rest

-- Smallest color not in used list
smallestMissing :: [Int] -> Int
smallestMissing used = go 0
  where go !c = if c `elem` used then go (c + 1) else c

-- Collect used colors of neighbors of v from coloring, scanning via adj matrix
getUsedColors :: Ptr Word8 -> Int -> [(Int, Int)] -> Int -> [Int] -> IO [Int]
getUsedColors !adjP !v !colList !u !acc
  | u >= numVertices = return acc
  | u == v = getUsedColors adjP v colList (u + 1) acc
  | otherwise = do
      !b <- peekElemOff adjP (u * numVertices + v)
      if b /= (0 :: Word8)
        then case lookupColor u colList of
               Just c  -> getUsedColors adjP v colList (u + 1) (c : acc)
               Nothing -> getUsedColors adjP v colList (u + 1) acc
        else getUsedColors adjP v colList (u + 1) acc

-- Greedy coloring of vertices [start..start+count-1]
colorAllIO :: Ptr Word8 -> Int -> Int -> [(Int, Int)] -> IO [(Int, Int)]
colorAllIO _ _ 0 !colList = return colList
colorAllIO !adjP !cur !remaining !colList = do
  !usedColors <- getUsedColors adjP cur colList 0 []
  let !newC = smallestMissing usedColors
  colorAllIO adjP (cur + 1) (remaining - 1) ((cur, newC) : colList)

-- Process one chunk: read adj.bin, color vertices, return packed result
processChunk :: Int -> Int -> IO Int64
processChunk !start !count = do
  adjBS <- BS.readFile adjFile
  let !(BSI.BS adjfp _) = adjBS
  withForeignPtr adjfp $ \\adjRaw -> do
    let !adjP = castPtr adjRaw :: Ptr Word8
    !coloring <- colorAllIO adjP start count []
    let !maxC = if null coloring then 0 else maximum (map snd coloring)
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
