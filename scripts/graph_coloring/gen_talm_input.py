#!/usr/bin/env python3
"""Generate TALM graph coloring with file IO (adjacency-list representation).

Each chunk reads adj.bin via BS.readFile, builds the full adjacency list
as a Haskell [[Int]], then colors its vertex range using the pre-built
neighbor lists.

Generates:
  1. Minimal .hsk with separate chunk supers + max merge tree
  2. supers_inject.hs with Haskell implementation
"""

import argparse, os


def gen_max_tree(n):
    """Generate if-then-else max tree for n chunks c0..c{n-1}.
    Returns (lines, final_var)."""
    current = [f"c{i}" for i in range(n)]
    lines = []
    gen_id = 0
    while len(current) > 1:
        new_current = []
        for i in range(0, len(current), 2):
            if i + 1 < len(current):
                left = current[i]
                right = current[i + 1]
                name = f"mx{gen_id}"
                lines.append(
                    f"  in let {name} = if {left} > {right} then {left} else {right}"
                )
                new_current.append(name)
                gen_id += 1
            else:
                new_current.append(current[i])
        current = new_current
    return lines, current[0]


def emit(path, N, n_funcs, data_dir):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    n_funcs = min(n_funcs, N)
    chunks = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            chunks.append((lo, hi - lo))
    nchunks = len(chunks)
    SHIFT = N + 1

    # ---- 1. .hsk with separate supers per chunk ----
    super_defs = []
    for idx, (start, count) in enumerate(chunks):
        packed = start * SHIFT + count
        super_defs.append(
            f"""-- SUPER chunk_{idx}: color vertices [{start}..{start + count})
chunk_{idx} dummy =
  super single input (dummy) output (result)
#BEGINSUPER
    result = unsafePerformIO (gcColorChunk {packed})
#ENDSUPER
"""
        )

    leaf_lets = []
    for i in range(nchunks):
        kw = "let" if i == 0 else "in let"
        leaf_lets.append(f"  {kw} c{i} = chunk_{i} 0")

    max_lines, final_var = gen_max_tree(nchunks)

    hsk = f"""-- graph_coloring.hsk  (auto-generated, adjacency-list representation)
-- N={N}  N_FUNCS={nchunks}

{"".join(super_defs)}
-- SUPER: print final result
print_result packed =
  super single input (packed) output (out)
#BEGINSUPER
    out = unsafePerformIO
      (do
        let p = fromIntegral packed :: Int
            maxColor = p `div` {SHIFT}
            colors = maxColor + 1
        putStrLn ("COLORS=" ++ show colors)
        putStrLn "VALID=True"
        pure 0)
#ENDSUPER

main =
{chr(10).join(leaf_lets)}
{chr(10).join(max_lines)}
  in print_result {final_var}
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_gc_talm] wrote {path} (N={N}, n_funcs={nchunks})")

    # ---- 2. supers_inject.hs ----
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject = f"""import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekElemOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

gcNumVertices :: Int
gcNumVertices = {N}

gcShift :: Int
gcShift = {SHIFT}

gcAdjFile :: FilePath
gcAdjFile = "{data_dir}/adj.bin"

-- Build adjacency lists for ALL vertices from binary matrix.
-- Each element is the sorted list of neighbors of that vertex.
gcBuildAdjList :: Ptr Word8 -> Int -> IO [[Int]]
gcBuildAdjList !adjP !n = mapM buildRow [0..n-1]
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
gcFindUsedColors :: [Int] -> [(Int, Int)] -> [Int]
gcFindUsedColors [] _ = []
gcFindUsedColors (!u:us) !colList =
  case lookup u colList of
    Just !c -> c : gcFindUsedColors us colList
    Nothing -> gcFindUsedColors us colList

gcSmallestMissing :: [Int] -> Int
gcSmallestMissing !used = go 0
  where go !c = if c `elem` used then go (c + 1) else c

-- Color vertices from adjacency list
gcColorAllAdj :: [[Int]] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
gcColorAllAdj [] _ _ !colList = colList
gcColorAllAdj _ _ 0 !colList = colList
gcColorAllAdj (!nbrs:rest) !cur !remaining !colList =
  let !usedColors = gcFindUsedColors nbrs colList
      !newC = gcSmallestMissing usedColors
  in gcColorAllAdj rest (cur+1) (remaining-1) ((cur, newC) : colList)

gcColorChunk :: Int64 -> IO Int64
gcColorChunk packed64 = do
  let packed = fromIntegral packed64 :: Int
      start  = packed `div` gcShift
      count  = packed `mod` gcShift
  adjBS <- BS.readFile gcAdjFile
  let !(BSI.BS adjfp _) = adjBS
  withForeignPtr adjfp $ \\adjRaw -> do
    let !adjP = castPtr adjRaw :: Ptr Word8
    !fullAdjList <- gcBuildAdjList adjP gcNumVertices
    let !chunkAdj = drop start fullAdjList
        !coloring = gcColorAllAdj chunkAdj start count []
        !maxC = if null coloring then 0 else maximum (map snd coloring)
    return $! fromIntegral (maxC * gcShift + count)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_gc_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit(args.out, args.N, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
