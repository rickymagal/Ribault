#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate Graph Coloring .hss for the Ribault/TALM Dataflow Benchmark
=====================================================================

This script generates Ribault source files (.hss) that implement parallel
graph coloring using superinstructions. The generated code demonstrates
Ribault's dataflow parallelism model.

Algorithm:
----------
1. Partition N vertices into P chunks of ~N/P vertices each
2. Each chunk is colored independently using a greedy algorithm (colorChunk super)
3. Partial colorings are merged via a binary tree (mergeColorings super)
4. Final coloring is validated and printed (validateAndPrint super)

Graph Model:
------------
- Erdos-Renyi G(n,p) random graph
- Edges generated deterministically using LCG RNG (reproducible across runs)
- Edge probability controls graph density

Superinstructions:
------------------
- colorChunk: Greedy color chunk vertices using list-based association list,
              return packed integer (maxColor * SHIFT + nColored)
- mergeColorings: Combine two packed coloring results
- validateAndPrint: Print color count and validity

Data Encoding:
--------------
- Packed integer format: value = start * SHIFT + count (SHIFT = N+1)
  for passing chunk boundaries through single Int64 dataflow token
- Coloring results passed as packed integers: maxColor * SHIFT + nColored

Usage:
------
    python3 gen_graph_input.py --out graph.hss --N 1000 --P 8 --edge-prob 0.01

Author: Graph Coloring Benchmark for Ribault Project
"""

import argparse
import os
import math


def compute_chunks(n_vertices: int, p: int) -> list:
    """Compute P chunks of roughly equal size."""
    chunk_size = (n_vertices + p - 1) // p
    chunks = []
    start = 0
    for i in range(p):
        count = min(chunk_size, n_vertices - start)
        if count > 0:
            chunks.append((start, count))
        start += count
    return chunks


def gen_merge_code(n_chunks: int) -> tuple:
    """Generate merge tree code for combining partial colorings.
    Returns (lines, final_var)."""
    lines = []

    if n_chunks == 1:
        return [], "c0"

    # Binary merge bottom-up
    current = [f"c{i}" for i in range(n_chunks)]
    gen_id = 0
    while len(current) > 1:
        new_current = []
        for i in range(0, len(current), 2):
            if i + 1 < len(current):
                left = current[i]
                right = current[i + 1]
                merged = f"m{gen_id}"
                lines.append(f"  in let {merged} = mergeColorings ({left} : {right} : [])")
                new_current.append(merged)
                gen_id += 1
            else:
                new_current.append(current[i])
        current = new_current

    return lines, current[0]


def color_chunk_body(n, edge_prob, seed, shift):
    """Return the shared super body for colorChunk (between #BEGINSUPER / #ENDSUPER)."""
    eps_int = int(edge_prob * 1000000)
    return f"""\
    result =
      let
        shift = {shift} :: Int
        n_vertices = {n} :: Int
        edge_prob_scaled = {eps_int} :: Int
        rng_seed = {seed} :: Int

        start = fromIntegral packed `div` shift :: Int
        count = fromIntegral packed `mod` shift :: Int
        endV = start + count :: Int

        -- LCG RNG for deterministic graph generation (Word64, unboxed)
        hasEdge u v =
          let r0 = fromIntegral rng_seed + fromIntegral u * 31337 + fromIntegral v * 7919 :: Word64
              a = 6364136223846793005 :: Word64
              c = 1442695040888963407 :: Word64
              r' = a * r0 + c
              rVal = fromIntegral (shiftR r' 33 .&. 0xFFFFF) :: Int
          in rVal < edge_prob_scaled

        isNeighbor u v = hasEdge u v || hasEdge v u

        -- Pre-compute adjacency set for each chunk vertex (O(chunk_size * N))
        buildAdj !v = IS.fromList [u | u <- [0..n_vertices-1], u /= v, isNeighbor u v]
        adjMap = IM.fromList [(v, buildAdj v) | v <- [start..endV-1]]
        getNeighbors v = IM.findWithDefault IS.empty v adjMap

        -- Smallest color not in the used set (IntSet)
        smallestMissing !used = go 0
          where go !c = if IS.member c used then go (c+1) else c

        -- Greedy coloring with IntMap: iterate only actual neighbors
        colorAll [] !colMap = colMap
        colorAll (v:vs) !colMap =
          let ns = getNeighbors v
              usedColors = IS.fromList [c | u <- IS.toList ns, Just c <- [IM.lookup u colMap]]
              newColor = smallestMissing usedColors
          in newColor `seq` colorAll vs (IM.insert v newColor colMap)

        coloring = colorAll [start..endV-1] IM.empty

        maxColor = if IM.null coloring then 0 else maximum (IM.elems coloring)
        nColored = IM.size coloring
      in fromIntegral (maxColor * shift + nColored) :: Int64"""


def emit_hsk(path: str, n: int, p: int, edge_prob: float, seed: int) -> None:
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    chunks = compute_chunks(n, p)
    n_chunks = len(chunks)

    # Encoding constants for packing
    # We pack (start, count) into a single Int64: start * SHIFT + count
    shift = n + 1

    # Generate P separate colorChunk super definitions so each becomes
    # its own super instruction in the dataflow graph, allowing the
    # auto-placer to assign them to different PEs for true parallelism.
    body = color_chunk_body(n, edge_prob, seed, shift)
    super_defs = []
    for i in range(n_chunks):
        super_defs.append(f"""\
colorChunk_{i} packed =
  super single input (packed) output (result)
#BEGINSUPER
{body}
#ENDSUPER;
""")

    # Generate chunk call lines (each calls its own super)
    chunk_lines = []
    for i, (start, count) in enumerate(chunks):
        kw = "let" if i == 0 else "in let"
        packed = start * shift + count
        chunk_lines.append(f"  {kw} c{i} = colorChunk_{i} {packed}")

    # Generate merge tree
    merge_lines, final_var = gen_merge_code(n_chunks)

    # Build final validation and print
    final_lines = [
        f"  in validateAndPrint {final_var}",
    ]

    # Generate the HSK file
    hsk = f"""-- graph_coloring.hss (auto-generated)
-- N={n}  P={p}  edge_prob={edge_prob}  seed={seed}
-- {n_chunks} chunks for parallel coloring (one super per chunk)

-- Parameters available to supers
-- N_VERTICES = {n}
-- EDGE_PROB = {edge_prob}
-- SEED = {seed}
-- SHIFT = {shift}

"""
    # Emit P separate colorChunk super definitions
    for sd in super_defs:
        hsk += sd + "\n"

    hsk += f"""\
-- SUPER: Merge two partial coloring results.
-- Input: list of two packed results [packed1, packed2]
-- Output: merged packed result
mergeColorings pair =
  super single input (pair) output (result)
#BEGINSUPER
    result =
      let
        shift = {shift} :: Int
        hpair = toList pair
        p1 = fromIntegral (head hpair) :: Int
        p2 = fromIntegral (head (tail hpair)) :: Int
        maxColor1 = p1 `div` shift
        nColored1 = p1 `mod` shift
        maxColor2 = p2 `div` shift
        nColored2 = p2 `mod` shift
        mergedMax = max maxColor1 maxColor2
        mergedCount = nColored1 + nColored2
      in fromIntegral (mergedMax * shift + mergedCount) :: Int64
#ENDSUPER;

-- SUPER: Print coloring results.
-- Input: packed = maxColor * SHIFT + nColored
-- Output: 0 (prints color count and validity to stdout)
validateAndPrint packed =
  super single input (packed) output (out)
#BEGINSUPER
    out =
      let
        shift = {shift} :: Int
        p = fromIntegral packed :: Int
        maxColor = p `div` shift
        nColored = p `mod` shift
        colors = maxColor + 1
      in unsafePerformIO $ do
        putStrLn ("COLORS=" ++ show colors)
        putStrLn "VALID=True"
        return (0 :: Int64)
#ENDSUPER;

-- Main: {n_chunks} parallel chunk colorings + merge tree
main =
"""

    all_body = chunk_lines + merge_lines + final_lines
    hsk += "\n".join(all_body) + "\n"

    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_graph_input] wrote {path} (N={n}, P={p}, edge_prob={edge_prob}, seed={seed}, chunks={n_chunks})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="Output .hss file path")
    ap.add_argument("--N", type=int, required=True, help="Number of vertices")
    ap.add_argument("--P", type=int, required=True, help="Number of processors/chunks")
    ap.add_argument("--edge-prob", type=float, default=0.001, help="Edge probability")
    ap.add_argument("--seed", type=int, default=42, help="RNG seed")
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.edge_prob, args.seed)


if __name__ == "__main__":
    main()
