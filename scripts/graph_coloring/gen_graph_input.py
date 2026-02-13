#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate Graph Coloring .hsk for the Ribault/TALM Dataflow Benchmark
=====================================================================

This script generates Ribault source files (.hsk) that implement parallel
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
- colorChunk: Greedy coloring of vertex range [start, start+count)
- mergeColorings: Combine two partial colorings (max colors, sum counts)
- validateAndPrint: Recompute full coloring and validate correctness

Data Encoding:
--------------
- Packed integer format: value = start * SHIFT + count (SHIFT = N+1)
- Enables passing multiple values through single Int64 dataflow token

Usage:
------
    python3 gen_graph_input.py --out graph.hsk --N 1000 --P 8 --edge-prob 0.01

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


def emit_hsk(path: str, n: int, p: int, edge_prob: float, seed: int) -> None:
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    chunks = compute_chunks(n, p)
    n_chunks = len(chunks)

    # Encoding constants for packing
    # We pack (start, count) into a single Int64: start * SHIFT + count
    shift = n + 1

    # Generate chunk call lines
    chunk_lines = []
    for i, (start, count) in enumerate(chunks):
        kw = "let" if i == 0 else "in let"
        packed = start * shift + count
        chunk_lines.append(f"  {kw} c{i} = colorChunk {packed}")

    # Generate merge tree
    merge_lines, final_var = gen_merge_code(n_chunks)

    # Build final validation and print
    final_lines = [
        f"  in validateAndPrint {final_var}",
    ]

    # Generate the HSK file
    hsk = f"""-- graph_coloring.hsk (auto-generated)
-- N={n}  P={p}  edge_prob={edge_prob}  seed={seed}
-- {n_chunks} chunks for parallel coloring

-- Parameters available to supers
-- N_VERTICES = {n}
-- EDGE_PROB = {edge_prob}
-- SEED = {seed}
-- SHIFT = {shift}

-- SUPER: Color a chunk of vertices using greedy algorithm.
-- Input: packed = start * SHIFT + count
-- Output: packed coloring info (colors_used * COLORS_SHIFT + valid_flag)
-- The coloring is stored internally and used for conflict resolution.
colorChunk packed =
  super single input (packed) output (result)
#BEGINSUPER
    result =
      let
        shift = {shift}
        n_vertices = {n}
        edge_prob_scaled = {int(edge_prob * 1000000)}  -- scaled by 1e6
        rng_seed = {seed}

        start = packed `div` shift
        count = packed `mod` shift
        endV = start + count

        -- LCG RNG for deterministic graph generation
        lcgA = 6364136223846793005
        lcgC = 1442695040888963407
        lcgNext r = (lcgA * r + lcgC) `mod` (2^63)
        lcgValue r = (r `div` (2^33)) `mod` 1000000  -- 0 to 999999

        -- Check if edge exists between u and v (u < v)
        hasEdge u v =
          let r0 = rng_seed + u * 31337 + v * 7919
              rVal = lcgValue (lcgNext (fromIntegral r0))
          in rVal < edge_prob_scaled

        -- Get neighbors of vertex v (only neighbors < v for greedy)
        neighborsLess v = [u | u <- [0..v-1], hasEdge u v || hasEdge v u]

        -- Find smallest color not in use by neighbors
        smallestMissing used = go 0
          where go c = if c `elem` used then go (c + 1) else c

        -- Greedy color vertices in range [start, endV)
        colorRange coloring v
          | v >= endV = coloring
          | otherwise =
              let neighborColors = [c | (u, c) <- coloring, u `elem` neighborsLess v]
                  myColor = smallestMissing neighborColors
              in colorRange ((v, myColor) : coloring) (v + 1)

        coloring = colorRange [] start
        maxColor = if null coloring then 0 else maximum (map snd coloring)
        nColored = length coloring
      in
        -- Pack result: maxColor * (n+1) + nColored
        fromIntegral (maxColor * (n_vertices + 1) + nColored)
#ENDSUPER;

-- SUPER: Merge two partial colorings and resolve conflicts.
-- Input: list of two packed colorings [c1, c2]
-- Output: merged packed coloring
mergeColorings pair =
  super single input (pair) output (result)
#BEGINSUPER
    result =
      let
        hpair = toList pair
        c1 = head hpair
        c2 = head (tail hpair)
        n_vertices = {n}

        -- Unpack: packed = maxColor * (n+1) + nColored
        unpack p = (p `div` (n_vertices + 1), p `mod` (n_vertices + 1))

        (max1, n1) = unpack c1
        (max2, n2) = unpack c2

        -- Merged: max of maxColors, sum of counts
        maxColor = if max1 > max2 then max1 else max2
        nColored = n1 + n2
      in
        fromIntegral (maxColor * (n_vertices + 1) + nColored)
#ENDSUPER;

-- SUPER: Validate the coloring and print results.
-- Input: packed coloring
-- Output: 0 (prints results to stdout)
validateAndPrint packed =
  super single input (packed) output (out)
#BEGINSUPER
    out =
      let
        n_vertices = {n}
        edge_prob_scaled = {int(edge_prob * 1000000)}
        rng_seed = {seed}

        maxColor = packed `div` (n_vertices + 1)
        nColored = packed `mod` (n_vertices + 1)

        -- Recompute the full coloring to validate
        lcgA = 6364136223846793005
        lcgC = 1442695040888963407
        lcgNext r = (lcgA * r + lcgC) `mod` (2^63)
        lcgValue r = (r `div` (2^33)) `mod` 1000000

        hasEdge u v =
          let r0 = rng_seed + u * 31337 + v * 7919
              rVal = lcgValue (lcgNext (fromIntegral r0))
          in rVal < edge_prob_scaled

        smallestMissing used = go 0
          where go c = if c `elem` used then go (c + 1) else c

        isNeighbor u v = hasEdge u v || hasEdge v u

        computeColor coloring v
          | v >= n_vertices = coloring
          | otherwise =
              let neighborColors = [c | (u, c) <- coloring, isNeighbor u v]
                  myColor = smallestMissing neighborColors
              in computeColor ((v, myColor) : coloring) (v + 1)

        fullColoring = computeColor [] 0

        -- Validate: no two adjacent vertices have same color
        isValid = and [not (isNeighbor u v) || cu /= cv
                      | (u, cu) <- fullColoring, (v, cv) <- fullColoring, u < v]

        colors = if null fullColoring then 0 else maximum (map snd fullColoring) + 1
        valid = if isValid then 1 else 0
      in
        unsafePerformIO $ do
          print colors
          print valid
          pure (0 :: Int64)
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
    ap.add_argument("--out", required=True, help="Output .hsk file path")
    ap.add_argument("--N", type=int, required=True, help="Number of vertices")
    ap.add_argument("--P", type=int, required=True, help="Number of processors/chunks")
    ap.add_argument("--edge-prob", type=float, default=0.001, help="Edge probability")
    ap.add_argument("--seed", type=int, default=42, help="RNG seed")
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.edge_prob, args.seed)


if __name__ == "__main__":
    main()
