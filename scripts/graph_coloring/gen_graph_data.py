#!/usr/bin/env python3
"""Generate binary adjacency matrix for graph coloring benchmark.

Writes adj.bin as a flat N*N byte array (row-major).
adj[i*N + j] = 1 if edge (i,j) exists, 0 otherwise.
Graph is symmetric (undirected).

Uses the same LCG hash as gen_graph_input.py / gen_hs_strategies.py
for identical graph topology.
"""

import argparse, os


def gen(out_dir, N, edge_prob, seed):
    os.makedirs(out_dir, exist_ok=True)
    prob_scaled = int(edge_prob * 1000000)

    a = 6364136223846793005
    c = 1442695040888963407
    m = 2**63
    d = 2**33

    adj = bytearray(N * N)
    n_edges = 0

    for u in range(N):
        for v in range(u + 1, N):
            r0_uv = seed + u * 31337 + v * 7919
            val_uv = ((a * r0_uv + c) % m // d) % 1000000

            r0_vu = seed + v * 31337 + u * 7919
            val_vu = ((a * r0_vu + c) % m // d) % 1000000

            if val_uv < prob_scaled or val_vu < prob_scaled:
                adj[u * N + v] = 1
                adj[v * N + u] = 1
                n_edges += 1

    path = os.path.join(out_dir, "adj.bin")
    with open(path, "wb") as f:
        f.write(adj)

    print(f"[gen_gc_data] wrote {path} ({N}x{N}, {os.path.getsize(path)} bytes, {n_edges} edges)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True, help="Number of vertices")
    ap.add_argument("--edge-prob", type=float, default=0.001)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    gen(args.out_dir, args.N, args.edge_prob, args.seed)


if __name__ == "__main__":
    main()
