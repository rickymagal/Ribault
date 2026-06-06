#!/usr/bin/env python3
"""Synthetic Speculative Decoding draft-tree benchmark — input generator.

Writes <out-dir>/config.txt with the benchmark parameters.  Unlike a real
speculative decoding workload (which requires a draft LLM), this benchmark
is purely synthetic: each "scoring" call does a fixed amount of arithmetic
on a small embedding vector, with LCG-derived inputs.  The point is to
exercise the same RUNTIME STRUCTURE as EAGLE-3-style draft-tree decoding:

  - Batch of B requests processed concurrently
  - Each request runs K decode steps
  - Each step builds a draft tree of branching L, depth D (so up to L^D nodes)
  - Per node: a recursive call to `score` that does HIDDEN_DIM_VEC_MUL flops
  - Reduction: sum scores over the tree (proxy for path verification)

What this preserves from real spec decoding:
  - Recursive tree expansion with cross-dependencies (child needs parent score)
  - Heap-allocated boxed nodes in Haskell -> GC pressure on Strategies
  - Work-per-task in the 100us-10ms window (HIDDEN_DIM tuned)
  - Per-request tagging (batch parallelism via task IDs in TALM)

What this does NOT preserve:
  - The actual LLM forward pass (GPU-bound; we measure CPU control-plane)
  - The reranking-by-value heuristic with tree-connectivity constraint
"""
import argparse, os


def emit(out_dir, batch, depth, branching, steps, hidden_dim, seed):
    os.makedirs(out_dir, exist_ok=True)
    with open(os.path.join(out_dir, "config.txt"), "w") as f:
        f.write(f"BATCH {batch}\n")
        f.write(f"DEPTH {depth}\n")
        f.write(f"BRANCHING {branching}\n")
        f.write(f"STEPS {steps}\n")
        f.write(f"HIDDEN_DIM {hidden_dim}\n")
        f.write(f"SEED {seed}\n")
    print(f"[gen_input] wrote {out_dir}/  batch={batch} depth={depth} "
          f"branching={branching} steps={steps} hidden_dim={hidden_dim}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--batch", type=int, default=8)
    ap.add_argument("--depth", type=int, default=4)
    ap.add_argument("--branching", type=int, default=4)
    ap.add_argument("--steps", type=int, default=10)
    ap.add_argument("--hidden-dim", type=int, default=64)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    emit(args.out_dir, args.batch, args.depth, args.branching, args.steps,
         args.hidden_dim, args.seed)


if __name__ == "__main__":
    main()
