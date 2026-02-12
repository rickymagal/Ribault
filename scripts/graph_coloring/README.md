# Graph Coloring Benchmark: Ribault vs GHC Parallel

This benchmark compares parallel graph coloring performance between:
- **Ribault/TALM**: Dataflow execution with GHC-compiled superinstructions
- **GHC Strategies**: `Control.Parallel.Strategies` (`parMap`, `rdeepseq`)
- **GHC par/pseq**: Low-level `Control.Parallel` primitives

## Algorithm

**Greedy Vertex Coloring** with parallel chunk processing:
1. Partition vertices into P chunks
2. Color each chunk independently using greedy algorithm
3. Merge partial colorings (binary tree reduction)
4. Validate the final coloring

### Graph Generation
- **Model**: Erdos-Renyi G(n,p) random graphs
- **Deterministic**: LCG-based RNG with fixed seed for reproducibility
- **Symmetric**: Edges are bidirectional

## Key Findings

### Parallel Scaling Results (N=1000 vertices)

| Processors | Ribault (TALM) | GHC Strategies |
|------------|----------------|----------------|
| P=1        | 56.2s          | 4.7ms          |
| P=2        | 17.5s          | 5.3ms          |
| P=4        | 3.8s           | 5.6ms          |
| P=8        | 2.2s           | 6.1ms          |
| P=12       | 0.99s          | 5.9ms          |
| P=16       | 0.60s          | 5.8ms          |
| P=20       | 0.36s          | 5.8ms          |

**Ribault speedup**: 156x from P=1 to P=20
**GHC speedup**: ~1.0x (no scaling)

### Why GHC Doesn't Scale

Running with RTS statistics (`+RTS -s`) reveals:

```
SPARKS: 16 (2 converted, 0 overflowed, 0 dud, 12 GC'd, 2 fizzled)
```

- **Sparks GC'd**: Work completed before workers could steal it
- **Sparks fizzled**: Workers found work already evaluated
- **Sequential bottlenecks**: `foldl' IM.union` and `resolveConflicts`

### Why Ribault Scales

1. **Explicit dataflow parallelism**: All parallel operations encoded at compile time
2. **Binary merge tree**: Parallel reduction vs sequential `foldl'`
3. **No conflict resolution**: Chunks are truly independent
4. **No spark overhead**: Work dispatched immediately to workers

## Architecture Comparison

### Ribault Dataflow (Parallel Merge Tree)
```
    c0 ──┐
    c1 ──┼─→ m0 ──┐
                  ├─→ m4 ──┐
    c2 ──┐        │        │
    c3 ──┼─→ m1 ──┘        ├─→ m6 → validate
                           │
    c4 ──┐                 │
    c5 ──┼─→ m2 ──┐        │
                  ├─→ m5 ──┘
    c6 ──┐        │
    c7 ──┼─→ m3 ──┘
```

### GHC Strategies (Sequential Bottlenecks)
```
    [c0..c7] via parMap      ← sparks created (may fizzle)
           │
           ↓
    foldl' IM.union          ← SEQUENTIAL
           │
           ↓
    resolveConflicts         ← SEQUENTIAL
           │
           ↓
        result
```

## Files

| File | Description |
|------|-------------|
| `gen_graph_input.py` | Generates Ribault `.hsk` files with superinstructions |
| `gen_hs_strategies.py` | Generates GHC code using `Control.Parallel.Strategies` |
| `gen_hs_parpseq.py` | Generates GHC code using `par`/`pseq` primitives |
| `run_super.sh` | Runs Ribault/TALM benchmark (codegen → asm → interp) |
| `run_hs.sh` | Runs GHC benchmark (compile → execute) |
| `run_compare.sh` | Orchestrates all three variants |
| `compare_best.py` | Aggregates results and generates summary |

## Usage

### Full Comparison
```bash
bash run_compare.sh \
  --N "1000,5000" \
  --reps 3 \
  --procs "1,2,4,8,16" \
  --edge-prob 0.01 \
  --seed 42 \
  --interp /path/to/TALM/interp/interp \
  --asm-root /path/to/TALM/asm \
  --codegen /path/to/Ribault \
  --outroot ./results \
  --tag my_experiment
```

### GHC-only
```bash
bash run_hs.sh \
  --N "1000" \
  --procs "1,2,4,8" \
  --reps 3 \
  --variant ghc \
  --outroot ./results
```

### Ribault/TALM-only
```bash
bash run_super.sh \
  --N "1000" \
  --procs "1,2,4,8" \
  --reps 3 \
  --interp /path/to/interp \
  --outroot ./results
```

## Output

Results are written to CSV files:
- `metrics_<tag>_super.csv` - Ribault/TALM results
- `metrics_<tag>_ghc.csv` - GHC Strategies results
- `metrics_<tag>_parpseq.csv` - GHC par/pseq results

Columns: `variant,N,P,edge_prob,seed,rep,runtime_sec,colors,valid,rc`

## Requirements

### For GHC benchmarks
- GHC with `-threaded` support
- Packages: `parallel`, `deepseq`, `containers`, `time`

### For Ribault/TALM benchmarks
- Ribault codegen (`./codegen`)
- TALM assembler (`TALM/asm/assembler.py`)
- TALM interpreter (`TALM/interp/interp`)
- `build_supers.sh` for superinstruction compilation

## Technical Notes

### Superinstruction Implementation

The Ribault `.hsk` file contains three superinstructions:

1. **`colorChunk`**: Colors a range of vertices using greedy algorithm
   - Input: packed integer `start * SHIFT + count`
   - Output: packed result `maxColor * (N+1) + nColored`

2. **`mergeColorings`**: Combines two partial colorings
   - Input: list of two packed colorings
   - Output: merged packed coloring

3. **`validateAndPrint`**: Validates and outputs results
   - Recomputes full coloring for verification
   - Prints color count and validity flag

### GHC RTS Flags

The GHC benchmarks are compiled with:
```
ghc -O2 -threaded -rtsopts -dynamic
```

And run with:
```
./graph_color +RTS -N<P> -RTS
```

To debug parallelism issues:
```
./graph_color +RTS -N8 -s -RTS
```

Look for the `SPARKS` line - high "GC'd" or "fizzled" counts indicate poor parallel utilization.

## References

- Ribault/TALM dataflow architecture
- GHC Parallel Haskell documentation
- Greedy graph coloring algorithm
