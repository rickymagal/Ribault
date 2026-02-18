# Running Benchmarks

## Quick Start

From the repository root:

```bash
bash scripts/run_all.sh
```

This runs all 4 benchmarks sequentially with default parameters and writes results to `RESULTS/run_all/`.

To specify a custom output directory:

```bash
bash scripts/run_all.sh /path/to/output
```

## Prerequisites

- GHC (with packages: `parallel`, `deepseq`, `time`, `bytestring`)
- Python 3
- The TALM interpreter compiled at `TALM/interp/interp`
- The `codegen` binary compiled at the repo root

## Default Parameters

| Parameter | Value |
|-----------|-------|
| Processors (P) | 1, 2, 4, 8, 12, 16, 20 |
| Repetitions | 3 |

### N-Queens

| Parameter | Value |
|-----------|-------|
| N (board size) | 8, 9, 10, 11, 12, 13, 14, 15, 16 |
| Cutoff | Computed per (N, P) via `ceil(log(4*P) / log(N))` |

### MergeSort

| Parameter | Value |
|-----------|-------|
| N (array size) | 500,000 to 15,000,000 (step 500,000) |
| MS_NPARTS | 64 |
| MS_LEAF | array |

### Text Search

| Parameter | Value |
|-----------|-------|
| Number of files | 50 |
| File size | 10 MB each |
| Keyword | FINDME |
| Density | 0.002 |
| TALM RTS allocation | 64m |

### Graph Coloring

| Parameter | Value |
|-----------|-------|
| N (vertices) | 1,000 and 5,000 |
| Edge probability | 0.1, 0.2, 0.3, 0.4, 0.5 |
| Seed | 42 |

## Customizing Parameters

Edit the variables at the top of `scripts/run_all.sh`:

```bash
# Processors and repetitions
PS_CSV="1,2,4,8,12,16,20"
PS_SPACE="1 2 4 8 12 16 20"
REPS=3

# N-Queens
NQ_NS="8 9 10 11 12 13 14 15 16"

# MergeSort
MS_START_N=500000
MS_STEP=500000
MS_N_MAX=15000000

# Text Search
TS_N_FILES=50
TS_FILE_SIZE=10000000    # 10 MB per file

# Graph Coloring
GC_NS="1000,5000"
GC_PROBS="0.1 0.2 0.3 0.4 0.5"
```

### Examples

**Smaller run for testing** (fewer P values, fewer N values):

```bash
# Edit run_all.sh:
PS_CSV="1,2,4"
PS_SPACE="1 2 4"
REPS=1
NQ_NS="8 10 12"
MS_START_N=1000000
MS_STEP=1000000
MS_N_MAX=3000000
GC_PROBS="0.1 0.3"
```

**Heavy run** (more processors, more repetitions):

```bash
PS_CSV="1,2,4,8,12,16,20,24"
PS_SPACE="1 2 4 8 12 16 20 24"
REPS=5
MS_N_MAX=20000000
```

## Output Structure

```
RESULTS/run_all/
  nqueens/
    metrics.csv                  # variant,N,cutoff,P,rep,seconds
  mergesort/
    metrics_ms_super.csv         # variant,N,P,rep,seconds,rc
    metrics_ms_ghc.csv
    metrics_ms_parpseq.csv
  textsearch/
    metrics.csv                  # variant,n_files,file_size,P,rep,seconds
  graph_coloring/
    prob_0_1/
      metrics_gc_prob_0_1_super.csv
      metrics_gc_prob_0_1_ghc.csv
      metrics_gc_prob_0_1_parpseq.csv
    prob_0_2/
      ...
    prob_0_3/
      ...
    prob_0_4/
      ...
    prob_0_5/
      ...
```

Each CSV contains one row per (variant, parameter combination, P, repetition). The `variant` column identifies the execution strategy:

- `super` — TALM dataflow execution
- `ghc` — GHC with `Control.Parallel.Strategies` (`parMap rdeepseq`)
- `parpseq` — GHC with explicit `par`/`pseq`

## Running Individual Benchmarks

Each benchmark can also be run independently:

```bash
# N-Queens
NS="8 10 12" PS="1 2 4 8" REPS=3 bash scripts/nqueens/run_validated.sh ./my_output

# Text Search
N_FILES=50 FILE_SIZE=10000000 PS="1 2 4 8" REPS=3 \
  bash scripts/textsearch/run_validated.sh ./my_output

# MergeSort
bash scripts/merge_sort_TALM_vs_Haskell/run_compare.sh \
  --start-N 500000 --step 500000 --n-max 5000000 \
  --reps 3 --procs "1,2,4,8" \
  --interp TALM/interp/interp --asm-root TALM/asm --codegen . \
  --outroot ./my_output --tag "ms"

# Graph Coloring
bash scripts/graph_coloring/run_compare.sh \
  --N "1000,5000" --reps 3 --procs "1,2,4,8" \
  --edge-prob 0.1 --seed 42 \
  --interp TALM/interp/interp --asm-root TALM/asm --codegen . \
  --outroot ./my_output --tag "gc"
```

## Notes

- All runs are sequential: one benchmark at a time, one repetition at a time.
- Each run overwrites previous CSV files in the same output directory.
- Validation is performed on every run (result correctness is verified before recording).
- No plots are generated during the run. Use the plot scripts separately (e.g., `scripts/paper_figures.py`).
