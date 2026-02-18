#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# run_all.sh — Unified benchmark runner
#
# Runs all 4 benchmarks sequentially, one run at a time:
#   1. N-Queens
#   2. MergeSort
#   3. Text Search
#   4. Graph Coloring
#
# Each rep is sequential. No parallel runs.
# ============================================================

REPO="$(cd "$(dirname "$0")/.." && pwd)"
OUTROOT="${1:-$REPO/RESULTS/run_all}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

# ── Shared infrastructure ──
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
PY3="${PY3:-python3}"

# ── Shared parameters ──
PS_CSV="1,2,4,8,16,32"            # comma-separated (mergesort, graph coloring)
PS_SPACE="1 2 4 8 16 32"          # space-separated (nqueens, textsearch)
REPS=3

# ── Per-benchmark parameters ──
# N-Queens
NQ_NS="8 9 10 11 12 13 14 15 16"

# MergeSort
MS_START_N=500000
MS_STEP=500000
MS_N_MAX=15000000

# Text Search
TS_N_FILES=50
TS_FILE_SIZE=10000000
TS_KEYWORD=FINDME
TS_DENSITY=0.002
TS_N_FUNCS=14
TS_TALM_RTS_A=64m

# Graph Coloring
GC_NS="1000,5000"
GC_PROBS="0.1 0.2 0.3 0.4 0.5"
GC_SEED=42

echo "============================================================"
echo "  UNIFIED BENCHMARK RUNNER"
echo "  Output: $OUTROOT"
echo "  P: $PS_CSV   reps: $REPS"
echo "============================================================"

T0=$(date +%s)

# ============================================================
# 1. N-QUEENS
# ============================================================
echo ""
echo "############################################################"
echo "#  1/4  N-QUEENS"
echo "############################################################"
NQ_OUT="$OUTROOT/nqueens"
NS="$NQ_NS" PS="$PS_SPACE" REPS="$REPS" \
  bash "$REPO/scripts/nqueens/run_validated.sh" "$NQ_OUT"

echo ""
echo "[OK] N-Queens complete -> $NQ_OUT/metrics.csv"

# ============================================================
# 2. MERGESORT
# ============================================================
echo ""
echo "############################################################"
echo "#  2/4  MERGESORT"
echo "############################################################"
MS_OUT="$OUTROOT/mergesort"
MS_LEAF=array DF_LIST_BUILTIN=1 SUPERS_FORCE_PAR=1 MS_NPARTS=64 \
PY3="$PY3" \
bash "$REPO/scripts/merge_sort_TALM_vs_Haskell/run_compare.sh" \
  --start-N "$MS_START_N" --step "$MS_STEP" --n-max "$MS_N_MAX" \
  --reps "$REPS" --procs "$PS_CSV" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$REPO" \
  --outroot "$MS_OUT" --tag "ms"

echo ""
echo "[OK] MergeSort complete -> $MS_OUT/"

# ============================================================
# 3. TEXT SEARCH
# ============================================================
echo ""
echo "############################################################"
echo "#  3/4  TEXT SEARCH"
echo "############################################################"
TS_OUT="$OUTROOT/textsearch"
N_FILES="$TS_N_FILES" FILE_SIZE="$TS_FILE_SIZE" \
KEYWORD="$TS_KEYWORD" DENSITY="$TS_DENSITY" N_FUNCS="$TS_N_FUNCS" \
TALM_RTS_A="$TS_TALM_RTS_A" \
PS="$PS_SPACE" REPS="$REPS" \
  bash "$REPO/scripts/textsearch/run_validated.sh" "$TS_OUT"

echo ""
echo "[OK] Text Search complete -> $TS_OUT/metrics.csv"

# ============================================================
# 4. GRAPH COLORING (loop over edge probs)
# ============================================================
echo ""
echo "############################################################"
echo "#  4/4  GRAPH COLORING"
echo "############################################################"
for PROB in $GC_PROBS; do
  PROB_TAG="prob_$(echo "$PROB" | tr '.' '_')"
  GC_OUT="$OUTROOT/graph_coloring/$PROB_TAG"
  echo ""
  echo "--- Graph Coloring: edge_prob=$PROB ---"
  PY3="$PY3" \
  bash "$REPO/scripts/graph_coloring/run_compare.sh" \
    --N "$GC_NS" --reps "$REPS" --procs "$PS_CSV" \
    --edge-prob "$PROB" --seed "$GC_SEED" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$REPO" \
    --outroot "$GC_OUT" --tag "gc_${PROB_TAG}"
done

echo ""
echo "[OK] Graph Coloring complete -> $OUTROOT/graph_coloring/"

# ============================================================
# Summary
# ============================================================
T1=$(date +%s)
ELAPSED=$(( T1 - T0 ))
HOURS=$(( ELAPSED / 3600 ))
MINS=$(( (ELAPSED % 3600) / 60 ))
SECS=$(( ELAPSED % 60 ))

echo ""
echo "============================================================"
echo "  ALL BENCHMARKS COMPLETE"
echo "  Total time: ${HOURS}h ${MINS}m ${SECS}s"
echo ""
echo "  Results:"
echo "    N-Queens:       $NQ_OUT/metrics.csv"
echo "    MergeSort:      $MS_OUT/"
echo "    Text Search:    $TS_OUT/metrics.csv"
echo "    Graph Coloring: $OUTROOT/graph_coloring/"
echo "============================================================"
