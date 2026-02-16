#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Run ALL benchmarks with the same parameters as RESULTS/.
# Each benchmark calls its own run_compare / run_validated script.
# ============================================================

REPO="$(cd "$(dirname "$0")/.." && pwd)"
OUTROOT="${1:-$REPO/RESULTS}"
mkdir -p "$OUTROOT"

INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO"   # for run_compare scripts that expect --codegen

echo "============================================================"
echo " RUNNING ALL BENCHMARKS"
echo " Output: $OUTROOT"
echo "============================================================"

# ── 1. Merge Sort ────────────────────────────────────────────
echo ""
echo "############################################################"
echo "# 1/7  MERGE SORT"
echo "############################################################"
bash "$REPO/scripts/merge_sort_TALM_vs_Haskell/run_compare.sh" \
  --start-N 50000 --step 50000 --n-max 1000000 \
  --reps 10 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN" \
  --outroot "$OUTROOT/mergesort" --tag ms

# ── 2. Matrix Multiply ──────────────────────────────────────
echo ""
echo "############################################################"
echo "# 2/7  MATRIX MULTIPLY"
echo "############################################################"
bash "$REPO/scripts/matmul/run_compare.sh" \
  --N "50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000" \
  --reps 10 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN" \
  --outroot "$OUTROOT/matmul" --tag matmul

# ── 3. Fibonacci ────────────────────────────────────────────
echo ""
echo "############################################################"
echo "# 3/7  FIBONACCI"
echo "############################################################"
bash "$REPO/scripts/fibonacci/run_compare.sh" \
  --N "35" --cutoff "15,20,25,30" \
  --reps 3 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN" \
  --outroot "$OUTROOT/fibonacci" --tag fib

# ── 4. Dyck N/Imbalance Sweep ───────────────────────────────
echo ""
echo "############################################################"
echo "# 4/7  DYCK SEQUENCE (N × imbalance sweep)"
echo "############################################################"
bash "$REPO/scripts/dyck/run_compare.sh" \
  --N "50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000,650000,700000,750000,800000,850000,900000,950000,1000000" \
  --reps 3 --procs "1,2,4,8" \
  --imb "0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100" \
  --delta "0" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN" \
  --outroot "$OUTROOT/dyck_N_IMB_sweep" --tag dyck

# ── 5. Graph Coloring ───────────────────────────────────────
echo ""
echo "############################################################"
echo "# 5/7  GRAPH COLORING"
echo "############################################################"
bash "$REPO/scripts/graph_coloring/run_compare.sh" \
  --N "50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000" \
  --reps 3 --procs "1,2,4,8" \
  --edge-prob 0.01 --seed 42 \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN" \
  --outroot "$OUTROOT/graph_coloring" --tag gc

# ── 6. N-Queens ─────────────────────────────────────────────
echo ""
echo "############################################################"
echo "# 6/7  N-QUEENS (validated)"
echo "############################################################"
NS="8 9 10 11 12 13 14 15" PS="1 2 4 8 12 16 20 24" REPS=3 \
  bash "$REPO/scripts/nqueens/run_validated.sh" "$OUTROOT/nqueens"

# ── 7. Text Search ──────────────────────────────────────────
echo ""
echo "############################################################"
echo "# 7/7  TEXT SEARCH (validated)"
echo "############################################################"
N_FILES=50 FILE_SIZE=5000000 N_FUNCS=12 \
  PS="1 2 4 8 12 16 24" REPS=3 \
  bash "$REPO/scripts/textsearch/run_validated.sh" "$OUTROOT/textsearch"

echo ""
echo "============================================================"
echo " ALL BENCHMARKS COMPLETE"
echo " Results: $OUTROOT"
echo "============================================================"
