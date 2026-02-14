#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

INTERP="$ROOT/TALM/interp/interp"
ASM_ROOT="$ROOT/TALM/asm"
CODEGEN_ROOT="$ROOT"
PY2=python3
PY3=python3
export PY2 PY3

echo "================================================================"
echo "  RE-RUNNING ALL BENCHMARKS WITH 64-BIT TAG INTERPRETER"
echo "================================================================"

# ── 1. MERGE SORT (parpseq incomplete, re-run all 3) ──
echo ""
echo "======== MERGE SORT ========"
bash "$ROOT/scripts/merge_sort_TALM_vs_Haskell/run_compare.sh" \
  --start-N 50000 --step 50000 --n-max 1000000 \
  --reps 10 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
  --outroot "$ROOT/RESULTS/mergesort" --tag ms

# ── 2. DYCK ──
echo ""
echo "======== DYCK ========"
bash "$ROOT/scripts/dyck/run_compare.sh" \
  --N "50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000,650000,700000,750000,800000,850000,900000,950000,1000000" \
  --reps 3 --procs "1,2,4,8" \
  --imb "0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100" \
  --delta "0" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
  --outroot "$ROOT/RESULTS/dyck_N_IMB_sweep" --tag dyck

# ── 3. FIBONACCI ──
echo ""
echo "======== FIBONACCI ========"
bash "$ROOT/scripts/fibonacci/run_compare.sh" \
  --N "35" --cutoff "15,20,25,30" \
  --reps 3 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
  --outroot "$ROOT/RESULTS/fibonacci" --tag fib

# ── 4. MATMUL ──
echo ""
echo "======== MATMUL ========"
bash "$ROOT/scripts/matmul/run_compare.sh" \
  --N "50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000" \
  --reps 10 --procs "1,2,4,8" \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
  --outroot "$ROOT/RESULTS/matmul" --tag matmul

# ── 5. GRAPH COLORING ──
echo ""
echo "======== GRAPH COLORING ========"
bash "$ROOT/scripts/graph_coloring/run_compare.sh" \
  --N "50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000" \
  --reps 3 --procs "1,2,4,8" \
  --edge-prob 0.01 --seed 42 \
  --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
  --outroot "$ROOT/RESULTS/graph_coloring" --tag gc

echo ""
echo "================================================================"
echo "  ALL BENCHMARKS COMPLETE"
echo "================================================================"
