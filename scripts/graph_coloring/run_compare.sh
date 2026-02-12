#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Graph Coloring Benchmark Comparison Script
# ============================================================
#
# This script orchestrates a full comparison between:
#   1. Ribault/TALM with GHC superinstructions (dataflow parallelism)
#   2. GHC with Control.Parallel.Strategies (spark-based parallelism)
#   3. GHC with par/pseq primitives (low-level spark parallelism)
#
# The benchmark demonstrates that Ribault's dataflow model achieves
# significantly better parallel scaling than GHC's spark-based model
# for fine-grained graph coloring workloads.
#
# Key observations from experiments:
#   - Ribault: 156x speedup from P=1 to P=20 (N=1000)
#   - GHC: ~1.0x speedup (sparks GC'd or fizzled before execution)
#
# Usage:
#   bash run_compare.sh \
#     --N "1000,5000" \
#     --procs "1,2,4,8,16,20" \
#     --reps 3 \
#     --edge-prob 0.01 \
#     --interp /path/to/interp \
#     --asm-root /path/to/TALM/asm \
#     --codegen /path/to/Ribault \
#     --outroot ./results \
#     --tag my_experiment
#
# Output:
#   - metrics_<tag>_super.csv   (Ribault results)
#   - metrics_<tag>_ghc.csv     (GHC Strategies results)
#   - metrics_<tag>_parpseq.csv (GHC par/pseq results)
#   - Text summary comparing all variants
#
# ============================================================

N_CSV=""; REPS=1
PROCS_CSV=""; EDGE_PROB="0.001"; SEED="42"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; TAG="gc_compare"
PY2="${PY2:-python3}"
PY3="${PY3:-python3}"
PLACE_MODE="${PLACE_MODE:-rr}"
SKIP_SUPER="${SKIP_SUPER:-0}"
SKIP_GHC="${SKIP_GHC:-0}"
SKIP_PARPSEQ="${SKIP_PARPSEQ:-0}"

usage(){
  echo "Usage: $0 --N \"10000,50000,...\" --reps R --procs \"1,2,...\" --outroot DIR"
  echo "          [--edge-prob P] [--seed S] [--tag TAG]"
  echo ""
  echo "For TALM comparison (optional):"
  echo "          --interp PATH --asm-root PATH --codegen PATH"
  echo ""
  echo "env: SKIP_SUPER=1   (skip TALM/super benchmark)"
  echo "     SKIP_GHC=1     (reuse existing GHC Strategies metrics)"
  echo "     SKIP_PARPSEQ=1 (reuse existing GHC par/pseq metrics)"
  echo "     PLACE_MODE, PY2, PY3"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)         N_CSV="$2"; shift 2;;
    --reps)      REPS="$2"; shift 2;;
    --procs)     PROCS_CSV="$2"; shift 2;;
    --edge-prob) EDGE_PROB="$2"; shift 2;;
    --seed)      SEED="$2"; shift 2;;
    --interp)    INTERP="$2"; shift 2;;
    --asm-root)  ASM_ROOT="$2"; shift 2;;
    --codegen)   CODEGEN_ROOT="$2"; shift 2;;
    --outroot)   OUTROOT="$2"; shift 2;;
    --tag)       TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$N_CSV" && -n "$PROCS_CSV" && -n "$OUTROOT" ]] || usage

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SUPER_CSV="$OUTROOT/metrics_${TAG}_super.csv"
GHC_CSV="$OUTROOT/metrics_${TAG}_ghc.csv"
PARPSEQ_CSV="$OUTROOT/metrics_${TAG}_parpseq.csv"
mkdir -p "$OUTROOT"

# Check if TALM infrastructure is available
TALM_AVAILABLE=0
if [[ -n "$INTERP" && -n "$ASM_ROOT" && -n "$CODEGEN_ROOT" ]]; then
  if [[ -x "$INTERP" && -f "$ASM_ROOT/assembler.py" && -x "$CODEGEN_ROOT/codegen" ]]; then
    TALM_AVAILABLE=1
  else
    echo "[WARN] TALM infrastructure specified but not fully available, skipping TALM benchmark"
  fi
fi

# ── Step 1: TALM super sweep ─────────────────────────────
if [[ "$SKIP_SUPER" -eq 1 && -f "$SUPER_CSV" ]]; then
  echo "=== Skipping TALM sweep (SKIP_SUPER=1, reusing $SUPER_CSV) ==="
elif [[ "$TALM_AVAILABLE" -eq 0 ]]; then
  echo "=== Skipping TALM sweep (infrastructure not available) ==="
else
  echo "=== TALM Super Sweep ==="
  PY2="$PY2" PY3="$PY3" PLACE_MODE="$PLACE_MODE" \
  bash "$SCRIPT_DIR/run_super.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" \
    --edge-prob "$EDGE_PROB" --seed "$SEED" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTROOT" --tag "${TAG}_super"
fi

# ── Step 2: GHC Strategies ───────────────────────────────
if [[ "$SKIP_GHC" -eq 1 && -f "$GHC_CSV" ]]; then
  echo "=== Skipping GHC Strategies (SKIP_GHC=1, reusing $GHC_CSV) ==="
else
  echo "=== GHC Strategies Benchmark ==="
  bash "$SCRIPT_DIR/run_hs.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" \
    --edge-prob "$EDGE_PROB" --seed "$SEED" \
    --outroot "$OUTROOT" --tag "${TAG}_ghc" --variant "ghc"
fi

# ── Step 3: GHC par/pseq ─────────────────────────────────
if [[ "$SKIP_PARPSEQ" -eq 1 && -f "$PARPSEQ_CSV" ]]; then
  echo "=== Skipping GHC par/pseq (SKIP_PARPSEQ=1, reusing $PARPSEQ_CSV) ==="
else
  echo "=== GHC par/pseq Benchmark ==="
  bash "$SCRIPT_DIR/run_hs.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" \
    --edge-prob "$EDGE_PROB" --seed "$SEED" \
    --outroot "$OUTROOT" --tag "${TAG}_parpseq" \
    --gen "$SCRIPT_DIR/gen_hs_parpseq.py" --variant "parpseq"
fi

# ── Step 4: Plots ────────────────────────────────────────
echo "=== Generating plots ==="
METRICS_FILES=()
[[ -f "$SUPER_CSV" ]]   && METRICS_FILES+=("$SUPER_CSV")
[[ -f "$GHC_CSV" ]]     && METRICS_FILES+=("$GHC_CSV")
[[ -f "$PARPSEQ_CSV" ]] && METRICS_FILES+=("$PARPSEQ_CSV")

if [[ ${#METRICS_FILES[@]} -eq 0 ]]; then
  echo "[ERROR] No metrics CSV found"; exit 1
fi

"$PY3" "$SCRIPT_DIR/compare_best.py" \
  --metrics "${METRICS_FILES[@]}" \
  --outdir "$OUTROOT" \
  --tag "$TAG"

echo "[DONE] results in: $OUTROOT"
