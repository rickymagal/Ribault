#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# nqueens/run_compare.sh — TALM + GHC-strategies + GHC-par/pseq
# Cutoff auto-computed per (N, P): ceil(log_N(OVERSUB * P))
# ============================================================

N_CSV="8,10,12,14"; REPS=3; OVERSUB=4
PROCS_CSV="1,2,4,8"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; TAG="nq"
PY2="${PY2:-python3}"
PY3="${PY3:-python3}"
SKIP_TALM="${SKIP_TALM:-0}"
SKIP_GHC="${SKIP_GHC:-0}"
SKIP_PARPSEQ="${SKIP_PARPSEQ:-0}"

usage(){
  echo "uso: $0 --N \"8,10,...\" --reps R --procs \"1,2,...\""
  echo "        --interp PATH --asm-root PATH --codegen PATH --outroot DIR"
  echo "        [--tag TAG] [--oversub K]"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)        N_CSV="$2"; shift 2;;
    --reps)     REPS="$2"; shift 2;;
    --procs)    PROCS_CSV="$2"; shift 2;;
    --interp)   INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen)  CODEGEN_ROOT="$2"; shift 2;;
    --outroot)  OUTROOT="$2"; shift 2;;
    --tag)      TAG="$2"; shift 2;;
    --oversub)  OVERSUB="$2"; shift 2;;
    --cutoff)   shift 2;;  # ignored, kept for compat
    *) usage;;
  esac
done

[[ -n "$OUTROOT" ]] || usage

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TALM_CSV="$OUTROOT/metrics_${TAG}_talm.csv"
GHC_CSV="$OUTROOT/metrics_${TAG}_ghc.csv"
PARPSEQ_CSV="$OUTROOT/metrics_${TAG}_parpseq.csv"
mkdir -p "$OUTROOT"

# ── Step 1: TALM ─────────────────────────────────────────
if [[ "$SKIP_TALM" -eq 1 && -f "$TALM_CSV" ]]; then
  echo "=== Skipping TALM (SKIP_TALM=1) ==="
else
  echo "=== TALM N-Queens ==="
  PY2="$PY2" \
  bash "$SCRIPT_DIR/run_talm.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" --oversub "$OVERSUB" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTROOT" --tag "${TAG}_talm"
fi

# ── Step 2: GHC Strategies ───────────────────────────────
if [[ "$SKIP_GHC" -eq 1 && -f "$GHC_CSV" ]]; then
  echo "=== Skipping GHC Strategies (SKIP_GHC=1) ==="
else
  echo "=== GHC Strategies N-Queens ==="
  bash "$SCRIPT_DIR/run_hs.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" --oversub "$OVERSUB" \
    --outroot "$OUTROOT" --tag "${TAG}_ghc" --variant "ghc" \
    --gen "$SCRIPT_DIR/gen_hs_strategies.py"
fi

# ── Step 3: GHC par/pseq ─────────────────────────────────
if [[ "$SKIP_PARPSEQ" -eq 1 && -f "$PARPSEQ_CSV" ]]; then
  echo "=== Skipping GHC par/pseq (SKIP_PARPSEQ=1) ==="
else
  echo "=== GHC par/pseq N-Queens ==="
  bash "$SCRIPT_DIR/run_hs.sh" \
    --N "$N_CSV" --reps "$REPS" --procs "$PROCS_CSV" --oversub "$OVERSUB" \
    --outroot "$OUTROOT" --tag "${TAG}_parpseq" --variant "parpseq" \
    --gen "$SCRIPT_DIR/gen_hs_parpseq.py"
fi

# ── Step 4: Plots ────────────────────────────────────────
echo "=== Generating all plots ==="
PLOT_ARGS=("--outdir" "$OUTROOT" "--tag" "$TAG")
METRICS_FILES=()
[[ -f "$TALM_CSV" ]]    && METRICS_FILES+=("$TALM_CSV")
[[ -f "$GHC_CSV" ]]     && METRICS_FILES+=("$GHC_CSV")
[[ -f "$PARPSEQ_CSV" ]] && METRICS_FILES+=("$PARPSEQ_CSV")

if [[ ${#METRICS_FILES[@]} -gt 0 ]]; then
  "$PY3" "$SCRIPT_DIR/plot.py" --metrics "${METRICS_FILES[@]}" "${PLOT_ARGS[@]}"
fi

echo "[DONE] results in: $OUTROOT"
