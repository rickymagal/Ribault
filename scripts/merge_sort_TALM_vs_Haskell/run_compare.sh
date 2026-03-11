#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# merge_sort/run_compare.sh
#
# Benchmark comparison:
#   1. TALM (EX21: linked-list merge sort with par/pseq inside super)
#   2. GHC Strategies (array-based merge sort, rpar/rseq)
#   3. GHC par/pseq (array-based merge sort)
#
# TALM uses the EX21 approach: DF graph handles recursive splitting
# down to N/P, then a single ms_super does parallel merge sort
# internally via par/pseq on Haskell lists.
#
# GHC standalone variants use array-based merge sort with Ptr Int
# and unboxed primops for maximum performance.
#
# Usage:
#   bash run_compare.sh --start-N 1000000 --step 1000000 --n-max 5000000 \
#     --reps 3 --procs "1,2,4,8,16" \
#     --interp PATH --asm-root PATH --codegen PATH --outroot DIR
# ============================================================

START_N=1000000; STEP=1000000; N_MAX=5000000; REPS=3
PROCS_CSV="1,2,4,8,16"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; TAG="ms"

PY2="${PY2:-python3}"
PY3="${PY3:-python3}"
GHC="${GHC:-ghc}"
GHC_PKGS="${GHC_PKGS:--package time -package parallel}"

SKIP_TALM="${SKIP_TALM:-0}"
SKIP_STRAT="${SKIP_STRAT:-0}"
SKIP_PARPSEQ="${SKIP_PARPSEQ:-0}"

usage(){
  echo "Usage: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\""
  echo "          --interp PATH --asm-root PATH --codegen PATH --outroot DIR"
  echo "          [--tag TAG]"
  echo ""
  echo "Env: SKIP_TALM=1 SKIP_STRAT=1 SKIP_PARPSEQ=1"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N)  START_N="$2"; shift 2;;
    --step)     STEP="$2"; shift 2;;
    --n-max)    N_MAX="$2"; shift 2;;
    --reps)     REPS="$2"; shift 2;;
    --procs)    PROCS_CSV="$2"; shift 2;;
    --interp)   INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen)  CODEGEN_ROOT="$2"; shift 2;;
    --outroot)  OUTROOT="$2"; shift 2;;
    --tag)      TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$OUTROOT" ]] || usage

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$OUTROOT"

TALM_CSV="$OUTROOT/metrics_${TAG}_talm.csv"
STRAT_CSV="$OUTROOT/metrics_${TAG}_strat.csv"
PARPSEQ_CSV="$OUTROOT/metrics_${TAG}_parpseq.csv"

# ── Step 1: TALM (EX21 linked-list par/pseq) ────────────────
if [[ "$SKIP_TALM" -eq 1 && -f "$TALM_CSV" ]]; then
  echo "=== Skipping TALM (SKIP_TALM=1) ==="
else
  echo "=== TALM EX21 Merge Sort ==="
  PY2="$PY2" PY3="$PY3" \
  MS_LEAF=ex21 SUPERS_FORCE_PAR=1 \
  bash "$SCRIPT_DIR/run.sh" \
    --start-N "$START_N" --step "$STEP" --n-max "$N_MAX" \
    --reps "$REPS" --procs "$PROCS_CSV" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTROOT" --vec range --plots no --tag "${TAG}_talm"
fi

# ── Step 2: GHC Strategies (array-based) ─────────────────────
if [[ "$SKIP_STRAT" -eq 1 && -f "$STRAT_CSV" ]]; then
  echo "=== Skipping GHC Strategies (SKIP_STRAT=1) ==="
else
  echo "=== GHC Strategies Merge Sort (array-based) ==="
  GHC_PKGS="$GHC_PKGS" \
  bash "$SCRIPT_DIR/run_hs.sh" \
    --start-N "$START_N" --step "$STEP" --n-max "$N_MAX" \
    --reps "$REPS" --procs "$PROCS_CSV" \
    --outroot "$OUTROOT" --vec range --tag "${TAG}_strat" \
    --gen "$SCRIPT_DIR/gen_hs_strategies.py" --variant "strat"
fi

# ── Step 3: GHC par/pseq (array-based) ───────────────────────
if [[ "$SKIP_PARPSEQ" -eq 1 && -f "$PARPSEQ_CSV" ]]; then
  echo "=== Skipping GHC par/pseq (SKIP_PARPSEQ=1) ==="
else
  echo "=== GHC par/pseq Merge Sort (array-based) ==="
  GHC_PKGS="$GHC_PKGS" \
  bash "$SCRIPT_DIR/run_hs.sh" \
    --start-N "$START_N" --step "$STEP" --n-max "$N_MAX" \
    --reps "$REPS" --procs "$PROCS_CSV" \
    --outroot "$OUTROOT" --vec range --tag "${TAG}_parpseq" \
    --gen "$SCRIPT_DIR/gen_hs_parpseq.py" --variant "parpseq"
fi

# ── Summary ──────────────────────────────────────────────────
echo ""
echo "======== RESULTS ========"

# Combine CSVs and print summary
"$PY3" - "$TALM_CSV" "$STRAT_CSV" "$PARPSEQ_CSV" <<'PYEOF'
import sys, csv, os
from collections import defaultdict

groups = defaultdict(list)
for path in sys.argv[1:]:
    if not os.path.isfile(path):
        continue
    for r in csv.DictReader(open(path)):
        # Normalize variant names
        variant = r['variant']
        if variant == 'super':
            variant = 'talm'
        key = (variant, int(r['N']), int(r['P']))
        try:
            groups[key].append(float(r['seconds']))
        except ValueError:
            pass

if not groups:
    print("No results found")
    sys.exit(0)

print(f"{'variant':>10} {'N':>10} {'P':>4} {'median':>10} {'times':>30}")
print("-" * 68)
for key in sorted(groups.keys()):
    variant, n, p = key
    times = sorted(groups[key])
    median = times[len(times)//2] if times else float('nan')
    ts = ", ".join(f"{t:.3f}" for t in times)
    print(f"{variant:>10} {n:>10} {p:>4} {median:>10.3f} {ts:>30}")
PYEOF

echo ""
echo "[DONE] results in: $OUTROOT"
