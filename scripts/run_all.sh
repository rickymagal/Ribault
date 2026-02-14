#!/usr/bin/env bash
set -euo pipefail

# ═══════════════════════════════════════════════════════════════
#  run_all.sh – Run all Ribault benchmarks with chosen parameters
# ═══════════════════════════════════════════════════════════════
#
# Usage:
#   bash scripts/run_all.sh [OPTIONS]
#
# Examples:
#   # Run everything with defaults (paper parameters):
#   bash scripts/run_all.sh
#
#   # Quick smoke test (small sizes, few reps):
#   bash scripts/run_all.sh --reps 1 --procs 1,2 \
#     --ms-N 50000,100000 --dyck-N 50000 --dyck-imb 0,50,100 \
#     --fib-N 35 --fib-cutoff 25,30 \
#     --matmul-N 100,200 --gc-N 50,100
#
#   # Run only mergesort and matmul:
#   bash scripts/run_all.sh --only mergesort,matmul
#
#   # Skip TALM, only re-run GHC variants:
#   SKIP_TALM=1 SKIP_SUPER=1 bash scripts/run_all.sh
#
# Environment variables:
#   SKIP_TALM=1 / SKIP_SUPER=1  – skip TALM/super runs
#   SKIP_GHC=1                   – skip GHC Strategies runs
#   SKIP_PARPSEQ=1               – skip GHC par/pseq runs
#   PY2, PY3                     – Python interpreters (default: python3)

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# ── Helper ────────────────────────────────────────────────────

usage() {
  cat <<'USAGE'
run_all.sh – Run all Ribault benchmarks with chosen parameters

Global options:
  --procs <csv>           Processor counts (default: 1,2,4,8)
  --reps <n>              Default repetitions for all benchmarks
  --outdir <dir>          Results root directory (default: RESULTS)
  --only <csv>            Only run these benchmarks (comma-separated)
                          Choices: mergesort,dyck,fibonacci,matmul,graph_coloring,nqueens
  --skip <csv>            Skip these benchmarks
  --dry-run               Print what would run without executing
  -h, --help              Show this help

MergeSort options:
  --ms-start-N <n>        Starting array size (default: 50000)
  --ms-step <n>           Step increment (default: 50000)
  --ms-max-N <n>          Maximum N (default: 1000000)
  --ms-N <csv>            Explicit N values (overrides start/step/max)
  --ms-reps <n>           Repetitions (default: --reps or 10)

Dyck options:
  --dyck-N <csv>          Sequence lengths (default: 50000..1000000 step 50000)
  --dyck-imb <csv>        Imbalance percentages (default: 0..100 step 5)
  --dyck-delta <csv>      Delta values (default: 0)
  --dyck-reps <n>         Repetitions (default: --reps or 3)

Fibonacci options:
  --fib-N <csv>           N values (default: 35)
  --fib-cutoff <csv>      Cutoff values (default: 15,20,25,30)
  --fib-reps <n>          Repetitions (default: --reps or 3)

MatMul options:
  --matmul-N <csv>        Matrix sizes (default: 50..1000 step 50)
  --matmul-reps <n>       Repetitions (default: --reps or 10)

Graph Coloring options:
  --gc-N <csv>            Graph sizes (default: 50..1000 step 50)
  --gc-reps <n>           Repetitions (default: --reps or 3)
  --gc-edge-prob <f>      Edge probability (default: 0.01)
  --gc-seed <n>           Random seed (default: 42)

N-Queens options:
  --nq-N <csv>            Board sizes (default: 8,10,12,13,14)
  --nq-cutoff <n>         Cutoff depth (default: 3)
  --nq-reps <n>           Repetitions (default: --reps or 3)
USAGE
  exit 0
}

# Generate a CSV range: seq_csv <start> <step> <end>
seq_csv() {
  local s=$1 step=$2 e=$3
  local out=""
  for ((i=s; i<=e; i+=step)); do
    [[ -n "$out" ]] && out+=","
    out+="$i"
  done
  echo "$out"
}

# ── Defaults ──────────────────────────────────────────────────

PROCS="1,2,4,8"
GLOBAL_REPS=""
OUTDIR="$ROOT/RESULTS"
ONLY=""
SKIP_BENCHMARKS=""
DRY_RUN=0

# MergeSort
MS_START_N=50000
MS_STEP=50000
MS_MAX_N=1000000
MS_N=""
MS_REPS=""

# Dyck
DYCK_N=""
DYCK_IMB=""
DYCK_DELTA="0"
DYCK_REPS=""

# Fibonacci
FIB_N="35"
FIB_CUTOFF="15,20,25,30"
FIB_REPS=""

# MatMul
MATMUL_N=""
MATMUL_REPS=""

# Graph Coloring
GC_N=""
GC_REPS=""
GC_EDGE_PROB="0.01"
GC_SEED="42"

# N-Queens
NQ_N="8,10,12,13,14"
NQ_CUTOFF="3"
NQ_REPS=""

# ── Parse arguments ───────────────────────────────────────────

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)       usage ;;
    --procs)         PROCS="$2"; shift 2 ;;
    --reps)          GLOBAL_REPS="$2"; shift 2 ;;
    --outdir)        OUTDIR="$2"; shift 2 ;;
    --only)          ONLY="$2"; shift 2 ;;
    --skip)          SKIP_BENCHMARKS="$2"; shift 2 ;;
    --dry-run)       DRY_RUN=1; shift ;;

    --ms-start-N)    MS_START_N="$2"; shift 2 ;;
    --ms-step)       MS_STEP="$2"; shift 2 ;;
    --ms-max-N)      MS_MAX_N="$2"; shift 2 ;;
    --ms-N)          MS_N="$2"; shift 2 ;;
    --ms-reps)       MS_REPS="$2"; shift 2 ;;

    --dyck-N)        DYCK_N="$2"; shift 2 ;;
    --dyck-imb)      DYCK_IMB="$2"; shift 2 ;;
    --dyck-delta)    DYCK_DELTA="$2"; shift 2 ;;
    --dyck-reps)     DYCK_REPS="$2"; shift 2 ;;

    --fib-N)         FIB_N="$2"; shift 2 ;;
    --fib-cutoff)    FIB_CUTOFF="$2"; shift 2 ;;
    --fib-reps)      FIB_REPS="$2"; shift 2 ;;

    --matmul-N)      MATMUL_N="$2"; shift 2 ;;
    --matmul-reps)   MATMUL_REPS="$2"; shift 2 ;;

    --gc-N)          GC_N="$2"; shift 2 ;;
    --gc-reps)       GC_REPS="$2"; shift 2 ;;
    --gc-edge-prob)  GC_EDGE_PROB="$2"; shift 2 ;;
    --gc-seed)       GC_SEED="$2"; shift 2 ;;

    --nq-N)          NQ_N="$2"; shift 2 ;;
    --nq-cutoff)     NQ_CUTOFF="$2"; shift 2 ;;
    --nq-reps)       NQ_REPS="$2"; shift 2 ;;

    *) echo "Unknown option: $1"; echo "Use --help for usage."; exit 1 ;;
  esac
done

# ── Resolve defaults ──────────────────────────────────────────

# Fill in generated ranges where explicit CSV wasn't given
[[ -z "$MS_N" ]]      && MS_N="$(seq_csv "$MS_START_N" "$MS_STEP" "$MS_MAX_N")"
[[ -z "$DYCK_N" ]]    && DYCK_N="$(seq_csv 50000 50000 1000000)"
[[ -z "$DYCK_IMB" ]]  && DYCK_IMB="$(seq_csv 0 5 100)"
[[ -z "$MATMUL_N" ]]  && MATMUL_N="$(seq_csv 50 50 1000)"
[[ -z "$GC_N" ]]      && GC_N="$(seq_csv 50 50 1000)"

# Per-benchmark reps: explicit > global > default
[[ -z "$MS_REPS" ]]     && MS_REPS="${GLOBAL_REPS:-10}"
[[ -z "$DYCK_REPS" ]]   && DYCK_REPS="${GLOBAL_REPS:-3}"
[[ -z "$FIB_REPS" ]]    && FIB_REPS="${GLOBAL_REPS:-3}"
[[ -z "$MATMUL_REPS" ]] && MATMUL_REPS="${GLOBAL_REPS:-10}"
[[ -z "$GC_REPS" ]]     && GC_REPS="${GLOBAL_REPS:-3}"
[[ -z "$NQ_REPS" ]]     && NQ_REPS="${GLOBAL_REPS:-3}"

# Infrastructure paths
INTERP="$ROOT/TALM/interp/interp"
ASM_ROOT="$ROOT/TALM/asm"
CODEGEN_ROOT="$ROOT"
PY2="${PY2:-python3}"
PY3="${PY3:-python3}"
export PY2 PY3

# ── Benchmark selection ───────────────────────────────────────

ALL_BENCHMARKS="mergesort dyck fibonacci matmul graph_coloring nqueens"

should_run() {
  local bench="$1"
  if [[ -n "$ONLY" ]]; then
    echo ",$ONLY," | grep -q ",$bench," && return 0 || return 1
  fi
  if [[ -n "$SKIP_BENCHMARKS" ]]; then
    echo ",$SKIP_BENCHMARKS," | grep -q ",$bench," && return 1 || return 0
  fi
  return 0
}

# ── Sanity checks ─────────────────────────────────────────────

[[ -x "$INTERP" ]]                || { echo "ERROR: interp not found: $INTERP"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" ]] || { echo "ERROR: assembler not found: $ASM_ROOT/assembler.py"; exit 1; }
[[ -x "$CODEGEN_ROOT/codegen" ]]  || { echo "ERROR: codegen not found: $CODEGEN_ROOT/codegen"; exit 1; }

# ── Print configuration ──────────────────────────────────────

echo "================================================================"
echo "  Ribault – Run All Benchmarks"
echo "================================================================"
echo ""
echo "Infrastructure:"
echo "  interp:   $INTERP"
echo "  asm:      $ASM_ROOT"
echo "  codegen:  $CODEGEN_ROOT/codegen"
echo "  outdir:   $OUTDIR"
echo "  procs:    $PROCS"
echo ""

ENABLED=""
for b in $ALL_BENCHMARKS; do
  if should_run "$b"; then
    ENABLED+="$b "
  fi
done
echo "Benchmarks: $ENABLED"
echo ""

if should_run mergesort; then
  echo "  MergeSort:      N=$MS_N  reps=$MS_REPS"
fi
if should_run dyck; then
  n_dyck=$(echo "$DYCK_N" | tr ',' '\n' | wc -l)
  n_imb=$(echo "$DYCK_IMB" | tr ',' '\n' | wc -l)
  echo "  Dyck:           ${n_dyck} N values, ${n_imb} imb values, delta=$DYCK_DELTA  reps=$DYCK_REPS"
fi
if should_run fibonacci; then
  echo "  Fibonacci:      N=$FIB_N  cutoff=$FIB_CUTOFF  reps=$FIB_REPS"
fi
if should_run matmul; then
  echo "  MatMul:         N=$MATMUL_N  reps=$MATMUL_REPS"
fi
if should_run graph_coloring; then
  echo "  Graph Coloring: N=$GC_N  edge_prob=$GC_EDGE_PROB  seed=$GC_SEED  reps=$GC_REPS"
fi
if should_run nqueens; then
  echo "  N-Queens:       N=$NQ_N  cutoff=$NQ_CUTOFF  reps=$NQ_REPS"
fi
echo ""

if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "(dry-run mode – exiting without running)"
  exit 0
fi

# ── Run benchmarks ────────────────────────────────────────────

FAILED=""
SECONDS_TOTAL=0

run_bench() {
  local name="$1"; shift
  local start_t=$SECONDS
  echo ""
  echo "======== ${name^^} ========"
  if "$@"; then
    local elapsed=$((SECONDS - start_t))
    echo "[DONE] $name completed in ${elapsed}s"
  else
    local elapsed=$((SECONDS - start_t))
    echo "[FAIL] $name failed after ${elapsed}s"
    FAILED+="$name "
  fi
}

# 1. MergeSort
if should_run mergesort; then
  # MergeSort uses start/step/max, not explicit --N
  run_bench mergesort bash "$ROOT/scripts/merge_sort_TALM_vs_Haskell/run_compare.sh" \
    --start-N "$MS_START_N" --step "$MS_STEP" --n-max "$MS_MAX_N" \
    --reps "$MS_REPS" --procs "$PROCS" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/mergesort" --tag ms
fi

# 2. Dyck
if should_run dyck; then
  run_bench dyck bash "$ROOT/scripts/dyck/run_compare.sh" \
    --N "$DYCK_N" --reps "$DYCK_REPS" --procs "$PROCS" \
    --imb "$DYCK_IMB" --delta "$DYCK_DELTA" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/dyck_N_IMB_sweep" --tag dyck
fi

# 3. Fibonacci
if should_run fibonacci; then
  run_bench fibonacci bash "$ROOT/scripts/fibonacci/run_compare.sh" \
    --N "$FIB_N" --cutoff "$FIB_CUTOFF" \
    --reps "$FIB_REPS" --procs "$PROCS" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/fibonacci" --tag fib
fi

# 4. MatMul
if should_run matmul; then
  run_bench matmul bash "$ROOT/scripts/matmul/run_compare.sh" \
    --N "$MATMUL_N" --reps "$MATMUL_REPS" --procs "$PROCS" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/matmul" --tag matmul
fi

# 5. Graph Coloring
if should_run graph_coloring; then
  run_bench graph_coloring bash "$ROOT/scripts/graph_coloring/run_compare.sh" \
    --N "$GC_N" --reps "$GC_REPS" --procs "$PROCS" \
    --edge-prob "$GC_EDGE_PROB" --seed "$GC_SEED" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/graph_coloring" --tag gc
fi

# 6. N-Queens
if should_run nqueens; then
  run_bench nqueens bash "$ROOT/scripts/nqueens/run_compare.sh" \
    --N "$NQ_N" --cutoff "$NQ_CUTOFF" \
    --reps "$NQ_REPS" --procs "$PROCS" \
    --interp "$INTERP" --asm-root "$ASM_ROOT" --codegen "$CODEGEN_ROOT" \
    --outroot "$OUTDIR/nqueens" --tag nq
fi

# ── Summary ───────────────────────────────────────────────────

echo ""
echo "================================================================"
if [[ -z "$FAILED" ]]; then
  echo "  ALL BENCHMARKS COMPLETE  (${SECONDS}s total)"
else
  echo "  DONE WITH FAILURES  (${SECONDS}s total)"
  echo "  Failed: $FAILED"
fi
echo "  Results in: $OUTDIR"
echo "================================================================"

# Print metrics file summary
echo ""
echo "Metrics files:"
for f in "$OUTDIR"/mergesort/metrics_ms_*.csv \
         "$OUTDIR"/dyck_N_IMB_sweep/metrics_dyck_*.csv \
         "$OUTDIR"/fibonacci/metrics_fib_*.csv \
         "$OUTDIR"/matmul/metrics_matmul_*.csv \
         "$OUTDIR"/graph_coloring/metrics_gc_*.csv \
         "$OUTDIR"/nqueens/metrics_nq_*.csv; do
  if [[ -f "$f" ]]; then
    lines=$(($(wc -l < "$f") - 1))
    printf "  %-60s %6d runs\n" "${f#$ROOT/}" "$lines"
  fi
done

# Error check
echo ""
total_errors=0
for f in "$OUTDIR"/mergesort/metrics_ms_*.csv \
         "$OUTDIR"/dyck_N_IMB_sweep/metrics_dyck_*.csv \
         "$OUTDIR"/fibonacci/metrics_fib_*.csv \
         "$OUTDIR"/matmul/metrics_matmul_*.csv \
         "$OUTDIR"/graph_coloring/metrics_gc_*.csv \
         "$OUTDIR"/nqueens/metrics_nq_*.csv; do
  if [[ -f "$f" ]]; then
    errs=$(grep -cE ',9[89]$' "$f" 2>/dev/null || true)
    total_errors=$((total_errors + errs))
  fi
done
echo "Total errors (rc=98/99): $total_errors"

[[ -z "$FAILED" ]] && exit 0 || exit 1
