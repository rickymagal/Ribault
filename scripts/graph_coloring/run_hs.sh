#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# GHC Parallel Graph Coloring Benchmark Runner
# ============================================================
#
# This script runs the graph coloring benchmark using pure GHC
# with parallel strategies or par/pseq primitives.
#
# Variants:
#   --variant ghc     : Uses Control.Parallel.Strategies (parMap, rdeepseq)
#   --variant parpseq : Uses Control.Parallel (par, pseq)
#
# GHC's Spark-Based Parallelism:
#   - Sparks are hints to the runtime, not guaranteed parallel execution
#   - Work-stealing scheduler attempts to distribute sparks to capabilities
#   - Fine-grained work often fails to parallelize (sparks GC'd or fizzled)
#
# To diagnose parallelism issues, examine RTS statistics:
#   ./graph_color +RTS -N8 -s -RTS
#
# Look for the SPARKS line:
#   SPARKS: 16 (2 converted, 12 GC'd, 2 fizzled)
#   - "converted": successfully ran in parallel
#   - "GC'd": garbage collected before workers could execute
#   - "fizzled": already evaluated when worker checked
#
# Usage:
#   bash run_hs.sh \
#     --N "1000,5000" \
#     --procs "1,2,4,8" \
#     --reps 3 \
#     --variant ghc \
#     --outroot ./results
#
# ============================================================

N_CSV=""; REPS=1
PROCS_CSV=""; OUTROOT=""; TAG="gc_ghc"
EDGE_PROB="0.001"; SEED="42"
GEN_OVERRIDE=""
VARIANT="ghc"
GHC="${GHC:-ghc}"
GHC_PKGS="${GHC_PKGS:--package time -package containers -package deepseq -package parallel}"
PY3="${PY3:-python3}"

usage(){
  echo "Usage: $0 --N \"10000,50000,...\" --reps R --procs \"1,2,...\" --outroot DIR"
  echo "          [--edge-prob P] [--seed S] [--tag TAG] [--gen script.py] [--variant name]"
  echo "env: GHC, GHC_PKGS, PY3"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)         N_CSV="$2"; shift 2;;
    --reps)      REPS="$2"; shift 2;;
    --procs)     PROCS_CSV="$2"; shift 2;;
    --outroot)   OUTROOT="$2"; shift 2;;
    --edge-prob) EDGE_PROB="$2"; shift 2;;
    --seed)      SEED="$2"; shift 2;;
    --tag)       TAG="$2"; shift 2;;
    --gen)       GEN_OVERRIDE="$2"; shift 2;;
    --variant)   VARIANT="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$N_CSV" && -n "$PROCS_CSV" && -n "$OUTROOT" ]] || usage

IFS=',' read -r -a NS    <<< "$N_CSV"
IFS=',' read -r -a PROCS <<< "$PROCS_CSV"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [[ -n "$GEN_OVERRIDE" ]]; then
  GEN_PY="$GEN_OVERRIDE"
else
  GEN_PY="$SCRIPT_DIR/gen_hs_strategies.py"
fi

[[ -f "$GEN_PY" ]] || { echo "[ERROR] gen script not found: $GEN_PY"; exit 1; }
command -v "$GHC" >/dev/null || { echo "[ERROR] GHC not found: $GHC"; exit 1; }
echo "[env ] GHC=${GHC} ; GEN=${GEN_PY} ; variant=${VARIANT}"

mkdir -p "$OUTROOT"
METRICS="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,edge_prob,seed,rep,seconds,colors,valid,rc" > "$METRICS"

TOTAL_RUNS=$(( ${#NS[@]} * ${#PROCS[@]} * REPS ))
RUN_NUM=0

for N in "${NS[@]}"; do
  for P in "${PROCS[@]}"; do
    CASE_DIR="$OUTROOT/${VARIANT}/N_${N}/P_${P}"
    BIN_DIR="$CASE_DIR/bin"; mkdir -p "$BIN_DIR"
    HS="$BIN_DIR/graph_color.hs"
    BIN="$BIN_DIR/graph_color"

    "$PY3" "$GEN_PY" --out "$HS" --N "$N" --P "$P" --edge-prob "$EDGE_PROB" --seed "$SEED"
    echo "[build] N=${N} P=${P} -> compiling"
    "$GHC" -O2 -threaded -rtsopts -dynamic $GHC_PKGS -outputdir "$BIN_DIR" -o "$BIN" "$HS" >/dev/null 2>&1

    for ((rep=1; rep<=REPS; rep++)); do
      RUN_NUM=$((RUN_NUM + 1))
      local_rc=0
      outlog="$CASE_DIR/run_P${P}_rep${rep}.out"
      mkdir -p "$(dirname "$outlog")"
      set +e
      "$BIN" +RTS -N"$P" -RTS >"$outlog" 2>/dev/null
      local_rc=$?
      set -e

      secs="NaN"
      colors="0"
      valid="False"
      if [[ $local_rc -eq 0 ]]; then
        secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$outlog")"
        colors="$(awk -F= '/^COLORS=/{print $2}' "$outlog")"
        valid="$(awk -F= '/^VALID=/{print $2}' "$outlog")"
        if [[ "$valid" != "True" ]]; then
          >&2 echo "[ERR ] INVALID COLORING at N=${N} P=${P} rep=${rep}"
          local_rc=99
        fi
      fi
      [[ -z "$secs" ]] && secs="NaN"
      [[ -z "$colors" ]] && colors="0"

      echo "[${RUN_NUM}/${TOTAL_RUNS}] N=${N} P=${P} rep=${rep} -> ${secs}s colors=${colors} valid=${valid} rc=${local_rc}"
      echo "${VARIANT},${N},${P},${EDGE_PROB},${SEED},${rep},${secs},${colors},${valid},${local_rc}" >> "$METRICS"
    done
  done
done

echo "[DONE] ${TOTAL_RUNS} runs; metrics: $METRICS"
