#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# nqueens/run_hs.sh — GHC N-Queens benchmark runner
# Cutoff computed per (N, P): ceil(log_N(OVERSUB * P)), min 1
# ============================================================

N_CSV=""; REPS=1; OVERSUB=4
PROCS_CSV=""; OUTROOT=""; TAG="nq_ghc"
VARIANT="ghc"
GEN_OVERRIDE=""
GHC="${GHC:-ghc}"
GHC_PKGS="${GHC_PKGS:--package time -package parallel}"
PY3="${PY3:-python3}"

usage(){
  echo "uso: $0 --N \"8,10,...\" --reps R --procs \"1,2,...\" --outroot DIR"
  echo "        [--tag TAG] [--gen script.py] [--variant name] [--oversub K]"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)        N_CSV="$2"; shift 2;;
    --reps)     REPS="$2"; shift 2;;
    --procs)    PROCS_CSV="$2"; shift 2;;
    --outroot)  OUTROOT="$2"; shift 2;;
    --tag)      TAG="$2"; shift 2;;
    --gen)      GEN_OVERRIDE="$2"; shift 2;;
    --variant)  VARIANT="$2"; shift 2;;
    --oversub)  OVERSUB="$2"; shift 2;;
    --cutoff)   shift 2;;  # ignored (auto-computed), kept for compat
    *) usage;;
  esac
done

[[ -n "$N_CSV" && -n "$PROCS_CSV" && -n "$OUTROOT" ]] || usage

IFS=',' read -r -a NS    <<< "$N_CSV"
IFS=',' read -r -a PROCS  <<< "$PROCS_CSV"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [[ -n "$GEN_OVERRIDE" ]]; then
  GEN_PY="$GEN_OVERRIDE"
else
  GEN_PY="$SCRIPT_DIR/gen_hs_strategies.py"
fi

[[ -f "$GEN_PY" ]] || { echo "[ERRO] gen script not found: $GEN_PY"; exit 1; }
command -v "$GHC" >/dev/null || { echo "[ERRO] GHC not found: $GHC"; exit 1; }

# Compute cutoff = ceil(log_N(OVERSUB * P)), minimum 1
compute_cutoff() {
  local n="$1" p="$2"
  "$PY3" -c "
import math
N, P, K = $n, $p, $OVERSUB
target = K * max(P, 1)
if N <= 1:
    print(1)
else:
    print(max(1, math.ceil(math.log(target) / math.log(N))))
"
}

echo "[env ] GHC=${GHC} ; GEN=${GEN_PY} ; variant=${VARIANT} ; oversub=${OVERSUB}"

mkdir -p "$OUTROOT"
METRICS="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,cutoff,P,rep,seconds,rc" > "$METRICS"

declare -A NQ_EXPECTED
NQ_EXPECTED[1]=1; NQ_EXPECTED[2]=0; NQ_EXPECTED[3]=0; NQ_EXPECTED[4]=2
NQ_EXPECTED[5]=10; NQ_EXPECTED[6]=4; NQ_EXPECTED[7]=40; NQ_EXPECTED[8]=92
NQ_EXPECTED[9]=352; NQ_EXPECTED[10]=724; NQ_EXPECTED[11]=2680; NQ_EXPECTED[12]=14200
NQ_EXPECTED[13]=73712; NQ_EXPECTED[14]=365596; NQ_EXPECTED[15]=2279184

TOTAL_RUNS=$(( ${#NS[@]} * ${#PROCS[@]} * REPS ))
RUN_NUM=0

# Track which (N, cutoff) combos have been compiled
declare -A BUILT_CUTOFFS

for N in "${NS[@]}"; do
  expected="${NQ_EXPECTED[$N]:-}"
  [[ -n "$expected" ]] && echo "[nq  ] expected nqueens($N) = $expected"

  for P in "${PROCS[@]}"; do
    CUTOFF=$(compute_cutoff "$N" "$P")
    BUILD_KEY="${N}_${CUTOFF}"
    CASE_DIR="$OUTROOT/${VARIANT}/N_${N}_C${CUTOFF}"

    # Build binary only once per (N, cutoff)
    if [[ -z "${BUILT_CUTOFFS[$BUILD_KEY]:-}" ]]; then
      BIN_DIR="$CASE_DIR/bin"; mkdir -p "$BIN_DIR"
      HS="$BIN_DIR/nqueens.hs"
      BIN="$BIN_DIR/nqueens"

      "$PY3" "$GEN_PY" --out "$HS" --N "$N" --cutoff "$CUTOFF"
      echo "[build] N=${N} cutoff=${CUTOFF} -> compiling"
      "$GHC" -O2 -threaded -rtsopts $GHC_PKGS -outputdir "$BIN_DIR" -o "$BIN" "$HS" >/dev/null 2>&1
      BUILT_CUTOFFS[$BUILD_KEY]="$CASE_DIR"
    fi

    CASE_DIR="${BUILT_CUTOFFS[$BUILD_KEY]}"
    BIN="$CASE_DIR/bin/nqueens"

    for ((rep=1; rep<=REPS; rep++)); do
      RUN_NUM=$((RUN_NUM + 1))
      local_rc=0
      outlog="$CASE_DIR/run_P${P}_rep${rep}.out"
      set +e
      "$BIN" +RTS -N"$P" -RTS >"$outlog" 2>/dev/null
      local_rc=$?
      set -e

      secs="NaN"
      if [[ $local_rc -eq 0 ]]; then
        secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$outlog")"
        result="$(awk -F= '/^RESULT=/{print $2}' "$outlog")"
        if [[ -n "$expected" && "$result" != "$expected" ]]; then
          >&2 echo "[ERR ] WRONG nqueens($N): got '$result', expected '$expected'"
          local_rc=99
        fi
      fi
      [[ -z "$secs" ]] && secs="NaN"

      echo "[${RUN_NUM}/${TOTAL_RUNS}] N=${N} C=${CUTOFF} P=${P} rep=${rep} -> ${secs}s rc=${local_rc}"
      echo "${VARIANT},${N},${CUTOFF},${P},${rep},${secs},${local_rc}" >> "$METRICS"
    done
  done
done

echo "[DONE] ${TOTAL_RUNS} runs; metrics: $METRICS"
