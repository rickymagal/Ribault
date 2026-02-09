#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# dyck/run_hs.sh — GHC-parallel baseline (index-based)
# Fixed N, varies IMB (work imbalance) and P.
# Checks correctness: delta=0 → expects "1", else "0".
# ============================================================

N=0; REPS=1
PROCS_CSV=""; OUTROOT=""; VEC_MODE="range"; TAG="hs_dyck"
IMB_CSV=""; DELTA_CSV="0"
GHC="${GHC:-ghc}"
GHC_PKGS_DEFAULT="-package parallel -package deepseq"
GHC_PKGS="${GHC_PKGS:-$GHC_PKGS_DEFAULT}"
PY3="${PY3:-python3}"

usage(){
  echo "uso: $0 --N SIZE --reps R --procs \"1,2,...\" --imb \"0,10,20,...\" [--delta \"0,2,-2\"] \\"
  echo "          --outroot PATH [--tag TAG]"
  echo "env: GHC, GHC_PKGS, PY3"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)     N="$2"; shift 2;;
    --reps)  REPS="$2"; shift 2;;
    --procs) PROCS_CSV="$2"; shift 2;;
    --imb)   IMB_CSV="$2"; shift 2;;
    --delta) DELTA_CSV="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec)   VEC_MODE="$2"; shift 2;;
    --tag)   TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ "$N" -gt 0 && -n "$REPS$PROCS_CSV$OUTROOT$IMB_CSV" ]] || usage

echo "[env ] GHC=${GHC} ; GHC_PKGS=${GHC_PKGS} ; PY3=${PY3} ; N=${N}"

BASE_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$BASE_DIR/gen_hs_input.py"
PLOT_PY="$BASE_DIR/plot.py"
mkdir -p "$OUTROOT"

METRICS="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,imb,delta,rep,seconds,rc" > "$METRICS"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"
IFS=',' read -r -a IMBS <<< "$IMB_CSV"
IFS=',' read -r -a DELTAS <<< "$DELTA_CSV"

# -------- helpers ----------
gen_hs() {
  local N="$1" P="$2" IMB="$3" DELTA="$4" out_hs="$5"
  "$PY3" "$GEN_PY" --out "$out_hs" --N "$N" --P "$P" --imb "$IMB" --delta "$DELTA" --vec "$VEC_MODE"
}

compile_hs() {
  local hs="$1" bin="$2"
  "$GHC" -O2 -threaded -rtsopts $GHC_PKGS -outputdir "$(dirname "$bin")" -o "$bin" "$hs" >/dev/null
}

expected_result() {
  local delta="$1"
  if [[ "$delta" -eq 0 ]]; then echo "1"; else echo "0"; fi
}

run_bin() {
  local bin="$1" P="$2" expected="$3"
  local t0 t1 rc=0 result
  t0=$(date +%s%N)
  result="$("$bin" +RTS -N"$P" -RTS 2>/dev/null)" || rc=$?
  t1=$(date +%s%N)
  local secs; secs="$(LC_NUMERIC=C awk -v A="$t0" -v B="$t1" 'BEGIN{ printf "%.6f", (B-A)/1e9 }')"

  # Correctness check
  if [[ "$rc" -eq 0 ]]; then
    local trimmed; trimmed="$(echo "$result" | tr -d '[:space:]')"
    if [[ "$trimmed" != "$expected" ]]; then
      >&2 echo "[WARN ] WRONG ANSWER: got '$trimmed', expected '$expected'"
      rc=99
    fi
  fi

  printf "%s %d" "$secs" "$rc"
}

# -------------- main loop --------------
for P in "${PROCS[@]}"; do
  for IMB in "${IMBS[@]}"; do
    for DELTA in "${DELTAS[@]}"; do
      EXPECTED="$(expected_result "$DELTA")"
      CASE_DIR="$OUTROOT/ghc/N_${N}/P_${P}/imb_${IMB}/delta_${DELTA}"
      BIN_DIR="$CASE_DIR/bin"; mkdir -p "$BIN_DIR"
      HS="$BIN_DIR/dyck_hs_N${N}_P${P}_imb${IMB}_delta${DELTA}.hs"
      BIN="$BIN_DIR/dyck_hs_N${N}_P${P}_imb${IMB}_delta${DELTA}"

      echo "[build_hs] N=${N} P=${P} imb=${IMB} delta=${DELTA}"
      gen_hs "$N" "$P" "$IMB" "$DELTA" "$HS"
      compile_hs "$HS" "$BIN"

      for ((rep=1; rep<=REPS; rep++)); do
        out="$(run_bin "$BIN" "$P" "$EXPECTED")" || true
        secs="NaN"; rc=999
        read -r secs rc <<< "$out" || true
        echo "variant=ghc, N=${N}, P=${P}, imb=${IMB}, delta=${DELTA}, rep=${rep}, secs=${secs}, rc=${rc}"
        echo "ghc,${N},${P},${IMB},${DELTA},${rep},${secs},${rc}" >> "$METRICS"
      done
    done
  done
done

echo "[DONE] métricas em: $METRICS"
echo "[TIP ] Para plotar:  python3 $PLOT_PY --metrics $METRICS --outdir $OUTROOT --tag ${TAG}"
