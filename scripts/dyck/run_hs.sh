#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# dyck/run_hs.sh — GHC-parallel baseline (gera .hs, compila e mede)
# Saída: results/<...>/metrics_<TAG>.csv
# Requer: ghc (+packages parallel, deepseq)
# ============================================================

START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; OUTROOT=""; VEC_MODE="range"; TAG="hs_dyck"
IMB_CSV=""; DELTA_CSV=""
GHC="${GHC:-ghc}"
GHC_PKGS_DEFAULT="-package parallel -package deepseq"
GHC_PKGS="${GHC_PKGS:-$GHC_PKGS_DEFAULT}"
PY3="${PY3:-python3}"

usage(){
  echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" \\"
  echo "          --imb \"1,25,...\" --delta \"0,2,-2\" --outroot PATH --vec range|rand --tag TAG"
  echo "env: GHC, GHC_PKGS (ex: '-package parallel -package deepseq'), PY3"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step) STEP="$2"; shift 2;;
    --n-max) N_MAX="$2"; shift 2;;
    --reps) REPS="$2"; shift 2;;
    --procs) PROCS_CSV="$2"; shift 2;;
    --imb) IMB_CSV="$2"; shift 2;;
    --delta) DELTA_CSV="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec) VEC_MODE="$2"; shift 2;;
    --tag) TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$START_N$STEP$N_MAX$REPS$PROCS_CSV$OUTROOT$IMB_CSV$DELTA_CSV" ]] || usage

echo "[env ] GHC=${GHC} ; GHC_PKGS=${GHC_PKGS} ; PY3=${PY3}"

BASE_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$BASE_DIR/gen_hs_input.py"
PLOT_HS="$BASE_DIR/plot_hs.py"
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

run_bin_time_rc() {
  # stdout: "<secs> <rc>"
  local bin="$1" P="$2"
  local t0 t1 rc=0
  t0=$(date +%s%N)
  if ! "$bin" +RTS -N"$P" -RTS >/dev/null 2>&1; then rc=$?; fi
  t1=$(date +%s%N)
  awk -v A="$t0" -v B="$t1" -v R="$rc" 'BEGIN{ printf "%.6f %d", (B-A)/1e9, R }'
}

# -------------- main loop --------------
for N in $(seq "$START_N" "$STEP" "$N_MAX"); do
  for P in "${PROCS[@]}"; do
    for IMB in "${IMBS[@]}"; do
      for DELTA in "${DELTAS[@]}"; do
        CASE_DIR="$OUTROOT/ghc/N_${N}/P_${P}/imb_${IMB}/delta_${DELTA}"
        BIN_DIR="$CASE_DIR/bin"; mkdir -p "$BIN_DIR"
        HS="$BIN_DIR/dyck_hs_N${N}_P${P}_imb${IMB}_delta${DELTA}.hs"
        BIN="$BIN_DIR/dyck_hs_N${N}_P${P}_imb${IMB}_delta${DELTA}"

        echo "[build_hs] N=${N} P=${P} imb=${IMB} delta=${DELTA}"
        gen_hs "$N" "$P" "$IMB" "$DELTA" "$HS"
        compile_hs "$HS" "$BIN"

        for ((rep=1; rep<=REPS; rep++)); do
          out="$(run_bin_time_rc "$BIN" "$P")" || true
          secs="NaN"; rc=999
          read -r secs rc <<< "$out" || true
          echo "variant=ghc, N=${N}, P=${P}, imb=${IMB}, delta=${DELTA}, rep=${rep}, secs=${secs}, rc=${rc}"
          echo "ghc,${N},${P},${IMB},${DELTA},${rep},${secs},${rc}" >> "$METRICS"
        done
      done
    done
  done
done

echo "[DONE] métricas em: $METRICS"
echo "[TIP ] Para plotar:  python3 $PLOT_HS --metrics $METRICS --outdir $OUTROOT --tag ${TAG}"
