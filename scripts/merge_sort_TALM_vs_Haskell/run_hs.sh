#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# merge_sort/run_hs.sh — baseline GHC parallel (-N)
# ============================================================

START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; OUTROOT=""; VEC_MODE="range"; TAG="ghc_ms"
GEN_OVERRIDE=""
VARIANT="ghc"
GHC="${GHC:-ghc}"
GHC_PKGS="${GHC_PKGS:--package time}"
PY3="${PY3:-python3}"

usage(){
  echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" --outroot DIR [--vec range|rand] [--tag nome] [--gen script.py] [--variant name]"
  echo "env: GHC=ghc  PY3=python3"
  exit 2
}

# ----------------- parse -----------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step)    STEP="$2";    shift 2;;
    --n-max)   N_MAX="$2";   shift 2;;
    --reps)    REPS="$2";    shift 2;;
    --procs)   PROCS_CSV="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec)     VEC_MODE="$2"; shift 2;;
    --tag)     TAG="$2"; shift 2;;
    --gen)     GEN_OVERRIDE="$2"; shift 2;;
    --variant) VARIANT="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$START_N$STEP$N_MAX$REPS$PROCS_CSV$OUTROOT" ]] || usage

command -v "$GHC" >/dev/null || { echo "[ERRO] GHC não encontrado: $GHC"; exit 1; }
echo "[env ] GHC=${GHC} ; PY3=${PY3} ; GHC_PKGS=${GHC_PKGS}"

MS_DIR="$(cd "$(dirname "$0")" && pwd)"
VALIDATE="$MS_DIR/validate.py"
if [[ -n "$GEN_OVERRIDE" ]]; then
  GEN_HS="$GEN_OVERRIDE"
else
  GEN_HS="$MS_DIR/gen_hs_input.py"
fi
[[ -f "$GEN_HS" ]] || { echo "[ERRO] não achei: $GEN_HS"; exit 1; }
echo "[hs  ] usando gerador: $GEN_HS"

mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,rep,seconds,rc" > "$METRICS_CSV"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"

# ----------------- helpers -----------------
gen_hs() {
  local N="$1" P="$2" out_hs="$3"
  "$PY3" "$GEN_HS" --out "$out_hs" --N "$N" --P "$P"
}

build_bin() {
  local hs="$1" bin="$2"
  local hs_dir
  hs_dir="$(dirname "$hs")"
  local c_helper="$hs_dir/msort_helpers.c"
  echo "[ghc ] compiling $hs -> $bin"
  if [[ -f "$c_helper" ]]; then
    local c_obj="$hs_dir/msort_helpers.o"
    gcc -O2 -c "$c_helper" -o "$c_obj"
    "$GHC" -O2 $GHC_PKGS -threaded -rtsopts -o "$bin" "$c_obj" "$hs" >/dev/null
  else
    "$GHC" -O2 $GHC_PKGS -threaded -rtsopts -o "$bin" "$hs" >/dev/null
  fi
}

run_bin_time_rc() {
  # stdout: "<secs> <rc>"
  local bin="$1" P="$2" logs="$3"
  mkdir -p "$logs"
  local outlog="$logs/run.out" errlog="$logs/run.err"
  set +e
  "$bin" +RTS -N"$P" -RTS >"$outlog" 2>"$errlog"
  local rc=$?
  set -e
  local secs="NaN"
  if [[ $rc -eq 0 ]]; then
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2; found=1} END{if(!found) print "NaN"}' "$outlog")"
    # Verify sorting correctness
    local sorted_val
    sorted_val="$(awk -F= '/^SORTED=/{print $2}' "$outlog" 2>/dev/null || true)"
    if [[ -n "$sorted_val" && "$sorted_val" != "True" ]]; then
      >&2 echo "[ERR ] SORT INCORRECT: SORTED=$sorted_val"
      rc=99
    elif [[ -z "$sorted_val" ]]; then
      >&2 echo "[WARN] SORTED= line missing from output"
    fi
  fi
  echo "$secs $rc"
}

# ----------------- main -----------------
N="$START_N"
while [[ "$N" -le "$N_MAX" ]]; do
  for P in "${PROCS[@]}"; do
    CASE_DIR="$OUTROOT/ghc/N_${N}/P_${P}"
    mkdir -p "$CASE_DIR/bin" "$CASE_DIR/logs"
    HS="$CASE_DIR/bin/ms_hs_N${N}_P${P}.hs"
    BIN="$CASE_DIR/bin/ms_hs_N${N}_P${P}"
    gen_hs "$N" "$P" "$HS"
    build_bin "$HS" "$BIN"

    for ((rep=1; rep<=REPS; rep++)); do
      out="$(run_bin_time_rc "$BIN" "$P" "$CASE_DIR/logs")"
      read -r secs rc <<< "$out"
      # External validation
      if [[ "$rc" -eq 0 && -f "$VALIDATE" ]]; then
        "$PY3" "$VALIDATE" "$CASE_DIR/logs/run.out" "$N" || rc=98
      fi
      echo "variant=ghc, N=${N}, P=${P}, rep=${rep}, secs=${secs}, rc=${rc}"
      if [[ "$rc" -ne 0 ]]; then
        echo "FATAL: ${VARIANT} N=${N} P=${P} rep=${rep} failed with rc=${rc}"
        exit 1
      fi
      echo "${VARIANT},${N},${P},${rep},${secs},${rc}" >> "$METRICS_CSV"
    done
  done
  N=$((N + STEP))
done

echo "[DONE] resultados em: $OUTROOT"
