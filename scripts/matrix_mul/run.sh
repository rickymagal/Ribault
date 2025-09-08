#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# matrix_mul/run.sh — gera HSK, compila, monta, plota
# ============================================================

START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; MAT_KIND="range"; PLOTS="yes"; TAG="mm_plain"
PY2="${PY2:-python2}"
PY3="${PY3:-python3}"

# rr | chunk
PLACE_MODE="${PLACE_MODE:-rr}"

usage(){
  echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" \\"
  echo "          --interp PATH --asm-root PATH --codegen PATH --outroot PATH [--mat range|rand] [--plots yes|no] [--tag nome]"
  echo "env: PLACE_MODE=rr|chunk"
  exit 2
}

# ----------------- parse -----------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step) STEP="$2"; shift 2;;
    --n-max) N_MAX="$2"; shift 2;;
    --reps) REPS="$2"; shift 2;;
    --procs) PROCS_CSV="$2"; shift 2;;
    --interp) INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN_ROOT="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --mat) MAT_KIND="$2"; shift 2;;
    --plots) PLOTS="$2"; shift 2;;
    --tag) TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$START_N$STEP$N_MAX$REPS$PROCS_CSV$INTERP$ASM_ROOT$CODEGEN_ROOT$OUTROOT" ]] || usage

echo "[env ] PY3=${PY3} ; PY2=${PY2}"

[[ -x "$INTERP" ]] || { echo "[ERRO] interp não executável: $INTERP"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" ]] || { echo "[ERRO] ASM_ROOT inválido: $ASM_ROOT"; exit 1; }

if [[ -x "$CODEGEN_ROOT/codegen" ]]; then
  CODEGEN="${CODEGEN_ROOT}/codegen"
else
  echo "[ERRO] não achei 'codegen' em: $CODEGEN_ROOT"; exit 1
fi
echo "[talm ] usando codegen: $CODEGEN"

MM_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$MM_DIR/gen_mm_input.py"
PLOT_PY="$MM_DIR/plot.py"
echo "[hsk ] usando gerador: $GEN_PY"

mkdir -p "$OUTROOT"
OUTROOT="$(readlink -f "$OUTROOT")"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "N,P,rep,seconds,rc" > "$METRICS_CSV"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"

# ----------------- helpers -----------------
gen_hsk() {
  local N="$1" P="$2" out_hsk="$3"
  echo "[build_fl] generating HSK (N=${N}, P=${P})"
  "$PY3" "$GEN_PY" --out "$out_hsk" --N "$N" --P "$P" --mat "$MAT_KIND"
}

build_fl() {
  local hsk="$1" fl="$2"
  echo "[talm ] codegen $hsk -> $fl"
  "$CODEGEN" "$hsk" > "$fl"
}

assemble_baseline() {
  local fl="$1" prefix="$2"

  # use caminhos absolutos para evitar o problema
  local ABS_FL ABS_PREFIX
  ABS_FL="$(readlink -f "$fl")"
  ABS_PREFIX="$(readlink -f "$prefix")"

  [[ -f "$ABS_FL" ]] || { echo "[ERRO] .fl não existe: $ABS_FL"; exit 1; }
  mkdir -p "$(dirname "$ABS_PREFIX")"

  pushd "$ASM_ROOT" >/dev/null
    echo "[asm  ] baseline (sem -a)"
    "$PY2" assembler.py -o "$ABS_PREFIX" "$ABS_FL" >/dev/null
  popd >/dev/null

  [[ -f "${ABS_PREFIX}.flb" && -f "${ABS_PREFIX}.pla" ]] || { echo "[ERRO] baseline não gerou .flb/.pla"; exit 1; }
  cp "${ABS_PREFIX}.pla" "${ABS_PREFIX}.pla.base"
}

rewrite_pla_manual() {
  local prefix="$1" P="$2" mode="$3"
  local base="${prefix}.pla.base"
  local pla="${prefix}.pla"
  local nt; nt="$(head -n1 "$base" | tr -d '\r')"
  [[ "$nt" =~ ^[0-9]+$ ]] || { echo "[ERRO] primeira linha de ${base} inválida"; exit 1; }

  if [[ "$P" -le 1 ]]; then
    echo "[pla  ] P=1 -> tudo no proc 0"
    awk -v N="$nt" 'BEGIN{print N} NR>1{print 0}' "$base" > "$pla"
    return
  fi

  case "$mode" in
    rr|RR)
      echo "[pla  ] manual RR: ntasks=${nt}, P=${P}"
      awk -v P="$P" -v N="$nt" 'BEGIN{print N} NR>1{ i=NR-2; print (i%P) }' "$base" > "$pla"
      ;;
    chunk|CHUNK)
      echo "[pla  ] manual CHUNK: ntasks=${nt}, P=${P}"
      awk -v P="$P" -v N="$nt" 'BEGIN{print N} NR>1{ i=NR-2; printf "%d\n", int((i*P)/N) }' "$base" > "$pla"
      ;;
    *) echo "[ERRO] PLACE_MODE inválido: $mode"; exit 1;;
  esac
}

print_pla_load() {
  local pla="$1" P="$2"
  local line; line="$(awk -v P="$P" '
    NR==1{N=$1; next}
    {c[$1]++}
    END{
      for(i=0;i<P;i++){
        n=(i in c? c[i]:0);
        printf "%d:%d%s", i, n, (i<P-1?" ":"\n")
      }
    }' "$pla")"
  echo "[pla  ] carga: $line"
}

run_interp_time_rc() {
  # stdout: "<secs> <rc>"
  local P="$1" flb="$2" pla="$3" case_dir="$4"
  local logs="$case_dir/logs"; mkdir -p "$logs"
  local outlog="$logs/run.out" errlog="$logs/run.err"

  >&2 echo "[run  ] interp: P=${P}"
  >&2 echo "[run  ] flb=${flb}"
  >&2 echo "[run  ] pla=${pla}"

  local t0 t1 pid rc=0
  t0=$(date +%s%N)
  "$INTERP" "$P" "$flb" "$pla" >"$outlog" 2>"$errlog" &
  pid=$!
  echo "$pid" >"$logs/pid"
  >&2 echo "[run  ] pid=${pid}"

  if ! wait "$pid"; then rc=$?; fi
  t1=$(date +%s%N)
  awk -v A="$t0" -v B="$t1" -v R="$rc" 'BEGIN{ printf "%.6f %d", (B-A)/1e9, R }'
}

# ----------------- main -----------------
N="$START_N"
while [[ "$N" -le "$N_MAX" ]]; do
  for P in "${PROCS[@]}"; do
    CASE_DIR="$OUTROOT/N_${N}/P_${P}"
    mkdir -p "$CASE_DIR"
    HSK="$CASE_DIR/mm_N${N}_P${P}.hsk"
    FL="$CASE_DIR/mm_N${N}_P${P}.fl"
    PREFIX="$CASE_DIR/mm_N${N}_P${P}"

    gen_hsk "$N" "$P" "$HSK"
    build_fl "$HSK" "$FL"
    assemble_baseline "$FL" "$PREFIX"
    rewrite_pla_manual "$PREFIX" "$P" "$PLACE_MODE"
    print_pla_load "${PREFIX}.pla" "$P"

    for ((rep=1; rep<=REPS; rep++)); do
      set +e
      out="$(run_interp_time_rc "$P" "${PREFIX}.flb" "${PREFIX}.pla" "$CASE_DIR")"
      st=$?
      set -e
      secs="NaN"; rc=999
      if [[ $st -eq 0 ]]; then
        read -r secs rc <<< "$out" || { secs="NaN"; rc=998; }
      fi
      echo "N=${N}, P=${P}, rep=${rep}, secs=${secs}, rc=${rc}"
      echo "${N},${P},${rep},${secs},${rc}" >> "$METRICS_CSV"
    done
  done
  N=$((N + STEP))
done

if [[ "$PLOTS" == "yes" ]]; then
  "$PY3" "$PLOT_PY" --metrics "$METRICS_CSV" --outdir "$OUTROOT" --tag "$TAG"
fi

echo "[DONE] resultados em: $OUTROOT"
