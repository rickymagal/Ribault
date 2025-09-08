#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# dyck/run.sh — SEM AUTOPLACE + .PLA manual (RR/CHUNK)
# Varre N (start/step/max), P (csv), DELTA (csv) e IMB (csv)
# ============================================================

START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; PLOTS="yes"; TAG="dyck_super"
IMB_CSV="1"; DELTA_CSV="0"
PY2="${PY2:-python2}"
PY3="${PY3:-python3}"

# rr | chunk
PLACE_MODE="${PLACE_MODE:-rr}"

# supers fixa (default: test/supers/24_dyck)
SUPERS_FIXED="${SUPERS_FIXED:-}"

usage(){
  echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" \\"
  echo "          --interp PATH --asm-root PATH --codegen PATH --outroot PATH \\"
  echo "          --imb \"1,25,50\" --delta \"0,2,-2\" [--plots yes|no] [--tag nome]"
  echo "env: PLACE_MODE=rr|chunk  SUPERS_FIXED=/abs/path/test/supers/24_dyck  LOG_ERR=1"
  exit 2
}

# ----------------- parse -----------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step)    STEP="$2"; shift 2;;
    --n-max)   N_MAX="$2"; shift 2;;
    --reps)    REPS="$2"; shift 2;;
    --procs)   PROCS_CSV="$2"; shift 2;;
    --interp)  INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN_ROOT="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --imb)     IMB_CSV="$2"; shift 2;;
    --delta)   DELTA_CSV="$2"; shift 2;;
    --plots)   PLOTS="$2"; shift 2;;
    --tag)     TAG="$2"; shift 2;;
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

# SUPERS fixa
if [[ -z "${SUPERS_FIXED}" ]]; then
  CAND="${CODEGEN_ROOT}/test/supers/24_dyck"
  [[ -f "$CAND/libsupers.so" ]] && SUPERS_FIXED="$CAND" || SUPERS_FIXED=""
fi
if [[ -n "$SUPERS_FIXED" ]]; then
  echo "[sup ] usando supers fixa: $SUPERS_FIXED"
  [[ -f "$SUPERS_FIXED/libsupers.so" ]] || { echo "[ERRO] libsupers.so não encontrada em SUPERS_FIXED"; exit 1; }
else
  echo "[ERRO] SUPERS_FIXED não definida e fallback não encontrado (test/supers/24_dyck)"; exit 1
fi

DYCK_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$DYCK_DIR/gen_dyck_input.py"
PLOT_PY="$DYCK_DIR/plot.py"
echo "[hsk ] usando gerador: $GEN_PY"

rm -rf "$OUTROOT"; mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,imb,delta,rep,seconds,rc" > "$METRICS_CSV"

IFS=',' read -r -a PROCS   <<< "$PROCS_CSV"
IFS=',' read -r -a IMBS    <<< "$IMB_CSV"
IFS=',' read -r -a DELTAS  <<< "$DELTA_CSV"

# ----------------- helpers -----------------
gen_hsk() {
  local N="$1" P="$2" IMB="$3" DELTA="$4" out_hsk="$5"
  echo "[build_fl] generating HSK (N=${N}, P=${P}, imb=${IMB}, delta=${DELTA})"
  "$PY3" "$GEN_PY" --out "$out_hsk" --N "$N" --P "$P" --imb "$IMB" --delta "$DELTA"
}

build_fl() {
  local hsk="$1" fl="$2"
  echo "[talm ] codegen $hsk -> $fl"
  "$CODEGEN" "$hsk" > "$fl"
  [[ -s "$fl" ]] || { echo "[ERRO] codegen não gerou $fl"; exit 1; }
}

assemble_baseline() {
  local fl="$1" prefix="$2"
  local fl_abs; fl_abs="$(readlink -f "$fl")"
  local prefix_abs; prefix_abs="$(readlink -f "$prefix")"

  pushd "$ASM_ROOT" >/dev/null
    echo "[asm  ] baseline (sem -a)"
    "$PY2" assembler.py -o "$prefix_abs" "$fl_abs" >/dev/null
  popd >/dev/null

  [[ -f "${prefix_abs}.flb" && -f "${prefix_abs}.pla" ]] || { echo "[ERRO] baseline não gerou .flb/.pla"; exit 1; }
  cp "${prefix_abs}.pla" "${prefix_abs}.pla.base"
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

# ===== FIX ROBUSTO AQUI =====
stage_supers_fixed() {
  local src="$1" case_dir="$2"
  local dst="$case_dir/supers/pkg"

  # cria destino e resolve caminho físico
  mkdir -p "$dst"
  local dst_abs
  dst_abs="$(cd "$dst" && pwd -P)"

  echo "[sup ] staging supers: src=$src -> dst=$dst_abs"

  # cópia robusta (evita problemas do cp -a com espaços/acentos)
  (cd "$src" && tar cf - .) | (cd "$dst_abs" && tar xpf -)

  [[ -f "$dst_abs/libsupers.so" ]] || { echo "[ERRO] cópia da supers falhou"; exit 1; }
  echo "$dst_abs/libsupers.so"
}

run_interp_time_rc() {
  # stdout: "<secs> <rc>"
  local P="$1" flb="$2" pla="$3" lib="${4:-}" case_dir="$5"
  local logs="$case_dir/logs"; mkdir -p "$logs"
  local outlog="$logs/run.out" errlog="$logs/run.err"

  >&2 echo "[run  ] interp: P=${P}"
  >&2 echo "[run  ] flb=${flb}"
  >&2 echo "[run  ] pla=${pla}"
  [[ -n "$lib" ]] && >&2 echo "[run  ] lib=${lib}"

  local t0 t1 pid rc=0
  t0=$(date +%s%N)

  if [[ -n "$lib" ]]; then
    local libdir; libdir="$(dirname "$lib")"
    local ghcdeps="$libdir/ghc-deps"
    LD_LIBRARY_PATH="$libdir:$ghcdeps" "$INTERP" "$P" "$flb" "$pla" "$lib" >"$outlog" 2>"$errlog" &
  else
    "$INTERP" "$P" "$flb" "$pla" >"$outlog" 2>"$errlog" &
  fi
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
    for IMB in "${IMBS[@]}"; do
      for DELTA in "${DELTAS[@]}"; do
        CASE_DIR="$OUTROOT/super/N_${N}/P_${P}/imb_${IMB}/delta_${DELTA}"
        mkdir -p "$CASE_DIR"
        HSK="$CASE_DIR/dyck_super_N${N}_P${P}_imb${IMB}_delta${DELTA}.hsk"
        FL="$CASE_DIR/dyck_super_N${N}_P${P}_imb${IMB}_delta${DELTA}.fl"
        PREFIX="$CASE_DIR/dyck_super_N${N}_P${P}_imb${IMB}_delta${DELTA}"

        gen_hsk "$N" "$P" "$IMB" "$DELTA" "$HSK"
        build_fl "$HSK" "$FL"
        assemble_baseline "$FL" "$PREFIX"
        rewrite_pla_manual "$PREFIX" "$P" "$PLACE_MODE"
        print_pla_load "${PREFIX}.pla" "$P"

        LIBSUP="$(stage_supers_fixed "$SUPERS_FIXED" "$CASE_DIR")"

        for ((rep=1; rep<=REPS; rep++)); do
          set +e
          out="$(run_interp_time_rc "$P" "${PREFIX}.flb" "${PREFIX}.pla" "$LIBSUP" "$CASE_DIR")"
          st=$?
          set -e
          secs="NaN"; rc=999
          if [[ $st -eq 0 ]]; then
            read -r secs rc <<< "$out" || { secs="NaN"; rc=998; }
          fi

          echo "variant=super, N=${N}, P=${P}, imb=${IMB}, delta=${DELTA}, rep=${rep}, secs=${secs}, rc=${rc}"
          echo "super,${N},${P},${IMB},${DELTA},${rep},${secs},${rc}" >> "$METRICS_CSV"

          if [[ "${LOG_ERR:-0}" -eq 1 && "$rc" -ne 0 ]]; then
            echo "[err ] $CASE_DIR/logs/run.err"
            sed -n '1,120p' "$CASE_DIR/logs/run.err" || true
          fi
        done
      done
    done
  done
  N=$((N + STEP))
done

if [[ "$PLOTS" == "yes" ]]; then
  "$PY3" "$PLOT_PY" --metrics "$METRICS_CSV" --outdir "$OUTROOT" --tag "$TAG"
fi

echo "[DONE] resultados em: $OUTROOT"
