#!/usr/bin/env bash
set -euo pipefail

# ============= Dyck (SUPER) sweep =====================
# Fixed N, varies IMB (work imbalance) and P.
# Uses EXEC_TIME_S from interpreter output.
# Checks correctness: delta=0 → expects "1", else "0".
# ======================================================

N=0; REPS=1
PROCS_CSV=""; IMB_CSV=""; DELTA_CSV="0"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; VEC_MODE="range"; PLOTS="yes"; TAG="dyck_super"
PY2="${PY2:-python2}"
PY3="${PY3:-python3}"
PLACE_MODE="${PLACE_MODE:-rr}"

# supers fixa (padrão: test/supers/24_dyck)
SUPERS_FIXED="${SUPERS_FIXED:-}"

usage(){
  echo "uso: $0 --N SIZE --reps R --procs \"1,2,...\" --imb \"0,10,20,...\" [--delta \"0,2,-2\"] \\"
  echo "          --interp PATH --asm-root PATH --codegen PATH --outroot PATH [--plots yes|no] [--tag nome]"
  echo "env: PLACE_MODE=rr|chunk  SUPERS_FIXED=/abs/path/test/supers/24_dyck  LOG_ERR=1"
  exit 2
}

# ----------------- parse -----------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)       N="$2"; shift 2;;
    --reps)    REPS="$2"; shift 2;;
    --procs)   PROCS_CSV="$2"; shift 2;;
    --imb)     IMB_CSV="$2"; shift 2;;
    --delta)   DELTA_CSV="$2"; shift 2;;
    --interp)  INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN_ROOT="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec)     VEC_MODE="$2"; shift 2;;
    --plots)   PLOTS="$2"; shift 2;;
    --tag)     TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ "$N" -gt 0 && -n "$REPS$PROCS_CSV$IMB_CSV$INTERP$ASM_ROOT$CODEGEN_ROOT$OUTROOT" ]] || usage

echo "[env ] PY3=${PY3} ; PY2=${PY2} ; N=${N}"

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
fi

DYCK_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$DYCK_DIR/gen_dyck_input.py"
PLOT_PY="$DYCK_DIR/plot.py"
echo "[hsk ] usando gerador: $GEN_PY"

mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,imb,delta,rep,seconds,rc" > "$METRICS_CSV"

IFS=',' read -r -a PROCS  <<< "$PROCS_CSV"
IFS=',' read -r -a IMBS   <<< "$IMB_CSV"
IFS=',' read -r -a DELTAS <<< "$DELTA_CSV"

# ---------- helpers ----------
abspath(){ local d b; d="$(cd "$(dirname "$1")" && pwd)"; b="$(basename "$1")"; printf "%s/%s" "$d" "$b"; }

gen_hsk() {
  local N="$1" P="$2" IMB="$3" DELTA="$4" out_hsk="$5"
  echo "[build_fl] generating HSK (N=${N}, P=${P}, imb=${IMB}, delta=${DELTA})"
  mkdir -p "$(dirname "$out_hsk")"
  "$PY3" "$GEN_PY" --out "$out_hsk" --N "$N" --P "$P" --imb "$IMB" --delta "$DELTA" --vec "$VEC_MODE"
}

build_fl() {
  local hsk="$1" fl="$2"
  echo "[talm ] codegen $hsk -> $fl"
  mkdir -p "$(dirname "$fl")"
  "$CODEGEN" "$hsk" > "$fl"
  [[ -s "$fl" ]] || { echo "[ERRO] .fl vazio/não criado: $fl"; exit 1; }
}

assemble_baseline() {
  local fl_abs="$1" prefix_abs="$2"
  pushd "$ASM_ROOT" >/dev/null
    echo "[asm  ] baseline (sem -a)"
    "$PY2" assembler.py -o "$prefix_abs" "$fl_abs" >/dev/null
  popd >/dev/null
  [[ -f "${prefix_abs}.flb" && -f "${prefix_abs}.pla" ]] || { echo "[ERRO] baseline não gerou .flb/.pla"; exit 1; }
  cp -f "${prefix_abs}.pla" "${prefix_abs}.pla.base"
}

rewrite_pla_manual() {
  local prefix_abs="$1" P="$2" mode="$3"
  local base="${prefix_abs}.pla.base"
  local pla="${prefix_abs}.pla"
  local nt; nt="$(head -n1 "$base" | tr -d '\r')"
  [[ "$nt" =~ ^[0-9]+$ ]] || { echo "[ERRO] primeira linha de ${base} inválida"; exit 1; }

  if [[ "$P" -le 1 ]]; then
    echo "[pla  ] P=1 -> tudo no proc 0"
    awk 'NR==1{print; next} {print 0}' "$base" > "$pla"
    return
  fi

  case "$mode" in
    rr|RR)
      echo "[pla  ] manual RR: ntasks=${nt}, P=${P}"
      awk -v P="$P" 'NR==1{print; next} {i=NR-2; print (i%P)}' "$base" > "$pla"
      ;;
    chunk|CHUNK)
      echo "[pla  ] manual CHUNK: ntasks=${nt}, P=${P}"
      awk -v P="$P" -v N="$nt" 'NR==1{print; next} {i=NR-2; printf "%d\n", int((i*P)/N)}' "$base" > "$pla"
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

stage_supers_fixed() {
  local src="$1" case_dir="$2"
  local src_abs; src_abs="$(abspath "$src")"
  local dst_abs; dst_abs="$(abspath "$case_dir")/supers/pkg"
  mkdir -p "$dst_abs" || { echo "[ERRO] mkdir falhou: $dst_abs"; exit 1; }

  [[ -f "$src_abs/libsupers.so" ]] || { echo "[ERRO] não encontrei $src_abs/libsupers.so"; exit 1; }
  cp -f "$src_abs/libsupers.so" "$dst_abs/"

  if [[ -d "$src_abs/ghc-deps" ]]; then
    mkdir -p "$dst_abs/ghc-deps"
    cp -f "$src_abs/ghc-deps/"* "$dst_abs/ghc-deps/" || true
  fi
  [[ -f "$src_abs/supers_rts_init.o" ]] && cp -f "$src_abs/supers_rts_init.o" "$dst_abs/" || true
  [[ -f "$src_abs/Supers_stub.h"   ]] && cp -f "$src_abs/Supers_stub.h"   "$dst_abs/" || true
  [[ -f "$src_abs/Supers.hi"       ]] && cp -f "$src_abs/Supers.hi"       "$dst_abs/" || true
  [[ -f "$src_abs/Supers.o"        ]] && cp -f "$src_abs/Supers.o"        "$dst_abs/" || true

  echo "$dst_abs/libsupers.so"
}

# Run interpreter, extract EXEC_TIME_S and check correctness
run_interp() {
  local P="$1" flb_abs="$2" pla_abs="$3" lib="${4:-}" case_dir="$5" expected="$6"
  local logs="$case_dir/logs"; mkdir -p "$logs"
  local outlog="$logs/run.out" errlog="$logs/run.err"

  >&2 echo "[run  ] interp: P=${P}"

  local rc=0
  if [[ -n "$lib" ]]; then
    local libdir; libdir="$(dirname "$lib")"
    local ghcdeps="$libdir/ghc-deps"
    LD_LIBRARY_PATH="$libdir:$ghcdeps" "$INTERP" "$P" "$flb_abs" "$pla_abs" "$lib" >"$outlog" 2>"$errlog" &
  else
    "$INTERP" "$P" "$flb_abs" "$pla_abs" >"$outlog" 2>"$errlog" &
  fi
  local pid=$!
  if ! wait "$pid"; then rc=$?; fi

  # Extract EXEC_TIME_S from output
  local secs="NaN"
  if [[ -f "$outlog" ]]; then
    local et; et="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$outlog" 2>/dev/null || true)"
    [[ -n "$et" ]] && secs="$et"
  fi

  # Correctness check
  if [[ -f "$outlog" && "$rc" -eq 0 ]]; then
    local result; result="$(grep -oP '^\d+$' "$outlog" | head -1 || true)"
    if [[ -n "$result" && "$result" != "$expected" ]]; then
      >&2 echo "[WARN ] WRONG ANSWER: got '$result', expected '$expected'"
      rc=99
    elif [[ -z "$result" ]]; then
      >&2 echo "[WARN ] No result found in output"
      rc=98
    fi
  fi

  printf "%s %d" "$secs" "$rc"
}

# Expected result: delta=0 → 1 (valid), else 0 (invalid)
expected_result() {
  local delta="$1"
  if [[ "$delta" -eq 0 ]]; then echo "1"; else echo "0"; fi
}

# ----------------- main -----------------
for P in "${PROCS[@]}"; do
  for IMB in "${IMBS[@]}"; do
    for DELTA in "${DELTAS[@]}"; do
      EXPECTED="$(expected_result "$DELTA")"
      CASE_DIR="$OUTROOT/super/N_${N}/P_${P}/imb_${IMB}/delta_${DELTA}"
      mkdir -p "$CASE_DIR"
      HSK="$CASE_DIR/dyck_N${N}_P${P}_imb${IMB}_delta${DELTA}.hsk"
      FL="$CASE_DIR/dyck_N${N}_P${P}_imb${IMB}_delta${DELTA}.fl"
      PREFIX="$CASE_DIR/dyck_N${N}_P${P}_imb${IMB}_delta${DELTA}"

      gen_hsk "$N" "$P" "$IMB" "$DELTA" "$HSK"
      build_fl "$HSK" "$FL"

      FL_ABS="$(abspath "$FL")"
      PREFIX_ABS="$(abspath "$PREFIX")"

      assemble_baseline "$FL_ABS" "$PREFIX_ABS"
      rewrite_pla_manual "$PREFIX_ABS" "$P" "$PLACE_MODE"
      print_pla_load "${PREFIX_ABS}.pla" "$P"

      LIBSUP=""
      if [[ -n "$SUPERS_FIXED" ]]; then
        LIBSUP="$(stage_supers_fixed "$SUPERS_FIXED" "$CASE_DIR")"
      fi

      for ((rep=1; rep<=REPS; rep++)); do
        set +e
        out="$(run_interp "$P" "${PREFIX_ABS}.flb" "${PREFIX_ABS}.pla" "$LIBSUP" "$CASE_DIR" "$EXPECTED")"
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

if [[ "$PLOTS" == "yes" ]]; then
  "$PY3" "$PLOT_PY" --metrics "$METRICS_CSV" --outdir "$OUTROOT" --tag "$TAG"
fi

echo "[DONE] resultados em: $OUTROOT"
