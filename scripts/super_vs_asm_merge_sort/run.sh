#!/usr/bin/env bash
set -euo pipefail

# ================= args =================
START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; VARIANTS=""
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; VEC_MODE="range"; PLOTS="yes"; TAG="super_v_asm"

# opcionais
PY3="${PY3:-python3}"
PY2="${PY2:-python2}"
LOG_ERR="${LOG_ERR:-0}"          # 1 = grava stdout/stderr por repetição em .../logs/
DLDBG="${DLDBG:-0}"              # 1 = LD_DEBUG=libs,files
SUPERS_FIXED=""                  # caminho absoluto para test/supers/21_merge_sort_super

usage(){
  echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" --variants \"asm super\" --interp PATH --asm-root PATH --codegen PATH --outroot PATH [--vec range|rand] [--plots yes|no] [--tag nome] [--supers-fixed DIR]"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step) STEP="$2"; shift 2;;
    --n-max) N_MAX="$2"; shift 2;;
    --reps) REPS="$2"; shift 2;;
    --procs) PROCS_CSV="$2"; shift 2;;
    --variants) VARIANTS="$2"; shift 2;;
    --interp) INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN_ROOT="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec) VEC_MODE="$2"; shift 2;;
    --plots) PLOTS="$2"; shift 2;;
    --tag) TAG="$2"; shift 2;;
    --supers-fixed) SUPERS_FIXED="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$START_N$STEP$N_MAX$REPS$PROCS_CSV$VARIANTS$INTERP$ASM_ROOT$CODEGEN_ROOT$OUTROOT" ]] || usage
[[ -x "$INTERP" ]] || { echo "interp não executável: $INTERP"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" && -f "$ASM_ROOT/scheduler.py" ]] || { echo "ASM_ROOT inválido: $ASM_ROOT"; exit 1; }
[[ -x "$CODEGEN_ROOT/codegen" ]] || { echo "codegen não encontrado em $CODEGEN_ROOT/codegen"; exit 1; }

MS_DIR="$(cd "$(dirname "$0")" && pwd)"

# Descobre supers fixa se não foi passada
if [[ -z "$SUPERS_FIXED" ]]; then
  for rel in "../../test/supers/21_merge_sort_super" "../test/supers/21_merge_sort_super" "../../../test/supers/21_merge_sort_super"; do
    try="$(cd "$MS_DIR/$rel" 2>/dev/null && pwd || true)"
    if [[ -n "$try" && -f "$try/libsupers.so" && -d "$try/ghc-deps" ]]; then SUPERS_FIXED="$try"; break; fi
  done
fi
[[ -n "$SUPERS_FIXED" ]] || { echo "[ERRO] não achei test/supers/21_merge_sort_super. Use --supers-fixed DIR"; exit 1; }
[[ -f "$SUPERS_FIXED/libsupers.so" && -d "$SUPERS_FIXED/ghc-deps" ]] || { echo "[ERRO] supers fixa inválida em: $SUPERS_FIXED"; exit 1; }

GEN_PY="$MS_DIR/_ms_gen_input.py"
PLOT_PY="$MS_DIR/plot.py"

echo "[env ] PY3=$PY3 ; PY2=$PY2"
echo "[hsk ] usando gerador: $GEN_PY"
echo "[sup ] usando supers fixa: $SUPERS_FIXED"

mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,rep,seconds,rc" > "$METRICS_CSV"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"
read -r -a VARIANTS_ARR <<< "$VARIANTS"

# ================= helpers =================
gen_hsk() {
  local variant="$1" N="$2" P="$3" out_hsk="$4"
  echo "[build_fl] generating HSK (N=${N}, variant=${variant}, threads=${P})"
  "$PY3" "$GEN_PY" --out "$out_hsk" --variant "$variant" --N "$N" --P "$P" --vec "$VEC_MODE"
}

build_fl() {
  local hsk="$1" fl="$2"
  echo "[talm ] codegen $hsk -> $fl"
  "$CODEGEN_ROOT/codegen" "$hsk" > "$fl"
}

assemble_with_place() {
  local fl="$1" prefix="$2" P="$3"
  pushd "$ASM_ROOT" >/dev/null

    if [[ "$P" -le 1 ]]; then
      echo "[asm  ] baseline (P=1)" >&2
      "$PY2" assembler.py -o "$prefix" "$fl" >/dev/null 2>&1
      [[ -f "${prefix}.flb" && -f "${prefix}.pla" ]] || { echo "ERRO: baseline não gerou flb/pla (P=1)"; popd >/dev/null; exit 1; }
      # força mapeamento sequencial (todos em 0), mantendo 1a linha = ntasks
      local nt; nt=$(head -n1 "${prefix}.pla" | tr -d '\r')
      awk -v N="$nt" 'BEGIN{print N} NR>1{print 0}' "${prefix}.pla" > "${prefix}.pla.seq"
      mv "${prefix}.pla.seq" "${prefix}.pla"
    else
      echo "[asm  ] autoplace -n $P (sem timeout, sem fallback)" >&2
      "$PY2" assembler.py -a -n "$P" -o "$prefix" "$fl" >/dev/null 2>&1 || {
        echo "ERRO: assembler.py -a -n $P falhou"; popd >/dev/null; exit 1;
      }
      [[ -f "${prefix}_auto.pla" && -f "${prefix}.flb" ]] || {
        echo "ERRO: autoplace não gerou ${prefix}_auto.pla ou ${prefix}.flb"; popd >/dev/null; exit 1;
      }
      mv "${prefix}_auto.pla" "${prefix}.pla"
      echo "[asm  ] usando placement automático: ${prefix}.pla" >&2
    fi

  popd >/dev/null
  echo "${prefix}.flb|${prefix}.pla"
}

run_interp_time() {
  local P="$1" flb="$2" pla="$3" lib="${4:-}" case_dir="$5" rep="$6"
  local -a envvars
  local libpath="$SUPERS_FIXED:$SUPERS_FIXED/ghc-deps"
  envvars+=( "LD_LIBRARY_PATH=${libpath}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" )
  [[ "$DLDBG" == "1" ]] && envvars+=( "LD_DEBUG=libs,files" )

  local t0 t1 secs rc
  t0=$(date +%s%N)

  if [[ "$LOG_ERR" == "1" ]]; then
    mkdir -p "$case_dir/logs"
    local OUT="$case_dir/logs/rep_${rep}.out"
    local ERR="$case_dir/logs/rep_${rep}.err"
    set +e
    if [[ -n "$lib" ]]; then
      env "${envvars[@]}" "$INTERP" "$P" "$flb" "$pla" "$lib" >"$OUT" 2>"$ERR"
    else
      env "${envvars[@]}" "$INTERP" "$P" "$flb" "$pla" >"$OUT" 2>"$ERR"
    fi
    rc=$?
    set -e
  else
    set +e
    if [[ -n "$lib" ]]; then
      env "${envvars[@]}" "$INTERP" "$P" "$flb" "$pla" "$lib" >/dev/null 2>&1
    else
      env "${envvars[@]}" "$INTERP" "$P" "$flb" "$pla" >/dev/null 2>&1
    fi
    rc=$?
    set -e
  fi

  t1=$(date +%s%N)
  secs=$(awk -v A="$t0" -v B="$t1" 'BEGIN{printf "%.6f",(B-A)/1e9}')
  echo "$secs|$rc"
}

# ================= main =================
for variant in "${VARIANTS_ARR[@]}"; do
  N="$START_N"
  while [[ "$N" -le "$N_MAX" ]]; do
    for P in "${PROCS[@]}"; do
      CASE_DIR="$OUTROOT/$variant/N_${N}/P_${P}"
      mkdir -p "$CASE_DIR"
      HSK="$CASE_DIR/mergesort_${variant}_N${N}_P${P}.hsk"
      FL="$CASE_DIR/mergesort_${variant}_N${N}_P${P}.fl"
      PREFIX="$CASE_DIR/mergesort_${variant}_N${N}_P${P}"

      gen_hsk "$variant" "$N" "$P" "$HSK"
      [[ -f "$HSK" ]] || { echo "[ERRO] geração do HSK"; exit 1; }

      build_fl "$HSK" "$FL"
      [[ -f "$FL" ]] || { echo "[ERRO] FL não gerado: $FL"; exit 1; }

      LIBSUP=""
      if [[ "$variant" == "super" ]]; then
        LIBSUP="$SUPERS_FIXED/libsupers.so"
        [[ -f "$LIBSUP" ]] || { echo "ERRO: libsupers.so não encontrada: $LIBSUP"; exit 1; }
      fi

      IFS='|' read -r FLB PLA < <(assemble_with_place "$FL" "$PREFIX" "$P")
      [[ -f "$FLB" && -f "$PLA" ]] || { echo "ERRO: FLB/PLA não gerados"; exit 1; }

      for ((rep=1; rep<=REPS; rep++)); do
        IFS='|' read -r secs rc < <(run_interp_time "$P" "$FLB" "$PLA" "$LIBSUP" "$CASE_DIR" "$rep")
        echo "variant=${variant}, N=${N}, P=${P}, rep=${rep}, secs=${secs}, rc=${rc}"
        echo "${variant},${N},${P},${rep},${secs},${rc}" >> "$METRICS_CSV"
        if [[ "$LOG_ERR" == "1" && "$rc" != "0" ]]; then
          echo "[err ] $CASE_DIR/logs/rep_${rep}.err"
        fi
      done
    done
    N=$((N + STEP))
  done
done

if [[ "$PLOTS" == "yes" ]]; then
  "$PY3" "$PLOT_PY" --metrics "$METRICS_CSV" --outdir "$OUTROOT" --tag "$TAG"
fi

echo "[DONE] resultados em: $OUTROOT"
