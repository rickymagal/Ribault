#!/usr/bin/env bash
set -euo pipefail

START_N=""; STEP=""; NMAX=""; REPS=""
PROCS=""; VARIANTS=""
INTERP=""; ASM_ROOT=""; CODEGEN=""; SUPERSGEN=""
OUTROOT=""; VEC="desc"; PLOTS="no"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N)   START_N="$2"; shift 2;;
    --step)      STEP="$2";    shift 2;;
    --n-max)     NMAX="$2";    shift 2;;
    --reps)      REPS="$2";    shift 2;;
    --procs)     PROCS="$2";   shift 2;;
    --variants)  VARIANTS="$2";shift 2;;
    --interp)    INTERP="$2";  shift 2;;
    --asm-root)  ASM_ROOT="$2";shift 2;;
    --codegen)   CODEGEN="$2"; shift 2;;
    --supersgen) SUPERSGEN="$2"; shift 2;;
    --outroot)   OUTROOT="$2"; shift 2;;
    --vec)       VEC="$2";     shift 2;;
    --plots)     PLOTS="$2";   shift 2;;
    *) echo "flag desconhecida: $1"; exit 2;;
  esac
done

for v in START_N STEP NMAX REPS PROCS VARIANTS INTERP ASM_ROOT CODEGEN SUPERSGEN OUTROOT; do
  [[ -n "${!v}" ]] || { echo "erro: faltou --${v,,}"; exit 2; }
done

echo "[until-fail] INTERP=$INTERP"
echo "[until-fail] ASM_ROOT=$ASM_ROOT"
echo "[until-fail] CODEGEN=$CODEGEN"
echo "[until-fail] SUPERSGEN=$SUPERSGEN"
echo "[until-fail] OUTROOT=$OUTROOT"
echo "[until-fail] PROCS=$PROCS VARIANTS=$VARIANTS REPS=$REPS VEC=$VEC"

REPO_ROOT="$(cd "$(dirname "$0")/.."; pwd)"
mkdir -p "$OUTROOT"

# --------- prepara lib da SUPER no OUTROOT ---------
SUPERS_NAME="21_merge_sort_super"
SUPER_SRC_DIR="${REPO_ROOT}/test/supers/${SUPERS_NAME}"   # <- CORRIGIDO
SUPER_OUT_DIR="${OUTROOT}/_super_lib"
mkdir -p "${SUPER_OUT_DIR}/ghc-deps"

copy_super_outputs() {
  cp -f "${SUPER_SRC_DIR}/libsupers.so" "${SUPER_OUT_DIR}/libsupers.so"
  [[ -d "${SUPER_SRC_DIR}/ghc-deps" ]] && cp -f "${SUPER_SRC_DIR}/ghc-deps/"* "${SUPER_OUT_DIR}/ghc-deps/" || true
  [[ -f "${SUPER_SRC_DIR}/supers_rts_init.o" ]] && cp -f "${SUPER_SRC_DIR}/supers_rts_init.o" "${SUPER_OUT_DIR}/" || true
}

if [[ -s "${SUPER_SRC_DIR}/libsupers.so" ]]; then
  copy_super_outputs || true
else
  echo "[until-fail] libsupers.so não encontrada em ${SUPER_SRC_DIR}; tentando 'make supers'…"
  ( cd "$REPO_ROOT" && make supers )
  copy_super_outputs
fi

if [[ ! -s "${SUPER_OUT_DIR}/libsupers.so" ]]; then
  echo "erro: não consegui gerar/copiar libsupers.so em ${SUPER_OUT_DIR}"; exit 1
fi

SUPERS_LIB="${SUPER_OUT_DIR}/libsupers.so"
EXTRA_LD_SUPER="${SUPER_OUT_DIR}/ghc-deps:${SUPER_OUT_DIR}:${ASM_ROOT}/../interp/ghc-deps:${ASM_ROOT}/../interp/rts-local"

IFS=',' read -r -a PROCS_ARR <<< "$PROCS"
IFS=',' read -r -a VARS_ARR  <<< "$VARIANTS"

run_case() {
  local variant="$1" N="$2" P="$3"
  local base_dir="${OUTROOT}/${variant}/N_${N}/P_${P}"
  mkdir -p "$base_dir"

  local base="${base_dir}/mergesort_${variant}_N${N}_P${P}"
  local HSK="${base}.hsk"
  local FL="${base}.fl"
  local FLB="${base}.flb"
  local PLA="${base}.pla"

  # 1) HSK (p = P; vetor conforme --vec)
  python3 "${REPO_ROOT}/scripts/_ms_gen_input.py" \
    --variant "$variant" --N "$N" --P "$P" --vec "$VEC" --out "$HSK"

  # 2) HSK -> FL
  bash "${REPO_ROOT}/scripts/_build_fl_from_hsk.sh" \
    --variant "$variant" \
    --codegen "$CODEGEN" \
    --supersgen "$SUPERSGEN" \
    --in "$HSK" --out "$FL" \
    --log "${base_dir}/codegen.log"

  [[ -s "$FL" ]] || { echo "error: faltou FL ($FL)"; exit 1; }

  # 3) .fl -> .flb/.pla
  bash "${REPO_ROOT}/scripts/_assemble_fl.sh" \
    --asm-root "$ASM_ROOT" \
    --fl "$FL" \
    --outbase "$base" \
    --P "$P"

  # 4) reps
  for rep in $(seq 1 "$REPS"); do
    local rep_dir="${base_dir}/rep_${rep}"
    mkdir -p "$rep_dir"
    echo "---- run: variant=${variant} N=${N} P=${P} rep=${rep} -> ${rep_dir}"
    if [[ "$variant" == "super" ]]; then
      python3 "${REPO_ROOT}/scripts/_run_interp.py" \
        --interp "$INTERP" \
        --flb "$FLB" \
        --pla "$PLA" \
        --P "$P" \
        --lib "$SUPERS_LIB" \
        --extra-ld "$EXTRA_LD_SUPER" \
        --cwd "$rep_dir"
    else
      python3 "${REPO_ROOT}/scripts/_run_interp.py" \
        --interp "$INTERP" \
        --flb "$FLB" \
        --pla "$PLA" \
        --P "$P" \
        --cwd "$rep_dir"
    fi
  done
}

for variant in "${VARS_ARR[@]}"; do
  N="$START_N"
  while [[ "$N" -le "$NMAX" ]]; do
    for P in "${PROCS_ARR[@]}"; do
      run_case "$variant" "$N" "$P"
    done
    N=$(( N + STEP ))
  done
done

python3 "${REPO_ROOT}/scripts/_collect_metrics.py" --root "$OUTROOT" || true

if [[ "$PLOTS" == "yes" ]]; then
  python3 "${REPO_ROOT}/scripts/_plot_scaling.py" --root "$OUTROOT" || true
  python3 "${REPO_ROOT}/scripts/_plot_vs_n.py"     --root "$OUTROOT" || true
fi

echo "[until-fail] DONE. Resultados em: $OUTROOT"
