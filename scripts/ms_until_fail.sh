#!/usr/bin/env bash
set -euo pipefail

# Defaults
START_N=1
STEP=1
N_MAX=1
REPS=1
PROCS=1
VARIANTS="asm"
INTERP="${INTERP:-/home/ricky/Área de trabalho/TALM/interp/interp}"
ASM_ROOT=""
CODEGEN=""
OUTROOT="./results"
VEC="ones"
PLOTS="no"
STRACE_BIN="${STRACE:-}"

SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<EOF
uso: $0 --start-N <n0> --step <k> --n-max <nmax> --reps <r> --procs <P> --variants <asm|super> \
        --interp <interp_bin> --asm-root <dir> --codegen <dir> --outroot <dir> --vec <ones|desc> [--plots no|yes]
EOF
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step)    STEP="$2"; shift 2;;
    --n-max)   N_MAX="$2"; shift 2;;
    --reps)    REPS="$2"; shift 2;;
    --procs)   PROCS="$2"; shift 2;;
    --variants) VARIANTS="$2"; shift 2;;
    --interp)  INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec)     VEC="$2"; shift 2;;
    --plots)   PLOTS="$2"; shift 2;;
    *) echo "[until-fail][ERRO] arg desconhecido: $1"; usage;;
  esac
done

[[ -z "${INTERP:-}" ]] && { echo "[until-fail][ERRO] --interp vazio"; exit 2; }
[[ -z "${ASM_ROOT:-}" ]] && { echo "[until-fail][ERRO] --asm-root requerido"; exit 2; }
[[ -z "${CODEGEN:-}" ]]  && { echo "[until-fail][ERRO] --codegen requerido"; exit 2; }

echo "[until-fail] INTERP=$INTERP"
echo "[until-fail] ASM_ROOT=$ASM_ROOT"
echo "[until-fail] CODEGEN=$CODEGEN"
echo "[until-fail] SUPERSGEN=${SUPERSGEN:-}"
echo "[until-fail] OUTROOT=$OUTROOT"
echo "[until-fail] PROCS=$PROCS VARIANTS=$VARIANTS REPS=$REPS VEC=$VEC"
echo "[until-fail] STRACE_BIN=${STRACE_BIN:-}"

# Loop (aqui você está rodando 1 N só, mas deixei genérico)
for (( N=$START_N; N<=$N_MAX; N+=$STEP )); do
  for variant in $VARIANTS; do
    OUTDIR="$OUTROOT/$variant/N_${N}/P_${PROCS}"
    mkdir -p "$OUTDIR"

    # 1) Gera .hsk
    HSK="$OUTDIR/mergesort_${variant}_N${N}_P${PROCS}.hsk"
    echo "[ms_gen_input] gerando $HSK (variant=$variant N=$N P=$PROCS vec=$VEC)"
    python3 "$SCRIPTDIR/_ms_gen_input.py" --n "$N" --P "$PROCS" --variant "$variant" --vec "$VEC" --out "$HSK"
    echo "[ms_gen_input] wrote $HSK"

    # 2) Constrói .fl a partir do .hsk (não mudo o que já está OK)
    FL="$OUTDIR/mergesort_${variant}_N${N}_P${PROCS}.fl"
    echo "[build_fl] GEN=$CODEGEN"
    echo "[build_fl] SUPERSGEN='(não setado)'"
    echo "[build_fl] HSK=$HSK"
    echo "[build_fl] OUT=$FL"
    "$SCRIPTDIR/_build_fl_from_hsk.sh" "$CODEGEN" "$HSK" "$FL"
    echo "[build_fl] OK -> $FL"

    # 3) Monta + balanceia PLA
    bash "$SCRIPTDIR/_assemble_fl.sh" --asm-root "$ASM_ROOT" --fl "$FL" --outdir "$OUTDIR" -P "$PROCS"

    # 4) Run (com strace se disponível)
    for (( rep=1; rep<=REPS; rep++ )); do
      REP_DIR="$OUTDIR/rep_${rep}"
      mkdir -p "$REP_DIR"
      python3 "$SCRIPTDIR/_run_interp.py" \
        --interp "$INTERP" \
        --flb "$OUTDIR/mergesort_${variant}_N${N}_P${PROCS}.flb" \
        --pla "$OUTDIR/mergesort_${variant}_N${N}_P${PROCS}.pla" \
        --threads "$PROCS" \
        --variant "$variant" \
        --N "$N" \
        --outdir "$REP_DIR" \
        --strace-bin "${STRACE_BIN:-}"
    done
  done
done
