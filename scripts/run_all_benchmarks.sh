#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# run_all_benchmarks.sh — Roda a suite completa de benchmarks:
#   1) Merge Sort  (TALM vs GHC)
#   2) Caminho de Dyck (TALM vs GHC)
# e gera os gráficos de comparação.
#
# Uso:
#   bash scripts/run_all_benchmarks.sh [opções]
#
# Opções (todas opcionais — defaults reproduzem os resultados do paper):
#   --start-N   N       Primeiro tamanho de entrada  (default: 50000)
#   --step      S       Passo entre tamanhos          (default: 50000)
#   --n-max     M       Último tamanho de entrada     (default: 1000000)
#   --reps      R       Repetições por configuração   (default: 10)
#   --procs     P       Cores, separados por vírgula  (default: "1,2,4,8")
#   --outroot   DIR     Diretório base para resultados (default: RESULTS)
#   --skip-build        Não recompila o projeto
#   --only-ms           Só roda Merge Sort
#   --only-dyck         Só roda Dyck
#
# Requisitos:
#   ghc (8.10.7+), gcc, python3, make, alex, happy
#   pip3 install matplotlib numpy pandas
# ============================================================

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# ── Defaults ────────────────────────────────────────────────
START_N=50000
STEP=50000
N_MAX=1000000
REPS=10
PROCS_CSV="1,2,4,8"
OUTROOT="$ROOT/RESULTS"
SKIP_BUILD=0
ONLY_MS=0
ONLY_DYCK=0

PY2="${PY2:-python3}"
PY3="${PY3:-python3}"

# ── Parse args ──────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N)    START_N="$2"; shift 2;;
    --step)       STEP="$2"; shift 2;;
    --n-max)      N_MAX="$2"; shift 2;;
    --reps)       REPS="$2"; shift 2;;
    --procs)      PROCS_CSV="$2"; shift 2;;
    --outroot)    OUTROOT="$2"; shift 2;;
    --skip-build) SKIP_BUILD=1; shift;;
    --only-ms)    ONLY_MS=1; shift;;
    --only-dyck)  ONLY_DYCK=1; shift;;
    -h|--help)
      sed -n '2,/^# ====/{ /^# ====/d; s/^# \?//; p }' "$0"
      exit 0;;
    *) echo "[ERRO] flag desconhecida: $1"; exit 2;;
  esac
done

# ── Construir N_CSV a partir de start/step/max ──────────────
N_CSV=""
for (( n=START_N; n<=N_MAX; n+=STEP )); do
  [[ -n "$N_CSV" ]] && N_CSV="${N_CSV},"
  N_CSV="${N_CSV}${n}"
done
echo "=== N values: $N_CSV ==="

# ── Checar dependências ─────────────────────────────────────
check_cmd(){ command -v "$1" >/dev/null 2>&1 || { echo "[ERRO] '$1' não encontrado. Instale-o antes de continuar."; exit 1; }; }
check_cmd ghc
check_cmd gcc
check_cmd python3
check_cmd make

echo "=== Dependências OK ==="

# ── Build ───────────────────────────────────────────────────
if [[ "$SKIP_BUILD" -eq 0 ]]; then
  echo ""
  echo "========================================================"
  echo "  COMPILANDO O PROJETO"
  echo "========================================================"
  make -C "$ROOT"
  make -C "$ROOT/TALM/interp" clean
  make -C "$ROOT/TALM/interp"
  echo "=== Build OK ==="
fi

# Verificar binários
for bin in "$ROOT/codegen" "$ROOT/supersgen" "$ROOT/TALM/interp/interp"; do
  [[ -x "$bin" ]] || { echo "[ERRO] binário não encontrado: $bin"; exit 1; }
done

INTERP="$ROOT/TALM/interp/interp"
ASM_ROOT="$ROOT/TALM/asm"
CODEGEN_ROOT="$ROOT"

MS_OUT="$OUTROOT/mergesort"
MS_GHC_OUT="$OUTROOT/mergesort_ghc"
DYCK_OUT="$OUTROOT/dyck_N_IMB_sweep"

elapsed(){
  local t0="$1" t1="$2"
  local dt=$(( t1 - t0 ))
  printf "%dm%02ds" $(( dt / 60 )) $(( dt % 60 ))
}

# ============================================================
#  MERGE SORT
# ============================================================
run_merge_sort(){
  echo ""
  echo "========================================================"
  echo "  MERGE SORT — TALM (array supers)"
  echo "========================================================"
  local t0; t0=$(date +%s)

  PY2="$PY2" PY3="$PY3" \
  MS_LEAF=array \
  DF_LIST_BUILTIN=1 \
  SUPERS_FORCE_PAR=1 \
  bash "$ROOT/scripts/merge_sort_TALM_vs_Haskell/run.sh" \
    --start-N "$START_N" \
    --step "$STEP" \
    --n-max "$N_MAX" \
    --reps "$REPS" \
    --procs "$PROCS_CSV" \
    --interp "$INTERP" \
    --asm-root "$ASM_ROOT" \
    --codegen "$CODEGEN_ROOT" \
    --outroot "$MS_OUT" \
    --vec range \
    --plots yes \
    --tag ms_super

  local t1; t1=$(date +%s)
  echo "=== TALM Merge Sort concluído em $(elapsed "$t0" "$t1") ==="

  echo ""
  echo "========================================================"
  echo "  MERGE SORT — GHC baseline"
  echo "========================================================"
  t0=$(date +%s)

  GHC_PKGS="-package time -package deepseq -package parallel" \
  bash "$ROOT/scripts/merge_sort_TALM_vs_Haskell/run_hs.sh" \
    --start-N "$START_N" \
    --step "$STEP" \
    --n-max "$N_MAX" \
    --reps "$REPS" \
    --procs "$PROCS_CSV" \
    --outroot "$MS_GHC_OUT" \
    --vec range \
    --tag ghc_ms

  t1=$(date +%s)
  echo "=== GHC Merge Sort concluído em $(elapsed "$t0" "$t1") ==="

  echo ""
  echo "========================================================"
  echo "  MERGE SORT — Gráfico de comparação"
  echo "========================================================"
  "$PY3" "$ROOT/scripts/merge_sort_TALM_vs_Haskell/compare_best.py" \
    --agg-super "$MS_OUT/metrics_aggregated_ms_super.csv" \
    --agg-ghc "$MS_GHC_OUT/metrics_aggregated_ghc_ms.csv" \
    --outdir "$MS_OUT" \
    --tag ms_compare

  echo "=== Gráfico: $MS_OUT/compare_best_ms_compare.png ==="
}

# ============================================================
#  DYCK PATH
# ============================================================
run_dyck(){
  echo ""
  echo "========================================================"
  echo "  CAMINHO DE DYCK — TALM + GHC + plots"
  echo "========================================================"
  local t0; t0=$(date +%s)

  PY2="$PY2" PY3="$PY3" \
  bash "$ROOT/scripts/dyck/run_compare.sh" \
    --N "$N_CSV" \
    --reps "$REPS" \
    --procs "$PROCS_CSV" \
    --imb "0,25,50,75,100" \
    --delta "0" \
    --interp "$INTERP" \
    --asm-root "$ASM_ROOT" \
    --codegen "$CODEGEN_ROOT" \
    --outroot "$DYCK_OUT" \
    --tag dyck

  local t1; t1=$(date +%s)
  echo "=== Dyck TALM+GHC concluído em $(elapsed "$t0" "$t1") ==="

  echo ""
  echo "========================================================"
  echo "  CAMINHO DE DYCK — Gráfico de comparação"
  echo "========================================================"
  "$PY3" "$ROOT/scripts/dyck/compare_best.py" \
    --metrics-super "$DYCK_OUT/metrics_dyck_super.csv" \
    --metrics-hs "$DYCK_OUT/metrics_dyck_ghc.csv" \
    --outdir "$DYCK_OUT" \
    --tag dyck

  echo "=== Gráfico: $DYCK_OUT/compare_best_dyck_*.png ==="
}

# ============================================================
#  MAIN
# ============================================================
TOTAL_T0=$(date +%s)

echo ""
echo "╔══════════════════════════════════════════════════════════╗"
echo "║         BENCHMARK SUITE: Ribault (TALM) vs GHC         ║"
echo "╠══════════════════════════════════════════════════════════╣"
echo "║  N       : $START_N .. $N_MAX (step $STEP)"
echo "║  Reps    : $REPS"
echo "║  Procs   : $PROCS_CSV"
echo "║  Outroot : $OUTROOT"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

if [[ "$ONLY_DYCK" -eq 0 ]]; then
  run_merge_sort
fi

if [[ "$ONLY_MS" -eq 0 ]]; then
  run_dyck
fi

TOTAL_T1=$(date +%s)

echo ""
echo "╔══════════════════════════════════════════════════════════╗"
echo "║                    SUITE COMPLETA                       ║"
echo "╠══════════════════════════════════════════════════════════╣"
if [[ "$ONLY_DYCK" -eq 0 ]]; then
echo "║  Merge Sort TALM : $MS_OUT/metrics_aggregated_ms_super.csv"
echo "║  Merge Sort GHC  : $MS_GHC_OUT/metrics_aggregated_ghc_ms.csv"
echo "║  Merge Sort plot : $MS_OUT/compare_best_ms_compare.png"
fi
if [[ "$ONLY_MS" -eq 0 ]]; then
echo "║  Dyck TALM       : $DYCK_OUT/metrics_dyck_super.csv"
echo "║  Dyck GHC        : $DYCK_OUT/metrics_dyck_ghc.csv"
echo "║  Dyck plots      : $DYCK_OUT/compare_best_dyck_*.png"
fi
echo "╠══════════════════════════════════════════════════════════╣"
echo "║  Tempo total     : $(elapsed "$TOTAL_T0" "$TOTAL_T1")"
echo "╚══════════════════════════════════════════════════════════╝"
