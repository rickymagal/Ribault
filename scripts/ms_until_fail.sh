#!/usr/bin/env bash
set -euo pipefail

# ------------------------
# Parâmetros (podem ser sobrescritos por flags)
# ------------------------
START_N=${START_N:-750}
FACTOR=${FACTOR:-1.2}
REPS=${REPS:-10}
PROCS_DEF="1,2,4,8"
IFS=',' read -r -a PROCS <<< "${PROCS:-$PROCS_DEF}"
VARIANTS_DEF="asm,super"
IFS=',' read -r -a VARIANTS <<< "${VARIANTS:-$VARIANTS_DEF}"

INTERP="${INTERP:-/home/ricky/Área de trabalho/TALM/interp/interp}"
BASE="results/ms/growth_until_fail"

PY=python3
BUILD="./scripts/_build_fl_from_hsk.sh"
ASMBL="./scripts/_assemble_fl.sh"
RUNPY="./scripts/_run_interp.py"
PLOT="./scripts/plot_ms_until_fail.py"

# ------------------------
# CLI
# ------------------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --factor)  FACTOR="$2";  shift 2;;
    --reps)    REPS="$2";    shift 2;;
    --procs)   IFS=',' read -r -a PROCS <<< "$2"; shift 2;;
    --variants)IFS=',' read -r -a VARIANTS <<< "$2"; shift 2;;
    --interp)  INTERP="$2";  shift 2;;
    --base)    BASE="$2";    shift 2;;
    *) echo "arg desconhecido: $1"; exit 2;;
  esac
done

# ------------------------
# Utils
# ------------------------
ceil_mul () { awk -v n="$1" -v f="$2" 'BEGIN{v=n*f; printf("%d",(v==int(v))?v:int(v)+1)}'; }

find_superlib () {
  # tenta localizar libsupers.so dentro do diretório do N (pode ser por-P)
  local root="$1" n="$2" p="$3"
  local -a cands=(
    "$root/libsupers.so"
    "$root/supers/libsupers.so"
    "$root/supers/N_${n}_p${p}/libsupers.so"
    "$root/N_${n}_p${p}.supers/libsupers.so"
    "$root/supers/21_merge_sort_super/libsupers.so"
  )
  local x
  for x in "${cands[@]}"; do
    [[ -f "$x" ]] && { echo "$x"; return; }
  done
  # busca ampla (mais profunda) mas limitada
  local found
  found="$(find "$root" -maxdepth 10 -type f -name 'libsupers.so' 2>/dev/null | head -n1 || true)"
  [[ -n "$found" ]] && echo "$found" || echo ""
}

read_rc () {
  $PY - <<PY
import json
print(json.load(open("$1","r"))["rc"])
PY
}

# ------------------------
# Loop até falhar (qualquer variante, qualquer P, qualquer rep)
# ------------------------
LAST_GOOD_N=0
N="$START_N"

while : ; do
  N_INT=$(printf "%d" "$N")

  # build/assemble para todos P/VAR
  for VAR in "${VARIANTS[@]}"; do
    for P in "${PROCS[@]}"; do
      ROOT="${BASE}/${VAR}/N_${N_INT}"
      HSK="${ROOT}/N_${N_INT}_p${P}.hsk"
      FL="${ROOT}/N_${N_INT}_p${P}.fl"

      "$BUILD" "$HSK" "$FL" --variant "$VAR" --threads "$P" --force
      "$ASMBL" "$FL" "$ROOT" --threads "$P"
    done
  done

  FAILED=0
  for VAR in "${VARIANTS[@]}"; do
    for P in "${PROCS[@]}"; do
      for ((rep=1; rep<=REPS; rep++)); do
        ROOT="${BASE}/${VAR}/N_${N_INT}"
        OUTDIR="${ROOT}/p_${P}/rep_${rep}"
        FLB="${ROOT}/N_${N_INT}_p${P}.flb"
        PLA="${ROOT}/N_${N_INT}_p${P}.pla"

        if [[ "$VAR" == "super" ]]; then
          SL="$(find_superlib "$ROOT" "$N_INT" "$P")"
          $PY "$RUNPY" \
            --interp "$INTERP" \
            --flb "$FLB" \
            --pla "$PLA" \
            --threads "$P" \
            --variant "$VAR" \
            ${SL:+--superlib "$SL"} \
            --outdir "$OUTDIR" || true
        else
          $PY "$RUNPY" \
            --interp "$INTERP" \
            --flb "$FLB" \
            --pla "$PLA" \
            --threads "$P" \
            --variant "$VAR" \
            --outdir "$OUTDIR" || true
        fi

        RC=$(read_rc "$OUTDIR/result.json")
        if [[ "$RC" -ne 0 ]]; then
          FAILED=1
          break
        fi
      done
      [[ "$FAILED" -eq 1 ]] && break
    done
    [[ "$FAILED" -eq 1 ]] && break
  done

  if [[ "$FAILED" -eq 1 ]]; then
    if [[ "$LAST_GOOD_N" -eq 0 ]]; then
      echo "[until-fail] falhou já no primeiro N=${N_INT}; nada para plotar."
      exit 0
    fi
    echo "[until-fail] falhou em N=${N_INT}. Plotando até N=${LAST_GOOD_N}…"
    $PY "$PLOT" --base "$BASE" --maxN "$LAST_GOOD_N" --out "results/ms/plots_until_fail"
    exit 0
  fi

  LAST_GOOD_N="$N_INT"
  N="$(ceil_mul "$N_INT" "$FACTOR")"
done
