#!/usr/bin/env bash
set -euo pipefail

ASM_ROOT=""
FL=""
OUTBASE=""
P="1"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --fl)       FL="$2";       shift 2;;
    --outbase)  OUTBASE="$2";  shift 2;;
    --P)        P="$2";        shift 2;;
    *) echo "[assemble] unknown arg: $1"; exit 2;;
  esac
done

[[ -n "$ASM_ROOT" && -d "$ASM_ROOT" ]] || { echo "[assemble] erro: --asm-root inválido"; exit 2; }
[[ -n "$FL" && -f "$FL" ]] || { echo "[assemble] erro: --fl inválido"; exit 2; }
[[ -n "$OUTBASE" ]] || { echo "[assemble] erro: --outbase faltou"; exit 2; }

FLB="${OUTBASE}.flb"
PLA="${OUTBASE}.pla"
mkdir -p "$(dirname "$OUTBASE")"

PY="$(command -v python2 || command -v python)"

echo "[assemble] FL=$FL"
# 1) assemble → .flb
"$PY" -u "${ASM_ROOT}/assembler.py" "$FL" -o "$OUTBASE"
[[ -s "$FLB" ]] || { echo "error: assembler não produziu $FLB"; exit 1; }

# 2) schedule (Round-Robin): muitas versões imprimem no stdout.
# Se sua versão aceitar -o, beleza; se não, redirecionamos.
set +e
"$PY" -u "${ASM_ROOT}/scheduler.py" "$FLB" -o "$PLA"
RC=$?
set -e
if [[ $RC -ne 0 || ! -s "$PLA" ]]; then
  # fallback: stdout → PLA
  "$PY" -u "${ASM_ROOT}/scheduler.py" "$FLB" > "$PLA"
fi

[[ -s "$PLA" ]] || { echo "error: scheduler não produziu $PLA"; exit 1; }
