#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   _assemble_fl.sh --asm-root <TALM/asm> --fl <in.fl> --outbase <out_base> --P <procs>
#
# Gera:
#   <out_base>.flb
#   <out_base>.pla  (com distribuição RR forçada entre os P threads)
#
# Logs:
#   <out_base>.assemble.log
#   <out_base>.schedule.log

ASM_ROOT=""
FL_IN=""
OUTBASE=""
PROCS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --fl)       FL_IN="$2";   shift 2;;
    --outbase)  OUTBASE="$2"; shift 2;;
    --P)        PROCS="$2";   shift 2;;
    *) echo "[assemble] unknown arg: $1"; exit 2;;
  esac
done

[[ -n "$ASM_ROOT" && -n "$FL_IN" && -n "$OUTBASE" && -n "$PROCS" ]] || {
  echo "usage: $0 --asm-root <TALM/asm> --fl <in.fl> --outbase <out_base> --P <procs>"
  exit 2
}

FLB="${OUTBASE}.flb"
PLA="${OUTBASE}.pla"
ASM_LOG="${OUTBASE}.assemble.log"
SCH_LOG="${OUTBASE}.schedule.log"

echo "[assemble] FL=${FL_IN}"

# 1) Assembler (python2): gera .flb
#    (sem -i; o assembler aceita '-o <base>' e '<in.fl>' como últimos args)
{
  python2 "${ASM_ROOT}/assembler.py" -o "${OUTBASE}" "${FL_IN}"
} > "${ASM_LOG}" 2>&1 || {
  echo "error: assembler falhou (veja ${ASM_LOG})"
  exit 1
}

[[ -s "${FLB}" ]] || { echo "error: não gerou ${FLB}"; exit 1; }

# 2) Scheduler (python2): gera .pla round-robin
#    Primeiro tentamos com '-o'; se não aparecer, tentamos redirecionando stdout.
set +e
python2 "${ASM_ROOT}/scheduler.py" "${FLB}" -p "${PROCS}" --rr -o "${PLA}" > "${SCH_LOG}" 2>&1
rc=$?
set -e

if [[ $rc -ne 0 || ! -s "${PLA}" ]]; then
  # fallback: alguns schedulers só escrevem em stdout
  set +e
  python2 "${ASM_ROOT}/scheduler.py" "${FLB}" -p "${PROCS}" --rr > "${PLA}" 2>> "${SCH_LOG}"
  rc2=$?
  set -e
  if [[ $rc2 -ne 0 || ! -s "${PLA}" ]]; then
    echo "error: scheduler não produziu ${PLA} (veja ${SCH_LOG})"
    exit 1
  fi
fi

# 3) Garantia de balanceamento: reescreve o .pla em RR puro
#    Formato: 1ª linha = NINST; linhas seguintes = id do thread por instrução.
python3 - "$PLA" "$PROCS" << 'PY'
import sys
pla_path = sys.argv[1]
P = int(sys.argv[2])

with open(pla_path, 'r') as f:
    lines = [ln.strip() for ln in f if ln.strip()!='']

try:
    n = int(lines[0])
    mapping = [int(x) for x in lines[1:]]
except Exception:
    # se o formato do PLA for binário/estranho, não mexe
    sys.exit(0)

# se já está balanceado e usa mais de 1 thread, mantemos;
# se está degenerado (<=1 thread usado) OU desbalanceado, forçamos RR.
used = set(mapping)
need_fix = (len(used) <= 1) or (len(mapping) != n)

if need_fix:
    newmap = [(i % P) for i in range(n)]
    with open(pla_path, 'w') as g:
        g.write(str(n) + "\n")
        g.write("\n".join(map(str, newmap)) + "\n")
PY

echo "[assemble] OK: ${FLB} + ${PLA}"
