#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   _assemble_fl.sh --fl <arquivo.fl> --outdir <dir> -P <threads> \
#                   --asm-root <dir> --py2 <python2> --assembler <assembler.py> [--autoplace yes|no]
#
# Saídas:
#   <outdir>/<basename>.flb e <outdir>/<basename>.pla (rebalanceado i%P)

FL=""
OUTDIR=""
THREADS=""
ASM_ROOT=""
PY2="python2"
ASSEMBLER=""
AUTOPLACE="no"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --fl)         FL="$2"; shift 2 ;;
    --outdir)     OUTDIR="$2"; shift 2 ;;
    -P|--threads) THREADS="$2"; shift 2 ;;
    --asm-root)   ASM_ROOT="${2:-}"; shift 2 ;;
    --py2)        PY2="$2"; shift 2 ;;
    --assembler)  ASSEMBLER="$2"; shift 2 ;;
    --autoplace)  AUTOPLACE="${2:-no}"; shift 2 ;;
    *) echo "[assemble][ERRO] argumento desconhecido: $1"; exit 2 ;;
  esac
done

[[ -n "${FL}" && -n "${OUTDIR}" ]] || { echo "[assemble][ERRO] --fl e --outdir são obrigatórios"; exit 2; }
THREADS="${THREADS:-1}"
ASSEMBLER="${ASSEMBLER:-${ASM_ROOT:+$ASM_ROOT/assembler.py}}"
[[ -n "${ASSEMBLER}" && -f "${ASSEMBLER}" ]] || { echo "[assemble][ERRO] assembler.py não encontrado: ${ASSEMBLER:-<vazio>}"; exit 2; }

mkdir -p "${OUTDIR}"

OUTBASENAME="$(basename "${FL%.fl}")"
OUTPREFIX="${OUTDIR}/${OUTBASENAME}"
FLB="${OUTPREFIX}.flb"
PLA="${OUTPREFIX}.pla"

echo "[assemble] FL=${FL}"
echo "[assemble] OUTDIR=${OUTDIR}"
echo "[assemble] P=${THREADS}"
echo "[assemble] ASM_ROOT=${ASM_ROOT}"
echo "[assemble] PY2=${PY2}"
echo "[assemble] ASSEMBLER=${ASSEMBLER}"

set -x
if [[ "${AUTOPLACE}" == "yes" ]]; then
  echo "[assemble] usando assembler.py (com -a, ntasks=-n ${THREADS})" >&2
  "${PY2}" "${ASSEMBLER}" -a -n "${THREADS}" -o "${OUTPREFIX}" "${FL}"
else
  echo "[assemble] usando assembler.py (sem -a, ntasks=-n ${THREADS})" >&2
  "${PY2}" "${ASSEMBLER}" -n "${THREADS}" -o "${OUTPREFIX}" "${FL}"
fi
set +x

# Rebalanceia o .pla ANTES do run (round-robin por índice)
python3 - "$PLA" "$THREADS" <<'PY'
import sys
pla_path = sys.argv[1]
P = int(sys.argv[2])

# Lê mapeamento existente (se houver)
lines = []
with open(pla_path, 'r') as f:
    for ln in f:
        ln = ln.strip()
        if ln:
            lines.append(ln)

# Header pode ser 'n' (qtd de tarefas) ou arquivo só com linhas de PE.
def parse_map(ls):
    # tenta header
    try:
        n = int(ls[0])
        vals = [int(x) for x in ls[1:1+n]]
        if len(vals) != n:
            raise ValueError
        return n, vals
    except Exception:
        # sem header: tudo são PEs
        vals = [int(x) for x in ls]
        return len(vals), vals

n, _old = parse_map(lines)

# round-robin por índice de tarefa
newmap = [i % P for i in range(n)]

# grava com header 'n' para eliminar ambiguidade
with open(pla_path, 'w') as f:
    f.write(str(n) + "\n")
    for p in newmap:
        f.write(str(p) + "\n")

# log de distribuição e "n_instrs" por thread (aqui = nº de tarefas atribuídas)
dist = [newmap.count(i) for i in range(P)]
print(f"[debug] pla: tarefas={n} P={P} dist={dist}")
for i,c in enumerate(dist):
    print(f"[debug] T[{i}]: n_instrs={c}")
PY

echo "[assemble] OK: ${FLB} + ${PLA}"
