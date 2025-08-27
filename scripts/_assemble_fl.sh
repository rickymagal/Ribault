#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   _assemble_fl.sh <input.fl> <outdir> [--threads P]
#
# Faz:
#   1) roda o montador Python2 do TALM (gera .flb e .pla)
#   2) se --threads P for passado, reescreve o .pla para range [0..P-1]

if [[ $# -lt 2 ]]; then
  echo "[assemble] usage: $0 <input.fl> <outdir> [--threads P]" >&2
  exit 2
fi

IN_FL="$1"
OUTDIR="$2"
shift 2 || true

THREADS=""
while (($#)); do
  case "$1" in
    --threads) THREADS="$2"; shift 2;;
    *) echo "[assemble] ignoring unknown flag: $1" >&2; shift;;
  esac
done

# Descobrir TALM/asm:
#  1) TALM_ASM_DIR (se setado)
#  2) irmão do repo:  <repo_root>/../TALM/asm
#  3) embutido:       <repo_root>/TALM/asm
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if [[ -n "${TALM_ASM_DIR:-}" ]]; then
  ASM_DIR="$TALM_ASM_DIR"
elif [[ -d "$REPO_ROOT/../TALM/asm" ]]; then
  ASM_DIR="$REPO_ROOT/../TALM/asm"
elif [[ -d "$REPO_ROOT/TALM/asm" ]]; then
  ASM_DIR="$REPO_ROOT/TALM/asm"
else
  echo "[assemble] ERRO: não encontrei TALM/asm.
  Sete TALM_ASM_DIR para o caminho absoluto (ex.: /home/<user>/.../TALM/asm)." >&2
  exit 1
fi

echo "[assemble] usando python2 em $ASM_DIR"
mkdir -p "$OUTDIR"

# Monta: gera <OUTDIR>/<basename>.flb e .pla
base="$(basename "$IN_FL" .fl)"
outbase="$OUTDIR/$base"
python2 "$ASM_DIR/assembler.py" "$IN_FL" -o "$outbase"

FLB="$outbase.flb"
PLA="$outbase.pla"

if [[ ! -s "$FLB" || ! -s "$PLA" ]]; then
  echo "[assemble] ERRO: não gerou $FLB ou $PLA" >&2
  exit 1
fi

echo "[assemble] gerados: $FLB  e  $PLA"

# Opcional: alinhar placement ao número de threads pedido
if [[ -n "${THREADS}" ]]; then
  python3 - "$PLA" "$THREADS" <<'PY'
import sys
pla_path = sys.argv[1]
P = int(sys.argv[2])
with open(pla_path, 'r') as f:
    lines = [ln.strip() for ln in f if ln.strip()]
n = int(lines[0]); placement = [int(x) for x in lines[1:1+n]]
fixed = [(p % P) for p in placement]
with open(pla_path, 'w') as f:
    f.write(str(n) + "\n")
    for p in fixed: f.write(str(p) + "\n")
PY
  echo "[assemble] PLA corrigido para P=$THREADS (round-robin)."
fi
