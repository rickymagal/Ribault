#!/usr/bin/env bash
set -euo pipefail

# Uso: _assemble_fl.sh input.fl out_dir
if [[ $# -ne 2 ]]; then
  echo "uso: $0 input.fl out_dir" >&2
  exit 2
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
INFL="$(readlink -f "$1")"
OUTDIR="$(readlink -f "$2")"
mkdir -p "$OUTDIR"

# TALM é irmão do HTC
TALM_DIR="$(readlink -f "$ROOT/../TALM")"
ASM_DIR="$TALM_DIR/asm"
if [[ ! -d "$ASM_DIR" ]]; then
  echo "ERRO: não encontrei TALM/asm em: $ASM_DIR" >&2
  exit 3
fi

BASENAME="$(basename "$INFL")"
NAME="${BASENAME%.fl}"

if ! command -v python2 >/dev/null 2>&1; then
  echo "ERRO: preciso de python2 para rodar o assembler legado do TALM." >&2
  echo "Instale python2 ou ajuste o wrapper para outra estratégia." >&2
  exit 5
fi

echo "[assemble] usando python2 em $ASM_DIR"
(
  cd "$ASM_DIR"
  # o assembler aceita: python2 assembler.py -o <BASENAME SEM EXT> <INPUT.fl>
  python2 assembler.py -o "$OUTDIR/$NAME" "$INFL"
)

# Confere/copias de segurança (se o assembler largar no CWD)
for ext in flb pla; do
  [[ -f "$OUTDIR/$NAME.$ext" ]] && continue
  [[ -f "$ASM_DIR/$NAME.$ext" ]] && cp -f "$ASM_DIR/$NAME.$ext" "$OUTDIR/" && continue
  [[ -f "$(dirname "$INFL")/$NAME.$ext" ]] && cp -f "$(dirname "$INFL")/$NAME.$ext" "$OUTDIR/" && continue
done

if [[ ! -f "$OUTDIR/$NAME.flb" || ! -f "$OUTDIR/$NAME.pla" ]]; then
  echo "ERRO: não encontrei $NAME.flb/.pla após montar." >&2
  exit 4
fi

echo "[assemble] gerados: $OUTDIR/$NAME.flb  e  $OUTDIR/$NAME.pla"
