#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"

if [[ $# -lt 2 ]]; then
  echo "uso: $0 input.hsk output.fl" >&2
  exit 2
fi

HSK="$1"
FLOUT="$2"

# 1) preferir o binário já existente
if [[ -x "$ROOT/codegen" ]]; then
  echo "[build_fl] usando binário $ROOT/codegen"
  "$ROOT/codegen" "$HSK" > "$FLOUT"
  exit 0
fi

# 2) fallback: compilar com GHC incluindo os diretórios corretos
echo "[build_fl] compilando codegen (fallback)"
# diretórios de saída (seguem o layout do seu repo)
mkdir -p "$ROOT/codegen.obj" "$ROOT/codegen.hi"

ghc \
  -isrc -isrc/Analysis -isrc/Synthesis \
  -odir "$ROOT/codegen.obj" -hidir "$ROOT/codegen.hi" \
  "$ROOT/src/Synthesis/MainCode.hs" \
  -o "$ROOT/codegen"

echo "[build_fl] rodando codegen recém-compilado"
"$ROOT/codegen" "$HSK" > "$FLOUT"
