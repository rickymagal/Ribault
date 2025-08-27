#!/usr/bin/env bash
set -euo pipefail

VARIANT="asm"
CODEGEN=""
SUPERSGEN=""
IN=""
OUT=""
LOG="/dev/null"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --variant)   VARIANT="$2";    shift 2;;
    --codegen)   CODEGEN="$2";    shift 2;;
    --supersgen) SUPERSGEN="$2";  shift 2;;  # aceito mas não obrigatório
    --in)        IN="$2";         shift 2;;
    --out)       OUT="$2";        shift 2;;
    --log)       LOG="$2";        shift 2;;
    *) echo "[build_fl] unknown arg: $1"; exit 2;;
  esac
done

[[ -n "$CODEGEN" ]] || { echo "[build_fl] erro: faltou --codegen"; exit 2; }
[[ -x "$CODEGEN" ]] || { echo "[build_fl] erro: $CODEGEN não é executável"; exit 2; }
[[ -n "$IN" && -f "$IN" ]] || { echo "[build_fl] erro: faltou --in <hsk>"; exit 2; }
[[ -n "$OUT" ]] || { echo "[build_fl] erro: faltou --out <fl>"; exit 2; }

mkdir -p "$(dirname "$OUT")"
echo "[codegen] $IN -> $OUT (variant=$VARIANT)" | tee -a "$LOG"
# O codegen gera TALM .fl de ambos (asm e super)
# Saída no stdout → redireciona para OUT; stderr → LOG
"$CODEGEN" "$IN" > "$OUT" 2>>"$LOG"
[[ -s "$OUT" ]] || { echo "[build_fl] erro: codegen não produziu $OUT"; exit 1; }
