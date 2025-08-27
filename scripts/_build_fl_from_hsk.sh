#!/usr/bin/env bash
set -euo pipefail

# Uso:
#   _build_fl_from_hsk.sh <input.hsk> <output.fl> [--variant asm|super] [--threads P] [--force]
#
# Notas:
# - Se <input.hsk> não existir ou estiver vazio, ele será (re)gerado via _ms_gen_input.py
#   usando N e P inferidos do nome do arquivo: N_XXX_pYYY.hsk  (XXX = N, YYY = P).
# - --variant e --threads só controlam a geração do .hsk. O codegen recebe APENAS o .hsk.
# - O codegen é resolvido como "$REPO_ROOT/codegen" por padrão (pode sobrescrever via CODEGEN=...)

if [[ $# -lt 2 ]]; then
  echo "[build_fl] usage: $0 <input.hsk> <output.fl> [--variant asm|super] [--threads P] [--force]" >&2
  exit 2
fi

IN_HSK="$1"
OUT_FL="$2"
shift 2 || true

VARIANT="asm"
THREADS=""
FORCE=0

while (($#)); do
  case "$1" in
    --variant)
      VARIANT="$2"; shift 2;;
    --threads)
      THREADS="$2"; shift 2;;
    --force|--force-regen)
      FORCE=1; shift;;
    *)
      echo "[build_fl] ignoring unknown flag: $1" >&2
      shift;;
  esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CODEGEN="${CODEGEN:-"$REPO_ROOT/codegen"}"

if [[ ! -x "$CODEGEN" ]]; then
  echo "[build_fl] codegen not found or not executable: $CODEGEN" >&2
  exit 1
fi

mkdir -p "$(dirname "$IN_HSK")" "$(dirname "$OUT_FL")"

# Inferir N e P a partir do nome do .hsk (N_XXX_pYYY.hsk)
bn="$(basename "$IN_HSK")"
N_INFER=""
P_INFER=""
if [[ "$bn" =~ ^N_([0-9]+)_p([0-9]+)\.hsk$ ]]; then
  N_INFER="${BASH_REMATCH[1]}"
  P_INFER="${BASH_REMATCH[2]}"
fi

# Se --threads não veio e conseguimos inferir do nome, usamos o P do nome
if [[ -z "${THREADS}" && -n "${P_INFER}" ]]; then
  THREADS="$P_INFER"
fi
# Sanidade: threads padrão 1 se ainda vazio
THREADS="${THREADS:-1}"

# (Re)gerar .hsk se necessário
if [[ "$FORCE" -eq 1 || ! -s "$IN_HSK" ]]; then
  if [[ -z "$N_INFER" ]]; then
    echo "[build_fl] cannot infer N from '$bn' (expected N_<N>_p<P>.hsk). Use that name format." >&2
    exit 1
  fi
  echo "[build_fl] generating HSK (N=$N_INFER, variant=$VARIANT, threads=$THREADS)"
  "$SCRIPT_DIR/_ms_gen_input.py" \
    --n "$N_INFER" \
    --variant "$VARIANT" \
    --threads "$THREADS" \
    --out "$IN_HSK"
fi

echo "[build_fl] using codegen: $CODEGEN"
# IMPORTANTE: codegen recebe APENAS o .hsk; saída redirecionada para o .fl
"$CODEGEN" "$IN_HSK" > "$OUT_FL"
