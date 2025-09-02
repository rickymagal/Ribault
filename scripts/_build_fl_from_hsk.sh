#!/usr/bin/env bash
set -euo pipefail

# Uso: _build_fl_from_hsk.sh <CODEGEN> <HSK> <OUT_FL>
# - <CODEGEN>: pode ser o diretório que contém o binário `codegen` OU o próprio binário
# - <HSK>: arquivo .hsk de entrada
# - <OUT_FL>: arquivo .fl de saída

GEN="${1:?}"
HSK="${2:?}"
OUT="${3:?}"

echo "[build_fl] GEN=${GEN}"
echo "[build_fl] SUPERSGEN='(não setado)'"
echo "[build_fl] HSK=${HSK}"
echo "[build_fl] OUT=${OUT}"

# Descobre o binário do codegen
CG=""
if [[ -x "${GEN}" && ! -d "${GEN}" ]]; then
  # GEN já é o binário (ex.: /home/.../Ribault/codegen ou /home/.../Ribault/lambdaflow-asm)
  CG="${GEN}"
elif [[ -x "${GEN%/}/codegen" ]]; then
  CG="${GEN%/}/codegen"
elif [[ -x "${GEN%/}/lambdaflow-asm" ]]; then
  CG="${GEN%/}/lambdaflow-asm"
else
  echo "[build_fl][ERRO] codegen não encontrado/executável: '${GEN}' (procurei por 'codegen' e 'lambdaflow-asm')" >&2
  exit 3
fi

# Garante diretório de saída
mkdir -p "$(dirname -- "${OUT}")"

# O codegen/lambdaflow-asm aceita **apenas** o arquivo de entrada.
# A saída .fl vai para stdout -> redirecionamos para ${OUT}.
set +e
"${CG}" "${HSK}" > "${OUT}"
rc=$?
set -e

if [[ ${rc} -ne 0 || ! -s "${OUT}" ]]; then
  echo "[build_fl][ERRO] falha no codegen (rc=${rc}); saída '${OUT}' vazia ou inexistente." >&2
  exit "${rc}"
fi

echo "[build_fl] OK -> ${OUT}"
