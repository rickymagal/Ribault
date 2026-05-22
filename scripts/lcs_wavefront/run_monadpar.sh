#!/usr/bin/env bash
set -euo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

# ============================================================
# LCS Wavefront — monad-par sweep (Control.Monad.Par)
#
# Same dataflow shape as TALM (DIM_ROWS x DIM_COLS rectangular
# grid with auto-DIM_COLS), same compute backend as the other
# Haskell variants (raw pointers via Foreign.Marshal.Alloc).
# IVar-per-block sync replaces TALM's firing rule — pure
# data-driven scheduling, no diagonal barriers.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront_monadpar}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_MP="$REPO/scripts/lcs_wavefront/gen_hs_monadpar.py"

REPS=${REPS:-7}
SEED=${SEED:-42}
ALPHABET=${ALPHABET:-4}
NS=(${NS:-200000})
PS=(${PS:-1 2 4 8 16 24 32 40 48})
DIM_COLS=${DIM_COLS:-auto}
TARGET_BLOCK_WIDTH=${TARGET_BLOCK_WIDTH:-1000}
TALM_RTS_A=${TALM_RTS_A:-256m}

N_PHYS_CORES=${N_PHYS_CORES:-24}
N_LOG_CORES=${N_LOG_CORES:-48}
GHC_BIN="${GHC:-ghc}"

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"; else echo "0-$((p - 1))"; fi
}
pin_tag() {
  local p="$1"
  if (( p <= N_PHYS_CORES )); then
    echo "physical, cores 0-$((p-1)), no HT"
  else
    local ht=$(( p - N_PHYS_CORES ))
    echo "oversubscribed, cores 0-$((p-1)) = $N_PHYS_CORES phys + $ht HT siblings"
  fi
}
OS_BANNER_SHOWN=0

validate() {
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found in $file"; cat "$file" 2>/dev/null || true; exit 1
  fi
  if [[ "$expected" == "SKIP" ]]; then
    if [[ -z "${CROSS_EXPECTED:-}" ]]; then
      CROSS_EXPECTED="$got"; echo "  OK: RESULT=$got (first result, will cross-validate)"
    elif [[ "$got" != "$CROSS_EXPECTED" ]]; then
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED"; exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    [[ "$got" == "$expected" ]] || { echo "FATAL: got $got, expected $expected"; exit 1; }
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "================================================================"
echo " LCS Wavefront — monad-par sweep"
echo "================================================================"
echo "   NS=${NS[*]}  DIM_ROWS=P  DIM_COLS=$DIM_COLS  ALPHABET=$ALPHABET"
echo "   reps=$REPS  seed=$SEED"
echo " P sweep: ${PS[*]}"
echo "================================================================"

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# SEQ_LEN=$N"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  CROSS_EXPECTED=""

  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --N "$N" --alphabet "$ALPHABET" --seed "$SEED" --out-dir "$INPUT_DIR"
  EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
  echo "  Expected LCS score: $EXPECTED"

  for P in "${PS[@]}"; do
    if [[ "$P" -gt "$N_PHYS_CORES" && "$OS_BANNER_SHOWN" -eq 0 ]]; then
      echo ""
      echo "  ################################################################"
      echo "  ##  OVERSUBSCRIPTION BEGINS HERE                              ##"
      echo "  ################################################################"
      OS_BANNER_SHOWN=1
    fi

    CUR_ROWS=$P
    if [[ "$DIM_COLS" == "auto" ]]; then
      CUR_COLS=$(( N / TARGET_BLOCK_WIDTH ))
      if (( CUR_COLS < P )); then CUR_COLS=$P; fi
    else
      CUR_COLS=$DIM_COLS
    fi
    TOTAL_BLOCKS=$((CUR_ROWS * CUR_COLS))
    CORES_P="$(pin_cores "$P")"
    PIN_TAG="$(pin_tag "$P")"
    echo ""
    echo "======== SEQ_LEN=$N  P=$P  grid=${CUR_ROWS}x${CUR_COLS} (${TOTAL_BLOCKS} blocks; cores: $CORES_P; $PIN_TAG) ========"

    BDIR="$NDIR/monadpar_P${P}"
    mkdir -p "$BDIR/obj"
    "$PY3" "$GEN_MP" --out "$BDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"
    # Use the monadpar-specific GHC environment (with monad-par package
    # exposed). The default env is shared with all other Haskell variants
    # and does NOT expose monad-par, so non-monad-par builds don't get
    # bloated by monad-par's dynamic-only deps.
    GHC_ENVIRONMENT=monadpar "$GHC_BIN" -O2 -threaded -rtsopts -dynamic \
        -package time -package monad-par -package containers -package vector \
        -outputdir "$BDIR/obj" -o "$BDIR/lcs_wf" "$BDIR/lcs_wf.hs" >/dev/null 2>&1

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$BDIR/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$BDIR/lcs_wf" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "MONAD-PAR N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "MONAD-PAR N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "monadpar,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "================================================================"
echo " MONAD-PAR SWEEP COMPLETED"
echo " CSV: $CSV"
echo "================================================================"
