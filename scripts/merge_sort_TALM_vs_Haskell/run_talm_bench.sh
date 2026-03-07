#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# run_talm_bench.sh — TALM merge sort benchmark (supers-based)
#
# Uses gen_talm_input.py: only mergeSortT stays in pure DF;
# len, splitCount, merge, ms_super are all Haskell supers.
# ============================================================

START_N=50000000
STEP=50000000
N_MAX=1000000000
REPS=3
PROCS_CSV="1,2,4,8,16"
OUTROOT=""
TAG="talm_ms"

RIBAULT=""        # path to Ribault repo root
PY3="${PY3:-python3}"

usage(){
  cat <<EOF
Usage:
  $0 --ribault /path/to/Ribault --outroot /path/to/results \\
     [--start-N 50000000] [--step 50000000] [--n-max 1000000000] \\
     [--reps 3] [--procs "1,2,4,8,16"] [--tag name]
EOF
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --ribault)  RIBAULT="$2";   shift 2;;
    --outroot)  OUTROOT="$2";   shift 2;;
    --start-N)  START_N="$2";   shift 2;;
    --step)     STEP="$2";      shift 2;;
    --n-max)    N_MAX="$2";     shift 2;;
    --reps)     REPS="$2";      shift 2;;
    --procs)    PROCS_CSV="$2"; shift 2;;
    --tag)      TAG="$2";       shift 2;;
    *) usage;;
  esac
done

[[ -n "$RIBAULT" && -n "$OUTROOT" ]] || usage

CODEGEN="$RIBAULT/codegen"
ASM_ROOT="$RIBAULT/TALM/asm"
INTERP="$RIBAULT/TALM/interp/interp"
BUILD_SUPERS="$RIBAULT/tools/build_supers.sh"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_TALM="$SCRIPT_DIR/gen_talm_input.py"

[[ -x "$CODEGEN" ]]       || { echo "[ERR] codegen not found: $CODEGEN"; exit 1; }
[[ -x "$INTERP" ]]        || { echo "[ERR] interp not found: $INTERP"; exit 1; }
[[ -f "$GEN_TALM" ]]      || { echo "[ERR] gen_talm_input.py not found: $GEN_TALM"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" ]] || { echo "[ERR] assembler.py not found: $ASM_ROOT"; exit 1; }

echo "[cfg ] RIBAULT=$RIBAULT"
echo "[cfg ] OUTROOT=$OUTROOT"
echo "[cfg ] N range: $START_N to $N_MAX step $STEP"
echo "[cfg ] P values: $PROCS_CSV"
echo "[cfg ] REPS=$REPS"

mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,rep,seconds,result" > "$METRICS_CSV"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"

# Detect GHC shim for HsFFI include
SHIM_DIR="$RIBAULT/build/ghc-shim"
CFLAGS_SUPERS=""
if [[ -d "$SHIM_DIR/rts" ]]; then
  GHC_VER="$(ghc --numeric-version)"
  SHIM_RTS_SO="$(ls "$SHIM_DIR/rts/libHSrts"*"-ghc${GHC_VER}.so" 2>/dev/null | head -1 || true)"
  if [[ -n "$SHIM_RTS_SO" ]]; then
    export GHC_LIBDIR="$SHIM_DIR"
    export RTS_SO="$SHIM_RTS_SO"
    if [[ -f "$SHIM_DIR/.cpath" ]]; then
      export C_INCLUDE_PATH="$(cat "$SHIM_DIR/.cpath")"
      export CPATH="$(cat "$SHIM_DIR/.cpath")"
    fi
    echo "[sup ] detected shim: GHC_LIBDIR=$GHC_LIBDIR"
  fi
fi

# Fallback: find HsFFI include from ghc
if [[ -z "$CFLAGS_SUPERS" ]]; then
  GHC_LIBDIR_RAW="$(ghc --print-libdir 2>/dev/null || true)"
  if [[ -n "$GHC_LIBDIR_RAW" ]]; then
    RTS_INC="$(find "$GHC_LIBDIR_RAW" -name HsFFI.h -printf '%h\n' 2>/dev/null | head -1 || true)"
    if [[ -n "$RTS_INC" ]]; then
      CFLAGS_SUPERS="-O2 -fPIC -I$RTS_INC"
    fi
  fi
fi

# Temporarily move GHC env file if it exists (avoids package ambiguity during codegen)
GHC_ENV_FILE=""
for f in "$HOME/.ghc"/*/environments/default; do
  [[ -f "$f" ]] && GHC_ENV_FILE="$f" && break
done

move_ghc_env() {
  if [[ -n "$GHC_ENV_FILE" && -f "$GHC_ENV_FILE" ]]; then
    mv "$GHC_ENV_FILE" "${GHC_ENV_FILE}.bak"
  fi
}

restore_ghc_env() {
  if [[ -n "$GHC_ENV_FILE" && -f "${GHC_ENV_FILE}.bak" ]]; then
    mv "${GHC_ENV_FILE}.bak" "$GHC_ENV_FILE"
  fi
}

trap restore_ghc_env EXIT

# ============================================================
# Main loop
# ============================================================

for N in $(seq "$START_N" "$STEP" "$N_MAX"); do
  echo ""
  echo "================================================================"
  echo "  N = $N  ($(echo "scale=0; $N / 1000000" | bc)M)"
  echo "================================================================"

  N_DIR="$OUTROOT/N_${N}"
  INPUT_DIR="$N_DIR/input"
  SUPERS_DIR="$N_DIR/supers"
  mkdir -p "$INPUT_DIR" "$SUPERS_DIR"

  # Write params.txt
  echo "$N" > "$INPUT_DIR/params.txt"

  # Build supers ONCE per N (supers don't depend on P)
  SUPERS_HSK="$SUPERS_DIR/supers.hsk"
  if [[ ! -f "$SUPERS_DIR/libsupers.so" ]]; then
    echo "[sup ] building supers for N=$N ..."
    "$PY3" "$GEN_TALM" --out "$SUPERS_HSK" --input-dir "$INPUT_DIR" --P 2
    if [[ -n "$CFLAGS_SUPERS" ]]; then
      CFLAGS="$CFLAGS_SUPERS" "$BUILD_SUPERS" "$SUPERS_HSK" "$SUPERS_DIR/Supers.hs"
    else
      "$BUILD_SUPERS" "$SUPERS_HSK" "$SUPERS_DIR/Supers.hs"
    fi
    echo "[sup ] built: $SUPERS_DIR/libsupers.so"
  else
    echo "[sup ] reusing cached supers for N=$N"
  fi

  for P in "${PROCS[@]}"; do
    echo ""
    echo "-------- N=$N  P=$P --------"

    CASE_DIR="$N_DIR/P_${P}"
    LOGS_DIR="$CASE_DIR/logs"
    mkdir -p "$CASE_DIR" "$LOGS_DIR"

    HSK="$CASE_DIR/ms_N${N}_P${P}.hsk"
    FL="$CASE_DIR/ms_N${N}_P${P}.fl"
    PREFIX="$CASE_DIR/ms_N${N}_P${P}"

    # Generate .hsk
    "$PY3" "$GEN_TALM" --out "$HSK" --input-dir "$INPUT_DIR" --P "$P"

    # Codegen
    echo "[cg  ] codegen -> $FL"
    move_ghc_env
    "$CODEGEN" "$HSK" > "$FL" 2>"$LOGS_DIR/codegen.err" || {
      restore_ghc_env
      echo "[ERR ] codegen failed"; cat "$LOGS_DIR/codegen.err"; exit 1
    }
    restore_ghc_env

    # Assemble with autoplace
    echo "[asm ] assembling with autoplace -n $P"
    (cd "$ASM_ROOT" && "$PY3" assembler.py -n "$P" -a -o "$PREFIX" "$FL") \
      >"$LOGS_DIR/asm.out" 2>"$LOGS_DIR/asm.err" || {
      echo "[ERR ] assembler failed"; cat "$LOGS_DIR/asm.err"; exit 1
    }

    # Use autoplace PLA
    PLA="${PREFIX}_auto.pla"
    [[ -f "$PLA" ]] || PLA="${PREFIX}.pla"

    LIBSUP="$SUPERS_DIR/libsupers.so"
    GHCDEPS="$SUPERS_DIR/ghc-deps"

    for ((rep=1; rep<=REPS; rep++)); do
      echo -n "  rep $rep/$REPS ... "

      OUTLOG="$LOGS_DIR/run_rep${rep}.out"
      ERRLOG="$LOGS_DIR/run_rep${rep}.err"

      SUPERS_FORCE_PAR=1 \
      SUPERS_RTS_N="$P" \
      SUPERS_WORKER=0 \
      SUPERS_WORKER_MAIN=0 \
      NUM_CORES="$P" \
      LD_LIBRARY_PATH="$SUPERS_DIR:$GHCDEPS" \
        "$INTERP" "$P" "${PREFIX}.flb" "$PLA" "$LIBSUP" \
        >"$OUTLOG" 2>"$ERRLOG"

      # Extract time and result
      EXEC_T="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERRLOG" 2>/dev/null || echo "NaN")"
      RESULT="$(grep -oP 'RESULT=\K[0-9]+' "$OUTLOG" 2>/dev/null || echo "-1")"

      echo "time=${EXEC_T}s  RESULT=${RESULT}"

      if [[ "$RESULT" != "1" ]]; then
        echo "[ERR ] WRONG RESULT=$RESULT for N=$N P=$P rep=$rep"
        echo "[ERR ] stdout:"; cat "$OUTLOG"
        echo "[ERR ] stderr:"; cat "$ERRLOG"
        exit 1
      fi

      echo "talm,$N,$P,$rep,$EXEC_T,$RESULT" >> "$METRICS_CSV"
    done
  done
done

echo ""
echo "================================================================"
echo "[DONE] All results in: $METRICS_CSV"
echo "================================================================"
