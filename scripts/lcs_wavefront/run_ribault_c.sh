#!/usr/bin/env bash
set -euo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

# ============================================================
# LCS Wavefront — Ribault + C super sweep
#
# Same Trebuchet runtime as TALM; same dataflow graph (.fl)
# as TALM auto-DIM_COLS; but the leaf compute (lcsBlock) is
# pure C compiled into libsupers.so — no GHC/Haskell. This
# isolates the Trebuchet scheduler from the leaf-backend
# language (Haskell-with-pointers vs Rust vs C).
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront_ribault_c}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_C="$REPO/scripts/lcs_wavefront/gen_lcs_c.py"
BUILD_C="$REPO/tools/build_supers_c.sh"

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
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED (cross-validate)"; exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    if [[ "$got" != "$expected" ]]; then
      echo "FATAL: $label -> got $got, expected $expected"; exit 1
    fi
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "================================================================"
echo " LCS Wavefront — Ribault + C super sweep"
echo "================================================================"
echo "   NS=${NS[*]}  DIM_ROWS=P  DIM_COLS=$DIM_COLS  ALPHABET=$ALPHABET"
echo "   reps=$REPS  seed=$SEED"
echo ""
echo " Leaf backend: pure C (lcs_c_supers.c) — same algorithm as Haskell"
echo " Runtime: Trebuchet (same interp + asm as TALM/Haskell variant)"
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

    TDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$TDIR/supers"

    "$PY3" "$GEN_C" --out-dir "$TDIR" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"
    bash "$BUILD_C" "$TDIR/lcs_c_supers.c" "$TDIR/supers"
    LIBSUP="$TDIR/supers/libsupers.so"

    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/lcs_wf_P${P}" "$TDIR/lcs_wf.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/lcs_wf_P${P}.flb"
    PLA="$TDIR/lcs_wf_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/lcs_wf_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_DISABLE=1 \
          taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -eq 0 ]]; then
          got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
          if [[ -n "$got" ]]; then break; fi
        fi
        echo "  WARN: RIBAULT-C N=$N P=$P rep=$rep attempt=$attempt failed (rc=$rc)"
        sleep 1
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      [[ -z "$secs" ]] && secs="NaN"
      echo "RIBAULT-C N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "RIBAULT-C N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "ribault_c,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "================================================================"
echo " RIBAULT-C SWEEP COMPLETED"
echo " CSV: $CSV"
echo "================================================================"
