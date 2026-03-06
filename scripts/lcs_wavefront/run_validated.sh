#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated LCS Wavefront Benchmark
# Sequential baseline + TALM + GHC-forkIO
#
# Rectangular grid strategy: DIM_ROWS=P × DIM_COLS (configurable).
# This gives large grain per block (N/P rows each) while
# maintaining enough anti-diagonals for full P-way parallelism.
#
# Sequential baseline uses DIM=1 (no blocking overhead).
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_SEQ="$REPO/scripts/lcs_wavefront/gen_hs_sequential.py"
GEN_TALM="$REPO/scripts/lcs_wavefront/gen_talm_input.py"
GEN_GHC="$REPO/scripts/lcs_wavefront/gen_hs_strategies.py"

REPS=${REPS:-3}
SEED=${SEED:-42}
ALPHABET=${ALPHABET:-4}
NS=(${NS:-1000 2000 3000})
PS=(${PS:-2 4 8})
DIM_COLS=${DIM_COLS:-16}
TALM_RTS_A=${TALM_RTS_A:-256m}

# Detect HsFFI include
GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir)"
SUPERS_CFLAGS="${CFLAGS:-}"
if [[ -z "$SUPERS_CFLAGS" ]]; then
  for cand in \
    "$GHC_LIBDIR/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/../lib/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/rts/include" \
    "$GHC_LIBDIR/include"; do
    if [[ -f "$cand/HsFFI.h" ]]; then
      SUPERS_CFLAGS="-O2 -fPIC -I$cand"
      break
    fi
  done
  [[ -z "$SUPERS_CFLAGS" ]] && SUPERS_CFLAGS="-O2 -fPIC"
fi

validate() {
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found in $file"
    cat "$file" 2>/dev/null || true
    exit 1
  fi
  if [[ "$expected" == "SKIP" ]]; then
    if [[ -z "${CROSS_EXPECTED:-}" ]]; then
      CROSS_EXPECTED="$got"
      echo "  OK: RESULT=$got (first result, will cross-validate)"
    elif [[ "$got" != "$CROSS_EXPECTED" ]]; then
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED (cross-validate)"
      exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    if [[ "$got" != "$expected" ]]; then
      echo "FATAL: $label -> got $got, expected $expected"
      exit 1
    fi
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "========================================="
echo " LCS Wavefront Benchmark (rectangular grid)"
echo " NS (seq_len)=${NS[*]}"
echo " DIM_ROWS=P  DIM_COLS=$DIM_COLS  ALPHABET=$ALPHABET"
echo " PS=${PS[*]}  reps=$REPS  seed=$SEED"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# SEQ_LEN=$N"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  CROSS_EXPECTED=""

  # ===== Generate input + expected =====
  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --N "$N" --alphabet "$ALPHABET" --seed "$SEED" \
      --out-dir "$INPUT_DIR"
  EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
  echo "  Expected LCS score: $EXPECTED"

  # ===== Build & run sequential baseline (DIM=1×1, no blocking) =====
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
      --dim-rows 1 --dim-cols 1
  "$GHC_BIN" -O2 -rtsopts -package time \
      -outputdir "$SDIR/obj" -o "$SDIR/lcs_wf" "$SDIR/lcs_wf.hs" >/dev/null 2>&1

  echo ""
  echo "======== SEQ_LEN=$N  SEQUENTIAL BASELINE (1x1) ========"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    "$SDIR/lcs_wf" >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "SEQ      N=$N rep=$rep -> ${secs}s"
    validate "SEQ N=$N rep=$rep" "$OUT" "$EXPECTED"
    echo "seq,$N,1,1,1,$rep,$secs" >> "$CSV"
  done

  # ===== Run parallel benchmarks (DIM_ROWS=P, DIM_COLS=$DIM_COLS) =====
  for P in "${PS[@]}"; do
    CUR_ROWS=$P
    CUR_COLS=$DIM_COLS
    TOTAL_BLOCKS=$((CUR_ROWS * CUR_COLS))
    echo ""
    echo "======== SEQ_LEN=$N  P=$P  grid=${CUR_ROWS}x${CUR_COLS} (${TOTAL_BLOCKS} blocks) ========"

    # --- Build TALM with DIM_ROWS=P, DIM_COLS ---
    TDIR="$NDIR/talm_P${P}"
    mkdir -p "$TDIR/supers"
    "$PY3" "$GEN_TALM" --out "$TDIR/lcs_wf.hsk" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"

    SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
        CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/lcs_wf.hsk" "$TDIR/supers/Supers.hs"

    LIBSUP="$TDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    # Assemble for P threads
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/lcs_wf_P${P}" "$TDIR/lcs_wf.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/lcs_wf_P${P}.flb"
    PLA="$TDIR/lcs_wf_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/lcs_wf_P${P}.pla"

    # --- Run TALM ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3 4 5 6 7 8 9 10; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
          LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -ne 0 ]]; then
          echo "  WARN: TALM N=$N P=$P rep=$rep rc=$rc (attempt $attempt/10), retrying..."
          sleep 1
          continue
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then break; fi
        echo "  WARN: RESULT= missing (attempt $attempt/10), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM     N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done

    # --- Build GHC forkIO with same grid ---
    GDIR="$NDIR/ghc_P${P}"
    mkdir -p "$GDIR/obj"
    "$PY3" "$GEN_GHC" --out "$GDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"
    "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
        -outputdir "$GDIR/obj" -o "$GDIR/lcs_wf" "$GDIR/lcs_wf.hs" >/dev/null 2>&1

    # --- Run GHC forkIO ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/lcs_wf" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC      N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "GHC N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "ghc,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
