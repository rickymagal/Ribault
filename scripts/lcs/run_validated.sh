#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated LCS Benchmark
# TALM + GHC Strategies + GHC par/pseq
#
# Computes LCS for N_PAIRS independent string pairs per run.
# Sweeps over STR_LEN values (NS) and processor counts (PS).
# Every repetition is validated against the expected sum.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_INPUT="$REPO/scripts/lcs/gen_input.py"
GEN_TALM="$REPO/scripts/lcs/gen_talm_input.py"
GEN_STRAT="$REPO/scripts/lcs/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/lcs/gen_hs_parpseq.py"

REPS=${REPS:-3}
SEED=${SEED:-42}
N_FUNCS=${N_FUNCS:-32}
N_PAIRS=${N_PAIRS:-64}
ALPHABET=${ALPHABET:-4}
# Sweep over string lengths
NS=(${NS:-200 400 600 800 1000})
PS=(${PS:-1 2 4 8})
TALM_RTS_A=${TALM_RTS_A:-128m}

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
  if [[ "$got" != "$expected" ]]; then
    echo "FATAL: $label -> got $got, expected $expected"
    exit 1
  fi
  echo "  OK: RESULT=$got (correct)"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,str_len,n_pairs,P,rep,seconds" > "$CSV"

echo "========================================="
echo " LCS Benchmark (Longest Common Subsequence)"
echo " NS (str_len)=${NS[*]}"
echo " N_PAIRS=$N_PAIRS  ALPHABET=$ALPHABET"
echo " PS=${PS[*]}  reps=$REPS  seed=$SEED  n_funcs=$N_FUNCS"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# STR_LEN=$N  N_PAIRS=$N_PAIRS"
  echo "############################################"

  NDIR="$OUTROOT/L_${N}"
  mkdir -p "$NDIR"

  # ===== Generate input + expected =====
  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --n-pairs "$N_PAIRS" --str-len "$N" \
      --alphabet "$ALPHABET" --seed "$SEED" --out-dir "$INPUT_DIR"
  EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
  echo "  Expected sum of LCS lengths: $EXPECTED"

  # ===== Build TALM =====
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/lcs.hsk" --input-dir "$INPUT_DIR" \
      --n-funcs "$N_FUNCS"
  "$CODEGEN" "$TDIR/lcs.hsk" > "$TDIR/lcs.fl" 2>/dev/null

  INJECT_FILE="$TDIR/supers_inject.hs"
  SUPERS_INJECT_FILE="$INJECT_FILE" \
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/lcs.hsk" "$TDIR/supers/Supers.hs"

  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # ===== Build GHC Strategies =====
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/lcs.hs" --input-dir "$INPUT_DIR" \
      --n-funcs "$N_FUNCS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package array \
      -outputdir "$GDIR/obj" -o "$GDIR/lcs" "$GDIR/lcs.hs" >/dev/null 2>&1

  # ===== Build GHC par/pseq =====
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/lcs.hs" --input-dir "$INPUT_DIR" \
      --n-funcs "$N_FUNCS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package array \
      -outputdir "$PDIR/obj" -o "$PDIR/lcs" "$PDIR/lcs.hs" >/dev/null 2>&1

  # ===== Run benchmarks =====
  for P in "${PS[@]}"; do
    echo ""
    echo "======== STR_LEN=$N  P=$P ========"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/lcs_P${P}" "$TDIR/lcs.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/lcs_P${P}.flb"
    PLA="$TDIR/lcs_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/lcs_P${P}.pla"

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
          echo "  WARN: TALM L=$N P=$P rep=$rep rc=$rc (attempt $attempt/10), retrying..."
          sleep 1
          continue
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then break; fi
        echo "  WARN: RESULT= missing (attempt $attempt/10), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM     L=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM L=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$N_PAIRS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/lcs" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-Str  L=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies L=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "ghc,$N,$N_PAIRS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PDIR/lcs" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-PP   L=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq L=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$N_PAIRS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
