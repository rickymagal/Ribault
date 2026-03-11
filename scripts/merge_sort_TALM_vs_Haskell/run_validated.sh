#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Merge Sort Benchmark
# Sequential baseline + TALM + par/pseq + Strategies
#
# TALM uses EX21 pattern: pure DF recursion with ms_super leaf.
# GHC variants use equivalent par/pseq and Strategies parallelism.
# All variants sort [N, N-1, ..., 1] and verify RESULT=1.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/merge_sort}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
MS_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_INPUT="$MS_DIR/gen_input.py"
GEN_TALM="$MS_DIR/gen_talm_input.py"
GEN_SEQ="$MS_DIR/gen_hs_sequential.py"
GEN_PARPSEQ="$MS_DIR/gen_hs_parpseq.py"
GEN_STRAT="$MS_DIR/gen_hs_strategies.py"

REPS=${REPS:-3}
NS=(${NS:-1000 2000 5000})
PS=(${PS:-2 4 8})
TALM_RTS_A=${TALM_RTS_A:-256m}

# Detect HsFFI include for build_supers
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
echo "variant,seq_len,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Merge Sort Benchmark"
echo " NS=${NS[*]}"
echo " PS=${PS[*]}  reps=$REPS"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# N=$N"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # ===== Generate input =====
  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --N "$N" --out-dir "$INPUT_DIR"
  EXPECTED="1"

  # ===== Build & run sequential baseline =====
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/ms_seq.hs" --input-dir "$INPUT_DIR"
  "$GHC_BIN" -O2 -rtsopts -package time \
      -outputdir "$SDIR/obj" -o "$SDIR/ms_seq" "$SDIR/ms_seq.hs" >/dev/null 2>&1

  echo ""
  echo "======== N=$N  SEQUENTIAL BASELINE ========"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    "$SDIR/ms_seq" >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "SEQ      N=$N rep=$rep -> ${secs}s"
    validate "SEQ N=$N rep=$rep" "$OUT" "$EXPECTED"
    echo "seq,$N,1,$rep,$secs" >> "$CSV"
  done

  # ===== Run parallel benchmarks =====
  for P in "${PS[@]}"; do
    echo ""
    echo "======== N=$N  P=$P ========"

    # --- Build TALM ---
    TDIR="$NDIR/talm_P${P}"
    mkdir -p "$TDIR/supers"
    "$PY3" "$GEN_TALM" --out "$TDIR/ms.hsk" --input-dir "$INPUT_DIR" --P "$P"

    # Codegen: .hsk -> .fl
    "$CODEGEN" "$TDIR/ms.hsk" > "$TDIR/ms.fl" 2>/dev/null

    # Build supers
    CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/ms.hsk" "$TDIR/supers/Supers.hs" >/dev/null 2>&1

    LIBSUP="$TDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    # Assemble for P threads
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/ms_P${P}" "$TDIR/ms.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/ms_P${P}.flb"
    PLA="$TDIR/ms_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/ms_P${P}.pla"

    # --- Run TALM ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3 4 5 6 7 8 9 10; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
          SUPERS_FORCE_PAR=1 \
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
      echo "TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$P,$rep,$secs" >> "$CSV"
    done

    # --- Build par/pseq ---
    PPDIR="$NDIR/parpseq_P${P}"
    mkdir -p "$PPDIR/obj"
    "$PY3" "$GEN_PARPSEQ" --out "$PPDIR/ms_pp.hs" --input-dir "$INPUT_DIR" --P "$P"
    "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
        -outputdir "$PPDIR/obj" -o "$PPDIR/ms_pp" "$PPDIR/ms_pp.hs" >/dev/null 2>&1

    # --- Run par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PPDIR/out_P${P}_r${rep}.txt"
      "$PPDIR/ms_pp" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "PARPSEQ  N=$N P=$P rep=$rep -> ${secs}s"
      validate "PARPSEQ N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$P,$rep,$secs" >> "$CSV"
    done

    # --- Build Strategies ---
    STDIR="$NDIR/strat_P${P}"
    mkdir -p "$STDIR/obj"
    "$PY3" "$GEN_STRAT" --out "$STDIR/ms_st.hs" --input-dir "$INPUT_DIR" --P "$P"
    "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package deepseq \
        -outputdir "$STDIR/obj" -o "$STDIR/ms_st" "$STDIR/ms_st.hs" >/dev/null 2>&1

    # --- Run Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$STDIR/out_P${P}_r${rep}.txt"
      "$STDIR/ms_st" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "STRAT    N=$N P=$P rep=$rep -> ${secs}s"
      validate "STRAT N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "strategies,$N,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
