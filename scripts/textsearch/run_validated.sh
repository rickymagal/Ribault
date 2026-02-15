#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Text-Search benchmark: TALM + GHC Strategies + GHC par/pseq
# Demonstrates pipelining advantage of dataflow execution.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/textsearch}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_CORPUS="$REPO/scripts/textsearch/gen_corpus.py"
GEN_TALM="$REPO/scripts/textsearch/gen_talm_input.py"
GEN_STRAT="$REPO/scripts/textsearch/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/textsearch/gen_hs_parpseq.py"

REPS=${REPS:-3}
N_FILES=${N_FILES:-200}
FILE_SIZE=${FILE_SIZE:-50000}
KEYWORD=${KEYWORD:-FINDME}
DENSITY=${DENSITY:-0.002}
N_FUNCS=${N_FUNCS:-14}
PS=(${PS:-1 2 4 8 12 16 24})
TALM_RTS_A=${TALM_RTS_A:-64m}  # GHC RTS allocation area for TALM supers

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

# Validate result
validate() {
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found"
    exit 1
  fi
  if [[ "$got" != "$expected" ]]; then
    echo "FATAL: $label -> got $got, expected $expected"
    exit 1
  fi
  echo "  OK: RESULT=$got (correct)"
}

# Warm page cache for the corpus
warm_cache() {
  cat "$CORPUS"/file_*.txt > /dev/null 2>&1
}

# ===== Generate corpus =====
CORPUS="$OUTROOT/corpus"
echo "Generating corpus: $N_FILES files, ~${FILE_SIZE}B each..."
"$PY3" "$GEN_CORPUS" --n-files "$N_FILES" --file-size "$FILE_SIZE" \
    --keyword "$KEYWORD" --density "$DENSITY" --out-dir "$CORPUS" --seed 42

EXPECTED="$(cat "$CORPUS/expected_total.txt")"

# Warm page cache so all variants start from same state
echo "Warming page cache..."
warm_cache

CSV="$OUTROOT/metrics.csv"
echo "variant,n_files,file_size,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Text-Search Benchmark (validated)"
echo " N_FILES=$N_FILES  FILE_SIZE=$FILE_SIZE  KEYWORD=$KEYWORD"
echo " P=${PS[*]}  reps=$REPS  TALM_RTS_A=$TALM_RTS_A"
echo " Expected total: $EXPECTED"
echo " Output: $OUTROOT"
echo "========================================="

# ===== Build TALM (once for all P) =====
TDIR="$OUTROOT/talm"
mkdir -p "$TDIR/supers"

echo ""
echo "--- Building TALM program ---"
"$PY3" "$GEN_TALM" --out "$TDIR/ts.hsk" --n-files "$N_FILES" \
    --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS"
"$CODEGEN" "$TDIR/ts.hsk" > "$TDIR/ts.fl" 2>/dev/null

# Build supers with inject file
INJECT_FILE="$TDIR/supers_inject.hs"

SUPERS_INJECT_FILE="$INJECT_FILE" SUPERS_GHC_PACKAGES="bytestring" \
    CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/ts.hsk" "$TDIR/supers/Supers.hs"

LIBSUP="$TDIR/supers/libsupers.so"
LIBDIR="$(dirname "$LIBSUP")"
GHCDEPS="$LIBDIR/ghc-deps"

# ===== Build GHC variants (once) =====
GDIR="$OUTROOT/ghc"
mkdir -p "$GDIR/obj"
echo ""
echo "--- Building GHC Strategies ---"
"$PY3" "$GEN_STRAT" --out "$GDIR/ts.hs" --n-files "$N_FILES" \
    --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package bytestring \
    -outputdir "$GDIR/obj" -o "$GDIR/ts" "$GDIR/ts.hs" >/dev/null 2>&1

PDIR="$OUTROOT/parpseq"
mkdir -p "$PDIR/obj"
echo ""
echo "--- Building GHC par/pseq ---"
"$PY3" "$GEN_PARPSEQ" --out "$PDIR/ts.hs" --n-files "$N_FILES" \
    --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package bytestring \
    -outputdir "$PDIR/obj" -o "$PDIR/ts" "$PDIR/ts.hs" >/dev/null 2>&1

# ===== Run benchmarks =====
for P in "${PS[@]}"; do
  echo ""
  echo "======== P=$P ========"

  # --- TALM ---
  # Assemble for this P
  pushd "$ASM_ROOT" >/dev/null
    "$PY3" assembler.py -a -n "$P" -o "$TDIR/ts_P${P}" "$TDIR/ts.fl" >/dev/null 2>&1
  popd >/dev/null
  FLB="$TDIR/ts_P${P}.flb"
  PLA="$TDIR/ts_P${P}_auto.pla"
  [[ -f "$PLA" ]] || PLA="$TDIR/ts_P${P}.pla"

  for ((rep=1; rep<=REPS; rep++)); do
    warm_cache
    OUT="$TDIR/out_P${P}_r${rep}.txt"
    ERR="$TDIR/err_P${P}_r${rep}.txt"
    for attempt in 1 2 3 4 5 6 7 8 9 10; do
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      rc=$?
      set -e
      if [[ $rc -ne 0 ]]; then
        echo "  WARN: TALM P=$P rep=$rep rc=$rc (attempt $attempt/10), retrying..."
        continue
      fi
      got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
      if [[ -n "$got" ]]; then break; fi
      echo "  WARN: RESULT= missing (attempt $attempt/10), retrying..."
    done
    secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
    echo "TALM     P=$P rep=$rep -> ${secs}s"
    validate "TALM P=$P rep=$rep" "$OUT" "$EXPECTED"
    echo "super,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
  done

  # --- GHC Strategies (default RTS settings) ---
  for ((rep=1; rep<=REPS; rep++)); do
    warm_cache
    OUT="$GDIR/out_P${P}_r${rep}.txt"
    "$GDIR/ts" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "GHC-Str  P=$P rep=$rep -> ${secs}s"
    validate "GHC-Strategies P=$P rep=$rep" "$OUT" "$EXPECTED"
    echo "ghc,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
  done

  # --- GHC par/pseq (default RTS settings) ---
  for ((rep=1; rep<=REPS; rep++)); do
    warm_cache
    OUT="$PDIR/out_P${P}_r${rep}.txt"
    "$PDIR/ts" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "GHC-PP   P=$P rep=$rep -> ${secs}s"
    validate "GHC-par/pseq P=$P rep=$rep" "$OUT" "$EXPECTED"
    echo "parpseq,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
