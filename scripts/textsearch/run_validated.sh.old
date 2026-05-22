#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Text-Search benchmark: TALM + GHC Strategies + GHC par/pseq
#
# Structure:
#   - Sequential baseline (P=1, GHC -N1): same for all variants
#   - Parallel strategies (P>=2): TALM, GHC Strategies, GHC par/pseq
#
# Supports pre-generated corpus (CORPUS_DIR) and multiple N values (NS_CSV).
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
FILE_SIZE=${FILE_SIZE:-10000000}
KEYWORD=${KEYWORD:-FINDME}
DENSITY=${DENSITY:-0.002}
N_FUNCS=${N_FUNCS:-14}
PS=(${PS:-2 4 8 16})
TALM_RTS_A=${TALM_RTS_A:-256m}

# NS_CSV: comma-separated list of N_FILES values to benchmark
NS_CSV="${NS_CSV:-2000,4000,6000,8000,10000}"
IFS=',' read -r -a NS_LIST <<< "$NS_CSV"

# Pre-generated corpus directory (skip generation if set and exists)
CORPUS_DIR="${CORPUS_DIR:-}"

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

# Compute expected total for first N files from manifest
expected_for_n() {
  local manifest="$1" n="$2"
  head -n "$n" "$manifest" | awk '{s+=$2} END{print s}'
}

# Warm page cache for N files
warm_cache() {
  local corpus="$1" n="$2"
  head -n "$n" "$corpus/manifest.txt" | awk -v d="$corpus" '{print d"/"$1}' | \
    xargs -P 8 cat > /dev/null 2>&1
}

# ===== Generate or reuse corpus =====
MAX_N="${NS_LIST[-1]}"  # largest N value

if [[ -n "$CORPUS_DIR" && -f "$CORPUS_DIR/manifest.txt" ]]; then
  CORPUS="$CORPUS_DIR"
  CORPUS_N="$(wc -l < "$CORPUS/manifest.txt")"
  if [[ "$CORPUS_N" -lt "$MAX_N" ]]; then
    echo "FATAL: corpus has $CORPUS_N files but MAX_N=$MAX_N"
    exit 1
  fi
  echo "Using pre-generated corpus: $CORPUS ($CORPUS_N files)"
else
  CORPUS="$OUTROOT/corpus"
  CORPUS_N="$MAX_N"
  echo "Generating corpus: $MAX_N files, ~${FILE_SIZE}B each..."
  "$PY3" "$GEN_CORPUS" --n-files "$MAX_N" --file-size "$FILE_SIZE" \
      --keyword "$KEYWORD" --density "$DENSITY" --out-dir "$CORPUS" --seed 42
  CORPUS_DIR="$CORPUS"
fi

# Compute pad width from total corpus size (filenames use this many digits)
PAD_WIDTH="${#CORPUS_N}"
[[ "$PAD_WIDTH" -lt 4 ]] && PAD_WIDTH=4

CSV="$OUTROOT/metrics.csv"
echo "variant,n_files,file_size,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Text-Search Benchmark (validated)"
echo " FILE_SIZE=$FILE_SIZE  KEYWORD=$KEYWORD"
echo " N values: ${NS_LIST[*]}"
echo " P=${PS[*]}  reps=$REPS  TALM_RTS_A=$TALM_RTS_A"
echo " Output: $OUTROOT"
echo "========================================="

# ===== Loop over N values =====
for N_FILES in "${NS_LIST[@]}"; do
  TOTAL_MB=$((N_FILES * FILE_SIZE / 1048576))
  EXPECTED="$(expected_for_n "$CORPUS/manifest.txt" "$N_FILES")"

  echo ""
  echo "############################################################"
  echo "# N=$N_FILES files  (${TOTAL_MB}MB total)  expected=$EXPECTED"
  echo "############################################################"

  NDIR="$OUTROOT/N_${N_FILES}"
  mkdir -p "$NDIR"

  # --- Build TALM ---
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/ts.hsk" --n-files "$N_FILES" \
      --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS" --pad-width "$PAD_WIDTH"
  "$CODEGEN" "$TDIR/ts.hsk" > "$TDIR/ts.fl" 2>/dev/null
  INJECT_FILE="$TDIR/supers_inject.hs"
  SUPERS_INJECT_FILE="$INJECT_FILE" SUPERS_GHC_PACKAGES="bytestring" \
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/ts.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # --- Build GHC Strategies ---
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/ts.hs" --n-files "$N_FILES" \
      --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS" --pad-width "$PAD_WIDTH"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel \
      -outputdir "$GDIR/obj" -o "$GDIR/ts" "$GDIR/ts.hs" >/dev/null 2>&1

  # --- Build GHC par/pseq ---
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/ts.hs" --n-files "$N_FILES" \
      --keyword "$KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$N_FUNCS" --pad-width "$PAD_WIDTH"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel \
      -outputdir "$PDIR/obj" -o "$PDIR/ts" "$PDIR/ts.hs" >/dev/null 2>&1

  echo "  Built all variants for N=$N_FILES"

  # ===== Sequential baseline (P=1) =====
  echo ""
  echo "  ---- Sequential Baseline (P=1) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    warm_cache "$CORPUS" "$N_FILES"
    OUT="$GDIR/out_seq_r${rep}.txt"
    "$GDIR/ts" +RTS -N1 -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "  SEQ      N=$N_FILES P=1 rep=$rep -> ${secs}s"
    validate "Sequential N=$N_FILES P=1 rep=$rep" "$OUT" "$EXPECTED"
    echo "seq,$N_FILES,$FILE_SIZE,1,$rep,$secs" >> "$CSV"
  done

  # ===== Parallel benchmarks (P>=2) =====
  for P in "${PS[@]}"; do
    echo ""
    echo "  ---- P=$P ----"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/ts_P${P}" "$TDIR/ts.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/ts_P${P}.flb"
    PLA="$TDIR/ts_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/ts_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      warm_cache "$CORPUS" "$N_FILES"
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -eq 0 ]]; then
          got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
          if [[ -n "$got" ]]; then break; fi
        fi
        echo "    WARN: TALM N=$N_FILES P=$P rep=$rep attempt=$attempt failed (rc=$rc)"
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "  TALM     N=$N_FILES P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N_FILES P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      warm_cache "$CORPUS" "$N_FILES"
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/ts" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-Str  N=$N_FILES P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N_FILES P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "strat,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      warm_cache "$CORPUS" "$N_FILES"
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PDIR/ts" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-PP   N=$N_FILES P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N_FILES P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N_FILES,$FILE_SIZE,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
