#!/usr/bin/env bash
set -euo pipefail

# Text Search perf stat collection only (LCS + Attention already done)

REPO="$HOME/Ribault"
PERF_ROOT="$REPO/results/perf"
PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"

GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir)"

SUPERS_CFLAGS=""
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

CABAL_PKG_DB=""
for cand in \
  "$HOME/.local/state/cabal/store/ghc-${GHC_VER}/package.db" \
  "$HOME/.cabal/store/ghc-${GHC_VER}/package.db"; do
  if [[ -d "$cand" ]]; then
    CABAL_PKG_DB="-package-db $cand"
    break
  fi
done

TALM_RTS_A=256m
PS=(2 4 8 16)

TS_N=10000
TS_FILE_SIZE=10000000
TS_KEYWORD=FINDME
TS_DENSITY=0.002
TS_N_FUNCS=14
TS_DIR="$PERF_ROOT/textsearch"
TS_BUILD="$TS_DIR/build"
mkdir -p "$TS_DIR" "$TS_BUILD"

GEN_TS_CORPUS="$REPO/scripts/textsearch/gen_corpus.py"
GEN_TS_TALM="$REPO/scripts/textsearch/gen_talm_input.py"
GEN_TS_STRAT="$REPO/scripts/textsearch/gen_hs_strategies.py"
GEN_TS_PARPSEQ="$REPO/scripts/textsearch/gen_hs_parpseq.py"

echo "================================================================"
echo " TEXT SEARCH PERF  N=$TS_N  FILE_SIZE=${TS_FILE_SIZE}"
echo " $(date)"
echo "================================================================"

# Reuse existing corpus
CORPUS="$TS_BUILD/corpus"
if [[ ! -f "$CORPUS/manifest.txt" ]]; then
  echo "FATAL: corpus not found at $CORPUS"
  exit 1
fi

# Pad width = max(4, len(str(n_files - 1)))  -- match gen_corpus.py
MAX_IDX=$((TS_N - 1))
PAD_WIDTH="${#MAX_IDX}"
[[ "$PAD_WIDTH" -lt 4 ]] && PAD_WIDTH=4
echo "  PAD_WIDTH=$PAD_WIDTH"

EXPECTED_TS="$(head -n "$TS_N" "$CORPUS/manifest.txt" | awk '{s+=$2} END{print s}')"
echo "  Expected RESULT=$EXPECTED_TS"

warm_cache() {
  head -n "$TS_N" "$CORPUS/manifest.txt" | awk -v d="$CORPUS" '{print d"/"$1}' | \
    xargs -P 8 cat > /dev/null 2>&1
}

# Rebuild binaries with correct PAD_WIDTH
echo "  Rebuilding TALM with PAD_WIDTH=$PAD_WIDTH..."
TDIR="$TS_BUILD/talm"
mkdir -p "$TDIR/supers"
"$PY3" "$GEN_TS_TALM" --out "$TDIR/ts.hsk" --n-files "$TS_N" \
    --keyword "$TS_KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$TS_N_FUNCS" --pad-width "$PAD_WIDTH"
"$CODEGEN" "$TDIR/ts.hsk" > "$TDIR/ts.fl" 2>/dev/null
INJECT_FILE="$TDIR/supers_inject.hs"
SUPERS_INJECT_FILE="$INJECT_FILE" SUPERS_GHC_PACKAGES="bytestring" \
    CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/ts.hsk" "$TDIR/supers/Supers.hs"
TS_LIBSUP="$TDIR/supers/libsupers.so"
TS_LIBDIR="$(dirname "$TS_LIBSUP")"
TS_GHCDEPS="$TS_LIBDIR/ghc-deps"

echo "  Rebuilding GHC Strategies..."
GDIR="$TS_BUILD/ghc"
mkdir -p "$GDIR/obj"
"$PY3" "$GEN_TS_STRAT" --out "$GDIR/ts.hs" --n-files "$TS_N" \
    --keyword "$TS_KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$TS_N_FUNCS" --pad-width "$PAD_WIDTH"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel $CABAL_PKG_DB \
    -outputdir "$GDIR/obj" -o "$GDIR/ts" "$GDIR/ts.hs" >/dev/null 2>&1

echo "  Rebuilding GHC par/pseq..."
PDIR="$TS_BUILD/parpseq"
mkdir -p "$PDIR/obj"
"$PY3" "$GEN_TS_PARPSEQ" --out "$PDIR/ts.hs" --n-files "$TS_N" \
    --keyword "$TS_KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$TS_N_FUNCS" --pad-width "$PAD_WIDTH"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel $CABAL_PKG_DB \
    -outputdir "$PDIR/obj" -o "$PDIR/ts" "$PDIR/ts.hs" >/dev/null 2>&1

echo "  Built all textsearch variants"

# Quick test
echo "  Quick validation test..."
warm_cache
OUT_TEST="$TS_DIR/test_stdout.txt"
"$GDIR/ts" +RTS -N1 -RTS >"$OUT_TEST" 2>/dev/null
GOT="$(awk -F= '/^RESULT=/{print $2}' "$OUT_TEST")"
echo "  Test: RESULT=$GOT (expected $EXPECTED_TS)"
if [[ "$GOT" != "$EXPECTED_TS" ]]; then
  echo "FATAL: validation failed!"
  exit 1
fi

# Perf: sequential
warm_cache
echo "  PERF: TextSearch seq (warmup)"
"$GDIR/ts" +RTS -N1 -RTS >/dev/null 2>/dev/null
warm_cache
echo "  PERF: TextSearch seq (measured)"
perf stat -d -d -o "$TS_DIR/seq_perf.txt" \
  "$GDIR/ts" +RTS -N1 -RTS >"$TS_DIR/seq_stdout.txt" 2>/dev/null
echo "  -> $TS_DIR/seq_perf.txt"

for P in "${PS[@]}"; do
  echo ""
  echo "  ---- TextSearch P=$P ----"

  # Assemble TALM for this P
  pushd "$ASM_ROOT" >/dev/null
    "$PY3" assembler.py -a -n "$P" -o "$TDIR/ts_P${P}" "$TDIR/ts.fl" >/dev/null 2>&1
  popd >/dev/null
  FLB="$TDIR/ts_P${P}.flb"
  PLA="$TDIR/ts_P${P}_auto.pla"
  [[ -f "$PLA" ]] || PLA="$TDIR/ts_P${P}.pla"

  # TALM warmup + perf
  warm_cache
  echo "  PERF: TextSearch TALM P=$P (warmup)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$TS_LIBDIR:$TS_GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$INTERP" "$P" "$FLB" "$PLA" "$TS_LIBSUP" >/dev/null 2>/dev/null
  set -e
  warm_cache
  echo "  PERF: TextSearch TALM P=$P (measured)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$TS_LIBDIR:$TS_GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    perf stat -d -d -o "$TS_DIR/talm_P${P}_perf.txt" \
    "$INTERP" "$P" "$FLB" "$PLA" "$TS_LIBSUP" \
    >"$TS_DIR/talm_P${P}_stdout.txt" 2>"$TS_DIR/talm_P${P}_stderr.txt"
  set -e
  echo "  -> $TS_DIR/talm_P${P}_perf.txt"

  # GHC Strategies warmup + perf
  warm_cache
  echo "  PERF: TextSearch Strategies P=$P (warmup)"
  "$GDIR/ts" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  warm_cache
  echo "  PERF: TextSearch Strategies P=$P (measured)"
  perf stat -d -d -o "$TS_DIR/strat_P${P}_perf.txt" \
    "$GDIR/ts" +RTS -N"$P" -RTS >"$TS_DIR/strat_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $TS_DIR/strat_P${P}_perf.txt"

  # GHC par/pseq warmup + perf
  warm_cache
  echo "  PERF: TextSearch par/pseq P=$P (warmup)"
  "$PDIR/ts" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  warm_cache
  echo "  PERF: TextSearch par/pseq P=$P (measured)"
  perf stat -d -d -o "$TS_DIR/parpseq_P${P}_perf.txt" \
    "$PDIR/ts" +RTS -N"$P" -RTS >"$TS_DIR/parpseq_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $TS_DIR/parpseq_P${P}_perf.txt"
done

echo ""
echo " TEXT SEARCH PERF COLLECTION COMPLETE"
echo " $(date)"
echo ""
echo "Files:"
find "$TS_DIR" -name '*_perf.txt' | sort
