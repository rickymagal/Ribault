#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# Perf stat collection for paper benchmarks
# Runs each benchmark at its largest N, 1 warmup + 1 perf-measured run
# per variant/P, saving perf stat output to results/perf/
#
# Benchmarks:
#   1. LCS Wavefront   N=100000  DIM_COLS=64  P=2,4,8,16
#   2. Attention        N=8192    D=512        P=2,4,8,16
#   3. Text Search      N=10000   10MB files   P=2,4,8,16
# =============================================================================

REPO="$HOME/Ribault"
PERF_ROOT="$REPO/results/perf"
mkdir -p "$PERF_ROOT"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"

# GHC setup
GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir)"

# HsFFI include
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

# Cabal package-db for parallel/time packages
CABAL_PKG_DB=""
for cand in \
  "$HOME/.local/state/cabal/store/ghc-${GHC_VER}/package.db" \
  "$HOME/.cabal/store/ghc-${GHC_VER}/package.db"; do
  if [[ -d "$cand" ]]; then
    CABAL_PKG_DB="-package-db $cand"
    break
  fi
done

N_PHYS_CORES=24
TALM_RTS_A=256m
PS=(2 4 8 16)

pin_cores() {
  local p="$1"
  if (( p > N_PHYS_CORES )); then
    echo "0-$((N_PHYS_CORES - 1))"
  else
    echo "0-$((p - 1))"
  fi
}

echo "========================================="
echo " PERF STAT COLLECTION"
echo " $(date)"
echo " GHC $GHC_VER"
echo " P values: ${PS[*]}"
echo " Output: $PERF_ROOT"
echo "========================================="


# #############################################################################
# 1. LCS WAVEFRONT  (N=100000, DIM_COLS=64)
# #############################################################################
echo ""
echo "================================================================"
echo " LCS WAVEFRONT  N=100000  DIM_COLS=64"
echo "================================================================"

LCS_N=100000
LCS_DIM_COLS=64
LCS_SEED=42
LCS_ALPHABET=4
LCS_DIR="$PERF_ROOT/lcs"
LCS_BUILD="$LCS_DIR/build"
mkdir -p "$LCS_DIR" "$LCS_BUILD"

GEN_LCS_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_LCS_SEQ="$REPO/scripts/lcs_wavefront/gen_hs_sequential.py"
GEN_LCS_TALM="$REPO/scripts/lcs_wavefront/gen_talm_input.py"
GEN_LCS_PARPSEQ="$REPO/scripts/lcs_wavefront/gen_hs_parpseq.py"
GEN_LCS_STRAT="$REPO/scripts/lcs_wavefront/gen_hs_strategies.py"

# Generate input
INPUT_DIR="$LCS_BUILD/input"
"$PY3" "$GEN_LCS_INPUT" --N "$LCS_N" --alphabet "$LCS_ALPHABET" --seed "$LCS_SEED" --out-dir "$INPUT_DIR"
EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
echo "  Expected LCS score: $EXPECTED"

# Build sequential
SDIR="$LCS_BUILD/seq"
mkdir -p "$SDIR/obj"
"$PY3" "$GEN_LCS_SEQ" --out "$SDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" --dim-rows 1 --dim-cols 1
"$GHC_BIN" -O2 -rtsopts -package time \
    -outputdir "$SDIR/obj" -o "$SDIR/lcs_wf" "$SDIR/lcs_wf.hs" >/dev/null 2>&1
echo "  Built sequential baseline"

# Perf: sequential
echo "  PERF: LCS seq (warmup)"
"$SDIR/lcs_wf" >/dev/null 2>/dev/null
echo "  PERF: LCS seq (measured)"
perf stat -d -d -o "$LCS_DIR/seq_perf.txt" "$SDIR/lcs_wf" >"$LCS_DIR/seq_stdout.txt" 2>/dev/null
echo "  -> $LCS_DIR/seq_perf.txt"

for P in "${PS[@]}"; do
  CUR_ROWS=$P
  echo ""
  echo "  ---- LCS P=$P (${CUR_ROWS}x${LCS_DIM_COLS} grid) ----"

  # Build TALM
  TDIR="$LCS_BUILD/talm_P${P}"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_LCS_TALM" --out "$TDIR/lcs_wf.hsk" --input-dir "$INPUT_DIR" \
      --dim-rows "$CUR_ROWS" --dim-cols "$LCS_DIM_COLS"
  SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/lcs_wf.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"
  pushd "$ASM_ROOT" >/dev/null
    "$PY3" assembler.py -a -n "$P" -o "$TDIR/lcs_wf_P${P}" "$TDIR/lcs_wf.fl" >/dev/null 2>&1
  popd >/dev/null
  FLB="$TDIR/lcs_wf_P${P}.flb"
  PLA="$TDIR/lcs_wf_P${P}_auto.pla"
  [[ -f "$PLA" ]] || PLA="$TDIR/lcs_wf_P${P}.pla"

  # TALM warmup
  echo "  PERF: LCS TALM P=$P (warmup)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >/dev/null 2>/dev/null
  set -e
  # TALM perf
  echo "  PERF: LCS TALM P=$P (measured)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    perf stat -d -d -o "$LCS_DIR/talm_P${P}_perf.txt" \
    "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$LCS_DIR/talm_P${P}_stdout.txt" 2>"$LCS_DIR/talm_P${P}_stderr.txt"
  set -e
  echo "  -> $LCS_DIR/talm_P${P}_perf.txt"

  # Build par/pseq
  PPDIR="$LCS_BUILD/parpseq_P${P}"
  mkdir -p "$PPDIR/obj"
  "$PY3" "$GEN_LCS_PARPSEQ" --out "$PPDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
      --dim-rows "$CUR_ROWS" --dim-cols "$LCS_DIM_COLS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel $CABAL_PKG_DB \
      -outputdir "$PPDIR/obj" -o "$PPDIR/lcs_wf" "$PPDIR/lcs_wf.hs" >/dev/null 2>&1

  # par/pseq warmup
  echo "  PERF: LCS par/pseq P=$P (warmup)"
  "$PPDIR/lcs_wf" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  # par/pseq perf
  echo "  PERF: LCS par/pseq P=$P (measured)"
  perf stat -d -d -o "$LCS_DIR/parpseq_P${P}_perf.txt" \
    "$PPDIR/lcs_wf" +RTS -N"$P" -RTS >"$LCS_DIR/parpseq_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $LCS_DIR/parpseq_P${P}_perf.txt"

  # Build Strategies
  STDIR="$LCS_BUILD/strat_P${P}"
  mkdir -p "$STDIR/obj"
  "$PY3" "$GEN_LCS_STRAT" --out "$STDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
      --dim-rows "$CUR_ROWS" --dim-cols "$LCS_DIM_COLS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel $CABAL_PKG_DB \
      -outputdir "$STDIR/obj" -o "$STDIR/lcs_wf" "$STDIR/lcs_wf.hs" >/dev/null 2>&1

  # Strategies warmup
  echo "  PERF: LCS Strategies P=$P (warmup)"
  "$STDIR/lcs_wf" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  # Strategies perf
  echo "  PERF: LCS Strategies P=$P (measured)"
  perf stat -d -d -o "$LCS_DIR/strat_P${P}_perf.txt" \
    "$STDIR/lcs_wf" +RTS -N"$P" -RTS >"$LCS_DIR/strat_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $LCS_DIR/strat_P${P}_perf.txt"
done

echo ""
echo " LCS PERF COLLECTION COMPLETE"


# #############################################################################
# 2. ATTENTION  (N=8192, D=512, N_FUNCS=14)
# #############################################################################
echo ""
echo "================================================================"
echo " ATTENTION  N=8192  D=512  N_FUNCS=14"
echo "================================================================"

ATTN_N=8192
ATTN_D=512
ATTN_N_FUNCS=14
ATTN_DIR="$PERF_ROOT/attention"
ATTN_BUILD="$ATTN_DIR/build"
mkdir -p "$ATTN_DIR" "$ATTN_BUILD"

GEN_ATTN_DATA="$REPO/scripts/attention/gen_data.py"
GEN_ATTN_TALM="$REPO/scripts/attention/gen_talm_input.py"
GEN_ATTN_SEQ="$REPO/scripts/attention/gen_hs_sequential.py"
GEN_ATTN_STRAT="$REPO/scripts/attention/gen_hs_strategies.py"
GEN_ATTN_PARPSEQ="$REPO/scripts/attention/gen_hs_parpseq.py"

# Generate data
DATADIR="$ATTN_BUILD/data"
"$PY3" "$GEN_ATTN_DATA" --out-dir "$DATADIR" --N "$ATTN_N" --D "$ATTN_D"
echo "  Generated Q/K/V data"

# Build TALM (shared supers for all P)
TDIR="$ATTN_BUILD/talm"
mkdir -p "$TDIR/supers"
"$PY3" "$GEN_ATTN_TALM" --out "$TDIR/attn.hsk" --N "$ATTN_N" --D "$ATTN_D" \
    --n-funcs "$ATTN_N_FUNCS" --data-dir "$DATADIR"
"$CODEGEN" "$TDIR/attn.hsk" > "$TDIR/attn.fl" 2>/dev/null
CFLAGS="$SUPERS_CFLAGS" SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
    SUPERS_GHC_PACKAGES="bytestring" \
    bash "$BUILD_SUPERS" "$TDIR/attn.hsk" "$TDIR/supers/Supers.hs"
ATTN_LIBSUP="$TDIR/supers/libsupers.so"
ATTN_LIBDIR="$(dirname "$ATTN_LIBSUP")"
ATTN_GHCDEPS="$ATTN_LIBDIR/ghc-deps"
echo "  Built TALM supers"

# Build GHC Sequential
SDIR="$ATTN_BUILD/seq"
mkdir -p "$SDIR/obj"
"$PY3" "$GEN_ATTN_SEQ" --out "$SDIR/attn.hs" --N "$ATTN_N" --D "$ATTN_D" \
    --n-funcs "$ATTN_N_FUNCS" --data-dir "$DATADIR"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring \
    -outputdir "$SDIR/obj" -o "$SDIR/attn" "$SDIR/attn.hs" >/dev/null 2>&1

# Build GHC Strategies
GDIR="$ATTN_BUILD/ghc"
mkdir -p "$GDIR/obj"
"$PY3" "$GEN_ATTN_STRAT" --out "$GDIR/attn.hs" --N "$ATTN_N" --D "$ATTN_D" \
    --n-funcs "$ATTN_N_FUNCS" --data-dir "$DATADIR"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package bytestring $CABAL_PKG_DB \
    -outputdir "$GDIR/obj" -o "$GDIR/attn" "$GDIR/attn.hs" >/dev/null 2>&1

# Build GHC par/pseq
PDIR="$ATTN_BUILD/parpseq"
mkdir -p "$PDIR/obj"
"$PY3" "$GEN_ATTN_PARPSEQ" --out "$PDIR/attn.hs" --N "$ATTN_N" --D "$ATTN_D" \
    --n-funcs "$ATTN_N_FUNCS" --data-dir "$DATADIR"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring $CABAL_PKG_DB \
    -outputdir "$PDIR/obj" -o "$PDIR/attn" "$PDIR/attn.hs" >/dev/null 2>&1

echo "  Built all attention variants"

# Perf: sequential
CORES_1="$(pin_cores 1)"
echo "  PERF: Attention seq (warmup)"
taskset -c "$CORES_1" "$SDIR/attn" +RTS -N1 -RTS >/dev/null 2>/dev/null
echo "  PERF: Attention seq (measured)"
perf stat -d -d -o "$ATTN_DIR/seq_perf.txt" \
  taskset -c "$CORES_1" "$SDIR/attn" +RTS -N1 -RTS >"$ATTN_DIR/seq_stdout.txt" 2>/dev/null
echo "  -> $ATTN_DIR/seq_perf.txt"

for P in "${PS[@]}"; do
  echo ""
  echo "  ---- Attention P=$P ----"
  CORES_P="$(pin_cores "$P")"

  # Assemble TALM for this P
  pushd "$ASM_ROOT" >/dev/null
    "$PY3" assembler.py -a -n "$P" -o "$TDIR/attn_P${P}" "$TDIR/attn.fl" >/dev/null 2>&1
  popd >/dev/null
  FLB="$TDIR/attn_P${P}.flb"
  PLA="$TDIR/attn_P${P}_auto.pla"
  [[ -f "$PLA" ]] || PLA="$TDIR/attn_P${P}.pla"

  # TALM warmup + perf
  echo "  PERF: Attention TALM P=$P (warmup)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$ATTN_LIBDIR:$ATTN_GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$ATTN_LIBSUP" >/dev/null 2>/dev/null
  set -e
  echo "  PERF: Attention TALM P=$P (measured)"
  set +e
  SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$ATTN_LIBDIR:$ATTN_GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    perf stat -d -d -o "$ATTN_DIR/talm_P${P}_perf.txt" \
    taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$ATTN_LIBSUP" \
    >"$ATTN_DIR/talm_P${P}_stdout.txt" 2>"$ATTN_DIR/talm_P${P}_stderr.txt"
  set -e
  echo "  -> $ATTN_DIR/talm_P${P}_perf.txt"

  # GHC Strategies warmup + perf
  echo "  PERF: Attention Strategies P=$P (warmup)"
  taskset -c "$CORES_P" "$GDIR/attn" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  echo "  PERF: Attention Strategies P=$P (measured)"
  perf stat -d -d -o "$ATTN_DIR/strat_P${P}_perf.txt" \
    taskset -c "$CORES_P" "$GDIR/attn" +RTS -N"$P" -RTS >"$ATTN_DIR/strat_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $ATTN_DIR/strat_P${P}_perf.txt"

  # GHC par/pseq warmup + perf
  echo "  PERF: Attention par/pseq P=$P (warmup)"
  taskset -c "$CORES_P" "$PDIR/attn" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
  echo "  PERF: Attention par/pseq P=$P (measured)"
  perf stat -d -d -o "$ATTN_DIR/parpseq_P${P}_perf.txt" \
    taskset -c "$CORES_P" "$PDIR/attn" +RTS -N"$P" -RTS >"$ATTN_DIR/parpseq_P${P}_stdout.txt" 2>/dev/null
  echo "  -> $ATTN_DIR/parpseq_P${P}_perf.txt"
done

echo ""
echo " ATTENTION PERF COLLECTION COMPLETE"


# #############################################################################
# 3. TEXT SEARCH  (N=10000 files, FILE_SIZE=10000000)
# #############################################################################
echo ""
echo "================================================================"
echo " TEXT SEARCH  N=10000  FILE_SIZE=10MB"
echo "================================================================"

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

# Generate corpus
CORPUS="$TS_BUILD/corpus"
if [[ ! -f "$CORPUS/manifest.txt" ]]; then
  echo "  Generating corpus: $TS_N files, ~${TS_FILE_SIZE}B each..."
  "$PY3" "$GEN_TS_CORPUS" --n-files "$TS_N" --file-size "$TS_FILE_SIZE" \
      --keyword "$TS_KEYWORD" --density "$TS_DENSITY" --out-dir "$CORPUS" --seed 42
else
  echo "  Reusing existing corpus"
fi

CORPUS_N="$(wc -l < "$CORPUS/manifest.txt")"
PAD_WIDTH="${#CORPUS_N}"
[[ "$PAD_WIDTH" -lt 4 ]] && PAD_WIDTH=4

# Expected result
EXPECTED_TS="$(head -n "$TS_N" "$CORPUS/manifest.txt" | awk '{s+=$2} END{print s}')"
echo "  Expected RESULT=$EXPECTED_TS"

# Warm page cache helper
warm_cache() {
  head -n "$TS_N" "$CORPUS/manifest.txt" | awk -v d="$CORPUS" '{print d"/"$1}' | \
    xargs -P 8 cat > /dev/null 2>&1
}

# Build TALM
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
echo "  Built TALM supers"

# Build GHC Strategies
GDIR="$TS_BUILD/ghc"
mkdir -p "$GDIR/obj"
"$PY3" "$GEN_TS_STRAT" --out "$GDIR/ts.hs" --n-files "$TS_N" \
    --keyword "$TS_KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$TS_N_FUNCS" --pad-width "$PAD_WIDTH"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel $CABAL_PKG_DB \
    -outputdir "$GDIR/obj" -o "$GDIR/ts" "$GDIR/ts.hs" >/dev/null 2>&1

# Build GHC par/pseq
PDIR="$TS_BUILD/parpseq"
mkdir -p "$PDIR/obj"
"$PY3" "$GEN_TS_PARPSEQ" --out "$PDIR/ts.hs" --n-files "$TS_N" \
    --keyword "$TS_KEYWORD" --corpus-dir "$CORPUS" --n-funcs "$TS_N_FUNCS" --pad-width "$PAD_WIDTH"
"$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring -package parallel $CABAL_PKG_DB \
    -outputdir "$PDIR/obj" -o "$PDIR/ts" "$PDIR/ts.hs" >/dev/null 2>&1

echo "  Built all textsearch variants"

# Perf: sequential (GHC Strategies binary with -N1)
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

echo ""
echo "========================================="
echo " ALL PERF DATA COLLECTED"
echo " Output: $PERF_ROOT"
echo " $(date)"
echo "========================================="
echo ""
echo "Files:"
find "$PERF_ROOT" -name '*_perf.txt' | sort
