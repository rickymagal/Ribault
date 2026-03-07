#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Merge Sort Benchmark
# Sequential baseline + TALM + par/pseq + Strategies
#
# All variants use the same algorithm: Haskell linked-list merge sort.
# Speedup is measured against the sequential baseline (single-threaded).
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/merge_sort}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_SEQ="$SCRIPT_DIR/gen_hs_sequential.py"
GEN_TALM="$SCRIPT_DIR/gen_talm_input.py"
GEN_PARPSEQ="$SCRIPT_DIR/gen_hs_parpseq.py"
GEN_STRAT="$SCRIPT_DIR/gen_hs_strategies.py"
CODEGEN="$REPO/codegen"

REPS=${REPS:-3}
NS=(${NS:-50000000 100000000 150000000 200000000 250000000 300000000 350000000 400000000 450000000 500000000 550000000 600000000 650000000 700000000 750000000 800000000 850000000 900000000 950000000 1000000000})
PS=(${PS:-2 4 8 16})

# Detect HsFFI include
GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR_RAW="$("$GHC_BIN" --print-libdir)"
SUPERS_CFLAGS="${CFLAGS:-}"
if [[ -z "$SUPERS_CFLAGS" ]]; then
  for cand in \
    "$GHC_LIBDIR_RAW/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR_RAW/../lib/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR_RAW/rts/include" \
    "$GHC_LIBDIR_RAW/include"; do
    if [[ -f "$cand/HsFFI.h" ]]; then
      SUPERS_CFLAGS="-O2 -fPIC -I$cand"
      break
    fi
  done
  [[ -z "$SUPERS_CFLAGS" ]] && SUPERS_CFLAGS="-O2 -fPIC"
fi

# Detect GHC shim
SHIM_DIR="$REPO/build/ghc-shim"
if [[ -d "$SHIM_DIR/rts" ]]; then
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

validate() {
  local label="$1" file="$2"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found in $file"
    cat "$file" 2>/dev/null || true
    exit 1
  fi
  if [[ "$got" != "1" ]]; then
    echo "FATAL: $label -> RESULT=$got (expected 1)"
    exit 1
  fi
  echo "  OK: RESULT=1"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Merge Sort Benchmark"
echo " NS=${NS[*]}"
echo " PS=${PS[*]}  reps=$REPS"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# N=$N  ($((N / 1000000))M)"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  INPUT_DIR="$NDIR/input"
  mkdir -p "$INPUT_DIR"
  echo "$N" > "$INPUT_DIR/params.txt"

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
    "$SDIR/ms_seq" +RTS -N1 -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "SEQ      N=$N rep=$rep -> ${secs}s"
    validate "SEQ N=$N rep=$rep" "$OUT"
    echo "seq,$N,1,$rep,$secs" >> "$CSV"
  done

  # ===== Build TALM supers (once per N) =====
  SUPERS_DIR="$NDIR/supers"
  if [[ ! -f "$SUPERS_DIR/libsupers.so" ]]; then
    mkdir -p "$SUPERS_DIR"
    echo "[sup ] building supers for N=$N ..."
    "$PY3" "$GEN_TALM" --out "$SUPERS_DIR/supers.hsk" --input-dir "$INPUT_DIR" --P 2
    CFLAGS="$SUPERS_CFLAGS" "$BUILD_SUPERS" "$SUPERS_DIR/supers.hsk" "$SUPERS_DIR/Supers.hs"
    echo "[sup ] built: $SUPERS_DIR/libsupers.so"
  fi

  # ===== Run parallel benchmarks =====
  for P in "${PS[@]}"; do
    echo ""
    echo "======== N=$N  P=$P ========"

    # --- TALM ---
    TDIR="$NDIR/talm_P${P}"
    mkdir -p "$TDIR/logs"

    "$PY3" "$GEN_TALM" --out "$TDIR/ms.hsk" --input-dir "$INPUT_DIR" --P "$P"
    "$CODEGEN" "$TDIR/ms.hsk" > "$TDIR/ms.fl" 2>"$TDIR/logs/codegen.err"

    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/ms" "$TDIR/ms.fl" >/dev/null 2>&1
    popd >/dev/null

    FLB="$TDIR/ms.flb"
    PLA="$TDIR/ms_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/ms.pla"
    LIBSUP="$SUPERS_DIR/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_r${rep}.txt"
      ERR="$TDIR/err_r${rep}.txt"
      SUPERS_FORCE_PAR=1 SUPERS_RTS_N="$P" \
        SUPERS_WORKER=0 SUPERS_WORKER_MAIN=0 \
        LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo NaN)"
      echo "TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT"
      echo "talm,$N,$P,$rep,$secs" >> "$CSV"
    done

    # --- par/pseq ---
    PPDIR="$NDIR/parpseq_P${P}"
    mkdir -p "$PPDIR/obj"
    "$PY3" "$GEN_PARPSEQ" --out "$PPDIR/ms.hs" --input-dir "$INPUT_DIR" --P "$P"
    "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
        -outputdir "$PPDIR/obj" -o "$PPDIR/ms" "$PPDIR/ms.hs" >/dev/null 2>&1

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PPDIR/out_r${rep}.txt"
      "$PPDIR/ms" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "PARPSEQ  N=$N P=$P rep=$rep -> ${secs}s"
      validate "PARPSEQ N=$N P=$P rep=$rep" "$OUT"
      echo "parpseq,$N,$P,$rep,$secs" >> "$CSV"
    done

    # --- Strategies ---
    STDIR="$NDIR/strat_P${P}"
    mkdir -p "$STDIR/obj"
    "$PY3" "$GEN_STRAT" --out "$STDIR/ms.hs" --input-dir "$INPUT_DIR" --P "$P"
    "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package deepseq \
        -outputdir "$STDIR/obj" -o "$STDIR/ms" "$STDIR/ms.hs" >/dev/null 2>&1

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$STDIR/out_r${rep}.txt"
      "$STDIR/ms" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "STRAT    N=$N P=$P rep=$rep -> ${secs}s"
      validate "STRAT N=$N P=$P rep=$rep" "$OUT"
      echo "strategies,$N,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
