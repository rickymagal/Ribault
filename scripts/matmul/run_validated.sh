#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated MatMul benchmark: TALM + GHC Strategies + GHC par/pseq
#
# Structure:
#   - Sequential baseline (P=1, GHC -N1): same for all variants
#   - Parallel strategies (P>=2): TALM, GHC Strategies, GHC par/pseq
#
# All variants use on-the-fly LCG matrix generation (no pre-stored
# matrices), list-comprehension dot products, and per-block truncation.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/matmul}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_TALM="$REPO/scripts/matmul/gen_talm_input.py"
GEN_SEQ="$REPO/scripts/matmul/gen_hs_sequential.py"
GEN_STRAT="$REPO/scripts/matmul/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/matmul/gen_hs_parpseq.py"

REPS=${REPS:-3}
N_FUNCS=${N_FUNCS:-14}
PS=(${PS:-2 4 8 16})
TALM_RTS_A=${TALM_RTS_A:-256m}

# NS: space-separated list of N values
NS=(${NS:-512 768 1024})

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
  got="$(awk -F= '/^CHECKSUM=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no CHECKSUM= found"
    exit 1
  fi
  if [[ "$got" != "$expected" ]]; then
    echo "FATAL: $label -> got $got, expected $expected"
    exit 1
  fi
  echo "  OK: CHECKSUM=$got (correct)"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,n_funcs,P,rep,seconds" > "$CSV"

echo "========================================="
echo " MatMul Benchmark (validated)"
echo " N values: ${NS[*]}"
echo " N_FUNCS=$N_FUNCS  P=${PS[*]}  reps=$REPS"
echo " TALM_RTS_A=$TALM_RTS_A"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################################"
  echo "# N=$N  (${N}x${N} matrix, N_FUNCS=$N_FUNCS)"
  echo "############################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # --- Build TALM ---
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/mm.hsk" --N "$N" --n-funcs "$N_FUNCS"
  "$CODEGEN" "$TDIR/mm.hsk" > "$TDIR/mm.fl" 2>/dev/null
  CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/mm.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # --- Build GHC Sequential ---
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/mm.hs" --N "$N" --n-funcs "$N_FUNCS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time \
      -outputdir "$SDIR/obj" -o "$SDIR/mm" "$SDIR/mm.hs" >/dev/null 2>&1

  # --- Build GHC Strategies ---
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/mm.hs" --N "$N" --n-funcs "$N_FUNCS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
      -outputdir "$GDIR/obj" -o "$GDIR/mm" "$GDIR/mm.hs" >/dev/null 2>&1

  # --- Build GHC par/pseq ---
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/mm.hs" --N "$N" --n-funcs "$N_FUNCS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time \
      -outputdir "$PDIR/obj" -o "$PDIR/mm" "$PDIR/mm.hs" >/dev/null 2>&1

  echo "  Built all variants for N=$N"

  # ===== Sequential baseline (P=1) =====
  echo ""
  echo "  ---- Sequential Baseline (P=1) ----"
  EXPECTED=""
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    "$SDIR/mm" +RTS -N1 -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT")"
    if [[ -z "$EXPECTED" ]]; then
      EXPECTED="$cs"
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s  (reference checksum: $EXPECTED)"
    else
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s"
      validate "Sequential N=$N P=1 rep=$rep" "$OUT" "$EXPECTED"
    fi
    echo "seq,$N,$N_FUNCS,1,$rep,$secs" >> "$CSV"
  done

  # ===== Parallel benchmarks (P>=2) =====
  for P in "${PS[@]}"; do
    echo ""
    echo "  ---- P=$P ----"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/mm_P${P}" "$TDIR/mm.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/mm_P${P}.flb"
    PLA="$TDIR/mm_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/mm_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -eq 0 ]]; then
          got="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || true)"
          if [[ -n "$got" ]]; then break; fi
        fi
        echo "    WARN: TALM N=$N P=$P rep=$rep attempt=$attempt failed (rc=$rc)"
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "  TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/mm" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-Str  N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "strat,$N,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PDIR/mm" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-PP   N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
