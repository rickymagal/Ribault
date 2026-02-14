#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated N-Queens benchmark: TALM + GHC Strategies + GHC par/pseq
# Verifies EVERY result before proceeding. Aborts on wrong answer.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/nqueens}"
mkdir -p "$OUTROOT"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_TALM="$REPO/scripts/nqueens/gen_talm_input.py"
GEN_STRAT="$REPO/scripts/nqueens/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/nqueens/gen_hs_parpseq.py"

OVERSUB=4
REPS=3
NS=(2 4 8 14)
PS=(1 2 4 8)

# Expected answers
declare -A EXPECTED
EXPECTED[2]=0; EXPECTED[4]=2; EXPECTED[8]=92; EXPECTED[14]=365596

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

# Compute cutoff
compute_cutoff() {
  "$PY3" -c "
import math
N, P, K = $1, $2, $OVERSUB
target = K * max(P, 1)
print(max(1, math.ceil(math.log(target) / math.log(N)))) if N > 1 else print(1)
"
}

# Validate result: extracts RESULT= from file, compares, prints, aborts on mismatch
validate() {
  local label="$1" file="$2" N="$3"
  local exp="${EXPECTED[$N]}"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found"
    exit 1
  fi
  if [[ "$got" != "$exp" ]]; then
    echo "FATAL: $label -> got $got, expected $exp"
    exit 1
  fi
  echo "  OK: RESULT=$got (correct)"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,cutoff,P,rep,seconds" > "$CSV"

echo "========================================"
echo " N-Queens Benchmark (validated)"
echo " N=${NS[*]}  P=${PS[*]}  reps=$REPS"
echo " Output: $OUTROOT"
echo "========================================"

# Track built artifacts to avoid rebuilding
declare -A TALM_BUILT  # key=N_C -> dir
declare -A GHC_BUILT   # key=variant_N_C -> binary path

for N in "${NS[@]}"; do
  echo ""
  echo "======== N=$N (expected=${EXPECTED[$N]}) ========"
  for P in "${PS[@]}"; do
    CUTOFF=$(compute_cutoff "$N" "$P")
    echo ""
    echo "--- N=$N P=$P cutoff=$CUTOFF ---"

    # ===== TALM =====
    TKEY="${N}_${CUTOFF}"
    TDIR="$OUTROOT/talm/N${N}_C${CUTOFF}"
    if [[ -z "${TALM_BUILT[$TKEY]:-}" ]]; then
      mkdir -p "$TDIR/supers"
      "$PY3" "$GEN_TALM" --out "$TDIR/nq.hsk" --N "$N" --cutoff "$CUTOFF" 2>/dev/null
      "$CODEGEN" "$TDIR/nq.hsk" > "$TDIR/nq.fl" 2>/dev/null
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/nq.hsk" "$TDIR/supers/Supers.hs" >/dev/null 2>&1
      TALM_BUILT[$TKEY]="$TDIR"
    fi
    TDIR="${TALM_BUILT[$TKEY]}"

    # Assemble for this P
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/nq_P${P}" "$TDIR/nq.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/nq_P${P}.flb"
    PLA="$TDIR/nq_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/nq_P${P}.pla"
    LIBSUP="$TDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      # Retry up to 3 times if RESULT= missing (unsafePerformIO flush race)
      for attempt in 1 2 3; do
        set +e
        LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -ne 0 ]]; then
          echo "FATAL: TALM N=$N P=$P rep=$rep rc=$rc"
          exit 1
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then
          break
        fi
        echo "  WARN: RESULT= missing (attempt $attempt/3), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$N"
      echo "super,$N,$CUTOFF,$P,$rep,$secs" >> "$CSV"
    done

    # ===== GHC Strategies =====
    GKEY="strat_${N}_${CUTOFF}"
    GDIR="$OUTROOT/ghc/N${N}_C${CUTOFF}"
    if [[ -z "${GHC_BUILT[$GKEY]:-}" ]]; then
      mkdir -p "$GDIR/obj"
      "$PY3" "$GEN_STRAT" --out "$GDIR/nq.hs" --N "$N" --cutoff "$CUTOFF" 2>/dev/null
      "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
        -outputdir "$GDIR/obj" -o "$GDIR/nq" "$GDIR/nq.hs" >/dev/null 2>&1
      GHC_BUILT[$GKEY]="$GDIR/nq"
    fi
    GBIN="${GHC_BUILT[$GKEY]}"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GBIN" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-Str  N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT" "$N"
      echo "ghc,$N,$CUTOFF,$P,$rep,$secs" >> "$CSV"
    done

    # ===== GHC par/pseq =====
    PKEY="parpseq_${N}_${CUTOFF}"
    PDIR="$OUTROOT/parpseq/N${N}_C${CUTOFF}"
    if [[ -z "${GHC_BUILT[$PKEY]:-}" ]]; then
      mkdir -p "$PDIR/obj"
      "$PY3" "$GEN_PARPSEQ" --out "$PDIR/nq.hs" --N "$N" --cutoff "$CUTOFF" 2>/dev/null
      "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
        -outputdir "$PDIR/obj" -o "$PDIR/nq" "$PDIR/nq.hs" >/dev/null 2>&1
      GHC_BUILT[$PKEY]="$PDIR/nq"
    fi
    PBIN="${GHC_BUILT[$PKEY]}"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PBIN" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-PP   N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT" "$N"
      echo "parpseq,$N,$CUTOFF,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================"
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================"
