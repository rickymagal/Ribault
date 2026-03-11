#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# 0/1 Knapsack via parallel merge-sort aggregation
# TALM + GHC Strategies + GHC par/pseq
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/subset_sum}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_ITEMS="$REPO/scripts/knapsack/gen_items.py"
GEN_TALM="$REPO/scripts/subset_sum/gen_talm_input.py"
GEN_STRAT="$REPO/scripts/subset_sum/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/subset_sum/gen_hs_parpseq.py"

REPS=${REPS:-3}
SEED=${SEED:-42}
NPARTS=${NPARTS:-64}
NS=(${NS:-21})
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
echo "variant,n_items,capacity,P,rep,seconds" > "$CSV"

echo "========================================="
echo " 0/1 Knapsack (merge-sort aggregation)"
echo " NS=${NS[*]}"
echo " PS=${PS[*]}  reps=$REPS  seed=$SEED  nparts=$NPARTS"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  TOTAL=$((2**N))
  echo ""
  echo "############################################"
  echo "# N_ITEMS=$N  (2^$N = $TOTAL subsets to sort)"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # ===== Generate items =====
  ITEMS_DIR="$NDIR/items"
  "$PY3" "$GEN_ITEMS" --n-items "$N" --seed "$SEED" --out-dir "$ITEMS_DIR"
  EXPECTED="$(cat "$ITEMS_DIR/expected.txt")"
  CAPACITY="$(cat "$ITEMS_DIR/capacity.txt")"
  echo "  Expected optimal value: $EXPECTED  Capacity: $CAPACITY"

  # ===== Build TALM =====
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/ss.hsk" --items-dir "$ITEMS_DIR" \
      --n-items "$N" --nparts "$NPARTS"
  DF_LIST_BUILTIN=1 "$CODEGEN" "$TDIR/ss.hsk" > "$TDIR/ss.fl" 2>/dev/null

  INJECT_FILE="$TDIR/supers_inject.hs"
  DF_LIST_BUILTIN=1 SUPERS_INJECT_FILE="$INJECT_FILE" \
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/ss.hsk" "$TDIR/supers/Supers.hs"

  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # ===== Build GHC Strategies =====
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/ss.hs" --items-dir "$ITEMS_DIR" \
      --n-items "$N" --nparts "$NPARTS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
      -package deepseq -package array \
      -outputdir "$GDIR/obj" -o "$GDIR/ss" "$GDIR/ss.hs" >/dev/null 2>&1

  # ===== Build GHC par/pseq =====
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/ss.hs" --items-dir "$ITEMS_DIR" \
      --n-items "$N" --nparts "$NPARTS"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel \
      -package deepseq -package array \
      -outputdir "$PDIR/obj" -o "$PDIR/ss" "$PDIR/ss.hs" >/dev/null 2>&1

  # ===== Run benchmarks =====
  for P in "${PS[@]}"; do
    echo ""
    echo "======== N_ITEMS=$N  P=$P ========"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      DF_LIST_BUILTIN=1 "$PY3" assembler.py -a -n "$P" -o "$TDIR/ss_P${P}" "$TDIR/ss.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/ss_P${P}.flb"
    PLA="$TDIR/ss_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/ss_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        DF_LIST_BUILTIN=1 SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
          LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -ne 0 ]]; then
          echo "  WARN: TALM N=$N P=$P rep=$rep rc=$rc (attempt $attempt/3), retrying..."
          sleep 1
          continue
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then break; fi
        echo "  WARN: RESULT= missing (attempt $attempt/3), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$CAPACITY,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/ss" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-Str  N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "ghc,$N,$CAPACITY,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PDIR/ss" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "GHC-PP   N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$CAPACITY,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
