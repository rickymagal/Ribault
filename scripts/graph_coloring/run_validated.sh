#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Graph Coloring benchmark (LCG-hash pure computation)
#
# Each chunk colors its vertex range using on-the-fly LCG edge
# testing + association-list greedy coloring. Pure compute, no IO.
#
# Variance reduction measures:
#   - Core pinning via taskset (physical cores only, no HT)
#   - Warmup run before measured reps (discarded)
#   - Multiple measured reps (median is robust statistic)
#
# Structure:
#   - Sequential baseline (GHC Strategies P=1)
#   - Parallel: TALM, GHC Strategies, GHC par/pseq at each P
#   - GHC binaries rebuilt per P (chunk count = P, hardcoded)
#   - TALM .hsk rebuilt per P (chunk supers = P), supers shared
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/graph_coloring}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_TALM="$REPO/scripts/graph_coloring/gen_graph_input.py"
GEN_STRAT="$REPO/scripts/graph_coloring/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/graph_coloring/gen_hs_parpseq.py"

REPS=${REPS:-5}
WARMUP=${WARMUP:-1}
EDGE_PROB=${EDGE_PROB:-0.001}
SEED=${SEED:-42}
PS=(${PS:-2 4 8 16})
TALM_RTS_A=${TALM_RTS_A:-64m}
SUPERS_RTS_N_OVERRIDE=${SUPERS_RTS_N:-}

NS=(${NS:-1000 1500 2000})

# ---- Core pinning ----
N_PHYS_CORES=${N_PHYS_CORES:-24}

pin_cores() {
  local p="$1"
  if (( p > N_PHYS_CORES )); then
    echo "0-$((N_PHYS_CORES - 1))"
  else
    echo "0-$((p - 1))"
  fi
}

# Detect GHC
GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir)"

# GHC packages for strategies/parpseq (containers, parallel, deepseq, time)
# Detect cabal package-db for 'parallel' package
CABAL_PKG_DB=""
for cand in \
  "$HOME/.cabal/store/ghc-${GHC_VER}/package.db" \
  "/home/$USER/.cabal/store/ghc-${GHC_VER}/package.db"; do
  if [[ -d "$cand" ]]; then
    CABAL_PKG_DB="-package-db $cand"
    break
  fi
done
GHC_PKG_FLAGS="${GHC_PKG_FLAGS:-$CABAL_PKG_DB -package time -package parallel -package deepseq -package containers}"

# Detect HsFFI include for supers build
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
  local label="$1" file="$2"
  local colors valid
  colors="$(awk -F= '/^COLORS=/{print $2}' "$file" 2>/dev/null || true)"
  valid="$(awk -F= '/^VALID=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$colors" ]]; then
    echo "FATAL: $label -> no COLORS= found"
    exit 1
  fi
  if [[ "$valid" != "True" ]]; then
    echo "FATAL: $label -> VALID=$valid (expected True)"
    exit 1
  fi
  echo "  OK: COLORS=$colors VALID=True"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,edge_prob,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Graph Coloring Benchmark (LCG pure compute)"
echo " N values: ${NS[*]}"
echo " edge_prob=$EDGE_PROB  seed=$SEED"
echo " P=${PS[*]}"
echo " reps=$REPS  warmup=$WARMUP"
echo " core pinning: physical cores 0-$((N_PHYS_CORES-1))"
echo " TALM_RTS_A=$TALM_RTS_A  SUPERS_RTS_N=${SUPERS_RTS_N_OVERRIDE:-P}"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################################"
  echo "# N=$N  (${N} vertices, edge_prob=$EDGE_PROB)"
  echo "############################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # --- Build TALM supers once per N (shared across all P) ---
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  # Generate a representative .hsk (P=1) for supers extraction
  "$PY3" "$GEN_TALM" --out "$TDIR/representative.hsk" --N "$N" --P 1 --edge-prob "$EDGE_PROB" --seed "$SEED"
  CFLAGS="$SUPERS_CFLAGS" \
    bash "$BUILD_SUPERS" "$TDIR/representative.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # --- Build GHC Sequential (Strategies with P=1, single chunk) ---
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$SDIR/gc.hs" --N "$N" --P 1 --edge-prob "$EDGE_PROB" --seed "$SEED"
  GHC_ENVIRONMENT=- "$GHC_BIN" $GHC_PKG_FLAGS -O2 -threaded -rtsopts \
      -outputdir "$SDIR/obj" -o "$SDIR/gc" "$SDIR/gc.hs" >/dev/null 2>&1

  echo "  Built supers + sequential baseline for N=$N"

  # ===== Sequential baseline (P=1) =====
  echo ""
  echo "  ---- Sequential Baseline (P=1) ----"
  CORES_1="$(pin_cores 1)"

  # Warmup
  for ((w=1; w<=WARMUP; w++)); do
    echo "  SEQ      N=$N P=1 warmup=$w (discarded)"
    taskset -c "$CORES_1" "$SDIR/gc" +RTS -N1 -RTS >/dev/null 2>/dev/null
  done

  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    taskset -c "$CORES_1" "$SDIR/gc" +RTS -N1 -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    colors="$(awk -F= '/^COLORS=/{print $2}' "$OUT")"
    echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s  COLORS=$colors"
    validate "Sequential N=$N P=1 rep=$rep" "$OUT"
    echo "seq,$N,$EDGE_PROB,1,$rep,$secs" >> "$CSV"
  done

  # ===== Parallel benchmarks (P>=2) =====
  for P in "${PS[@]}"; do
    echo ""
    echo "  ---- P=$P ----"
    CORES_P="$(pin_cores "$P")"

    # --- Build TALM for this P ---
    "$PY3" "$GEN_TALM" --out "$TDIR/gc_P${P}.hsk" --N "$N" --P "$P" --edge-prob "$EDGE_PROB" --seed "$SEED"
    "$CODEGEN" "$TDIR/gc_P${P}.hsk" > "$TDIR/gc_P${P}.fl" 2>/dev/null
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/gc_P${P}" "$TDIR/gc_P${P}.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/gc_P${P}.flb"
    PLA="$TDIR/gc_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/gc_P${P}.pla"

    # --- Build GHC Strategies for this P ---
    GDIR="$NDIR/ghc_P${P}"
    mkdir -p "$GDIR/obj"
    "$PY3" "$GEN_STRAT" --out "$GDIR/gc.hs" --N "$N" --P "$P" --edge-prob "$EDGE_PROB" --seed "$SEED"
    GHC_ENVIRONMENT=- "$GHC_BIN" $GHC_PKG_FLAGS -O2 -threaded -rtsopts \
        -outputdir "$GDIR/obj" -o "$GDIR/gc" "$GDIR/gc.hs" >/dev/null 2>&1

    # --- Build GHC par/pseq for this P ---
    PDIR="$NDIR/parpseq_P${P}"
    mkdir -p "$PDIR/obj"
    "$PY3" "$GEN_PARPSEQ" --out "$PDIR/gc.hs" --N "$N" --P "$P" --edge-prob "$EDGE_PROB" --seed "$SEED"
    GHC_ENVIRONMENT=- "$GHC_BIN" $GHC_PKG_FLAGS -O2 -threaded -rtsopts \
        -outputdir "$PDIR/obj" -o "$PDIR/gc" "$PDIR/gc.hs" >/dev/null 2>&1

    echo "  Built all variants for N=$N P=$P"

    # --- TALM ---
    # TALM warmup
    for ((w=1; w<=WARMUP; w++)); do
      echo "  TALM     N=$N P=$P warmup=$w (discarded)"
      set +e
      SUPERS_RTS_N="${SUPERS_RTS_N_OVERRIDE:-$P}" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >/dev/null 2>/dev/null
      set -e
    done

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_N="${SUPERS_RTS_N_OVERRIDE:-$P}" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -eq 0 ]]; then
          got="$(awk -F= '/^COLORS=/{print $2}' "$OUT" 2>/dev/null || true)"
          if [[ -n "$got" ]]; then break; fi
        fi
        echo "    WARN: TALM N=$N P=$P rep=$rep attempt=$attempt failed (rc=$rc)"
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "  TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT"
      echo "super,$N,$EDGE_PROB,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((w=1; w<=WARMUP; w++)); do
      echo "  GHC-Str  N=$N P=$P warmup=$w (discarded)"
      taskset -c "$CORES_P" "$GDIR/gc" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
    done

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$GDIR/gc" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-Str  N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT"
      echo "strat,$N,$EDGE_PROB,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((w=1; w<=WARMUP; w++)); do
      echo "  GHC-PP   N=$N P=$P warmup=$w (discarded)"
      taskset -c "$CORES_P" "$PDIR/gc" +RTS -N"$P" -RTS >/dev/null 2>/dev/null
    done

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$PDIR/gc" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-PP   N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT"
      echo "parpseq,$N,$EDGE_PROB,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
