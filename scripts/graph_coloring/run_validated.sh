#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Graph Coloring benchmark with file-IO data pipeline
#
# Each parallel chunk reads adj.bin via BS.readFile (GHC heap
# allocation), then greedy-colors its vertex range using
# association-list lookup.
#
# Variance reduction measures:
#   - Core pinning via taskset (physical cores only, no HT)
#   - Warmup run before measured reps (discarded)
#   - 5 measured reps (median is robust statistic)
#
# Structure:
#   - Pre-generate binary adj.bin per N
#   - Sequential baseline (P=1)
#   - Parallel: TALM, GHC Strategies, GHC par/pseq at each P
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
GEN_DATA="$REPO/scripts/graph_coloring/gen_graph_data.py"
GEN_TALM="$REPO/scripts/graph_coloring/gen_talm_input.py"
GEN_SEQ="$REPO/scripts/graph_coloring/gen_hs_sequential.py"
GEN_STRAT="$REPO/scripts/graph_coloring/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/graph_coloring/gen_hs_parpseq.py"

REPS=${REPS:-5}
WARMUP=${WARMUP:-1}
N_FUNCS=${N_FUNCS:-14}
EDGE_PROB=${EDGE_PROB:-0.001}
SEED=${SEED:-42}
PS=(${PS:-2 4 8 16})
TALM_RTS_A=${TALM_RTS_A:-64m}

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
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^COLORS=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no COLORS= found"
    exit 1
  fi
  if [[ "$got" != "$expected" ]]; then
    echo "FATAL: $label -> got COLORS=$got, expected $expected"
    exit 1
  fi
  echo "  OK: COLORS=$got (correct)"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,edge_prob,n_funcs,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Graph Coloring Data Pipeline Benchmark"
echo " N values: ${NS[*]}"
echo " edge_prob=$EDGE_PROB  seed=$SEED"
echo " N_FUNCS=$N_FUNCS  P=${PS[*]}"
echo " reps=$REPS  warmup=$WARMUP"
echo " core pinning: physical cores 0-$((N_PHYS_CORES-1))"
echo " TALM_RTS_A=$TALM_RTS_A"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################################"
  echo "# N=$N  (${N} vertices, edge_prob=$EDGE_PROB, N_FUNCS=$N_FUNCS)"
  echo "############################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # --- Generate binary adj.bin ---
  DATADIR="$NDIR/data"
  if [[ ! -f "$DATADIR/adj.bin" ]]; then
    "$PY3" "$GEN_DATA" --out-dir "$DATADIR" --N "$N" --edge-prob "$EDGE_PROB" --seed "$SEED"
  else
    echo "  [gen_data] reusing existing $DATADIR/adj.bin"
  fi

  # --- Build TALM ---
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/gc.hsk" --N "$N" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$CODEGEN" "$TDIR/gc.hsk" > "$TDIR/gc.fl" 2>/dev/null
  CFLAGS="$SUPERS_CFLAGS" SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
    SUPERS_GHC_PACKAGES="bytestring" \
    bash "$BUILD_SUPERS" "$TDIR/gc.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # --- Build GHC Sequential ---
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/gc.hs" --N "$N" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring \
      -outputdir "$SDIR/obj" -o "$SDIR/gc" "$SDIR/gc.hs" >/dev/null 2>&1

  # --- Build GHC Strategies ---
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/gc.hs" --N "$N" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package bytestring \
      -outputdir "$GDIR/obj" -o "$GDIR/gc" "$GDIR/gc.hs" >/dev/null 2>&1

  # --- Build GHC par/pseq ---
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/gc.hs" --N "$N" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring \
      -outputdir "$PDIR/obj" -o "$PDIR/gc" "$PDIR/gc.hs" >/dev/null 2>&1

  echo "  Built all variants for N=$N"

  # ===== Sequential baseline (P=1) =====
  echo ""
  echo "  ---- Sequential Baseline (P=1) ----"
  EXPECTED=""
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
    if [[ -z "$EXPECTED" ]]; then
      EXPECTED="$colors"
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s  (reference COLORS=$EXPECTED)"
    else
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s"
      validate "Sequential N=$N P=1 rep=$rep" "$OUT" "$EXPECTED"
    fi
    echo "seq,$N,$EDGE_PROB,$N_FUNCS,1,$rep,$secs" >> "$CSV"
  done

  # ===== Parallel benchmarks (P>=2) =====
  for P in "${PS[@]}"; do
    echo ""
    echo "  ---- P=$P ----"
    CORES_P="$(pin_cores "$P")"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/gc_P${P}" "$TDIR/gc.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/gc_P${P}.flb"
    PLA="$TDIR/gc_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/gc_P${P}.pla"

    # TALM warmup
    for ((w=1; w<=WARMUP; w++)); do
      echo "  TALM     N=$N P=$P warmup=$w (discarded)"
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >/dev/null 2>/dev/null
      set -e
    done

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
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
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$EDGE_PROB,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
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
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "strat,$N,$EDGE_PROB,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
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
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$EDGE_PROB,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
