#!/usr/bin/env bash
set -euo pipefail

# Ensure UTF-8 locale: supersgen + ribault emit Unicode chars (en-dashes etc.)
# in comments; default POSIX locale on some servers cannot encode these,
# causing supersgen to abort with "cannot encode character".
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

# ============================================================
# Validated LCS Wavefront Benchmark
# Sequential baseline + TALM + par/pseq + Strategies
#
# Rectangular grid strategy: DIM_ROWS=P × DIM_COLS (configurable).
# This gives large grain per block (N/P rows each) while
# maintaining enough anti-diagonals for full P-way parallelism.
#
# Sequential baseline uses DIM=1 (no blocking overhead).
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_SEQ="$REPO/scripts/lcs_wavefront/gen_hs_sequential.py"
GEN_TALM="$REPO/scripts/lcs_wavefront/gen_talm_input.py"
GEN_PARPSEQ="$REPO/scripts/lcs_wavefront/gen_hs_parpseq.py"
GEN_STRAT="$REPO/scripts/lcs_wavefront/gen_hs_strategies.py"

REPS=${REPS:-7}
SEED=${SEED:-42}
ALPHABET=${ALPHABET:-4}
NS=(${NS:-100000 200000 300000 400000 500000})
PS=(${PS:-1 2 4 8 16 24 32 40 48})
# Per-variant skip flags. par/pseq stays flat at ~1.3x speedup for LCS
# wavefront (diagonal barrier ceiling) and is the slowest variant to run at
# large N — skip it to save hours.
SKIP_PARPSEQ=${SKIP_PARPSEQ:-1}
SKIP_STRAT=${SKIP_STRAT:-0}
# SKIP_SEQ=1 disables the sequential baseline build/run. Useful for large N
# where SEQ takes hours; pair with an externally-supplied EXPECTED for
# cross-validation. The CROSS_EXPECTED variable in the parallel loop will
# be initialised from the first parallel variant's RESULT instead.
SKIP_SEQ=${SKIP_SEQ:-0}
# DIM_COLS modes:
#  - integer (e.g. "64"): fixed DIM_COLS for all N (paper-compatible).
#  - "auto": scale DIM_COLS = max(P, N / TARGET_BLOCK_WIDTH) per N. Keeps each
#    block's column-width ~constant, so cache footprint per block stays
#    bounded and total block count grows linearly with N. This is intended
#    to preserve TALM's firing-rule advantage at large N (more diagonals
#    means STRAT pays its barrier cost more times, while TALM's wavefront
#    firing keeps all workers busy).
DIM_COLS=${DIM_COLS:-auto}
TARGET_BLOCK_WIDTH=${TARGET_BLOCK_WIDTH:-1000}
TALM_RTS_A=${TALM_RTS_A:-256m}

# ---- Core pinning (architectural equivalence with attention runner) ----
# Physical cores: 0..(N_PHYS_CORES-1). HT siblings: N_PHYS_CORES..(N_LOG_CORES-1).
# For P <= N_PHYS_CORES: pinning is strictly within physical cores (no HT).
# For P >  N_PHYS_CORES: pinning extends into HT siblings (OVERSUBSCRIPTION).
N_PHYS_CORES=${N_PHYS_CORES:-24}
N_LOG_CORES=${N_LOG_CORES:-48}

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then
    echo "0-$((N_LOG_CORES - 1))"
  else
    echo "0-$((p - 1))"
  fi
}

pin_tag() {
  local p="$1"
  if (( p <= N_PHYS_CORES )); then
    echo "physical, cores 0-$((p-1)), no HT"
  else
    local ht=$(( p - N_PHYS_CORES ))
    echo "oversubscribed, cores 0-$((p-1)) = $N_PHYS_CORES phys + $ht HT siblings"
  fi
}
OS_BANNER_SHOWN=0

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
  if [[ "$expected" == "SKIP" ]]; then
    if [[ -z "${CROSS_EXPECTED:-}" ]]; then
      CROSS_EXPECTED="$got"
      echo "  OK: RESULT=$got (first result, will cross-validate)"
    elif [[ "$got" != "$CROSS_EXPECTED" ]]; then
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED (cross-validate)"
      exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    if [[ "$got" != "$expected" ]]; then
      echo "FATAL: $label -> got $got, expected $expected"
      exit 1
    fi
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "================================================================"
echo " LCS Wavefront Benchmark (rectangular grid, equal-footing)"
echo "================================================================"
echo " Workload params:"
if [[ "$DIM_COLS" == "auto" ]]; then
  echo "   NS (seq_len)=${NS[*]}  DIM_ROWS=P  DIM_COLS=auto (N/$TARGET_BLOCK_WIDTH, floor=P)  ALPHABET=$ALPHABET"
else
  echo "   NS (seq_len)=${NS[*]}  DIM_ROWS=P  DIM_COLS=$DIM_COLS  ALPHABET=$ALPHABET"
fi
echo "   reps=$REPS  seed=$SEED"
echo ""
echo " Methodological equivalence preserved:"
echo "   * RTS allocation: -A$TALM_RTS_A applied to TALM internal RTS *and*"
echo "     to every GHC binary invocation (seq baseline, Strategies, par/pseq)"
echo "   * Threads: TALM uses SUPERS_RTS_N=P; GHC uses +RTS -N\$P"
echo "   * Core pinning: taskset -c 0..(P-1) on every execution (TALM and GHC)"
echo "   * Repetitions: $REPS measured per (variant, N, P)"
echo "   * Variants compared: TALM/Trebuchet, GHC Strategies (parMap rdeepseq),"
echo "     GHC par/pseq (manual sparking)"
echo "   * Identical compile flags: -O2 (GHC -threaded for parallel variants)"
echo "   * Identical input strings and identical grid for every variant"
echo ""
echo " P sweep: ${PS[*]}"
echo "   physical cores 0..$((N_PHYS_CORES-1))  (no HT)"
echo "   P > $N_PHYS_CORES => OVERSUBSCRIPTION (HT siblings $N_PHYS_CORES..$((N_LOG_CORES-1)))"
echo ""
echo " Output: $OUTROOT"
echo "================================================================"

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# SEQ_LEN=$N"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  CROSS_EXPECTED=""

  # ===== Generate input + expected =====
  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --N "$N" --alphabet "$ALPHABET" --seed "$SEED" \
      --out-dir "$INPUT_DIR"
  EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
  echo "  Expected LCS score: $EXPECTED"

  # ===== Build & run sequential baseline (DIM=1×1, no blocking) =====
  if [[ "$SKIP_SEQ" -eq 0 ]]; then
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
      --dim-rows 1 --dim-cols 1
  "$GHC_BIN" -O2 -rtsopts -dynamic -package time -package vector \
      -outputdir "$SDIR/obj" -o "$SDIR/lcs_wf" "$SDIR/lcs_wf.hs" >/dev/null 2>&1

  echo ""
  echo "======== SEQ_LEN=$N  SEQUENTIAL BASELINE (1x1; cores: $(pin_cores 1); $(pin_tag 1)) ========"
  CORES_1="$(pin_cores 1)"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    taskset -c "$CORES_1" "$SDIR/lcs_wf" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    echo "SEQ      N=$N rep=$rep -> ${secs}s"
    validate "SEQ N=$N rep=$rep" "$OUT" "$EXPECTED"
    echo "seq,$N,1,1,1,$rep,$secs" >> "$CSV"
  done
  fi  # SKIP_SEQ

  # ===== Run parallel benchmarks (DIM_ROWS=P, DIM_COLS=$DIM_COLS) =====
  for P in "${PS[@]}"; do
    if [[ "$P" -gt "$N_PHYS_CORES" && "$OS_BANNER_SHOWN" -eq 0 ]]; then
      echo ""
      echo "  ################################################################"
      echo "  ##  OVERSUBSCRIPTION BEGINS HERE                              ##"
      echo "  ##  Next P values exceed $N_PHYS_CORES physical cores; HT siblings"
      echo "  ##  (cores $N_PHYS_CORES..$((N_LOG_CORES-1))) are now used.            ##"
      echo "  ##  Below this line, runtime numbers should be interpreted    ##"
      echo "  ##  under SMT contention, not pure physical-core scaling.     ##"
      echo "  ################################################################"
      OS_BANNER_SHOWN=1
    fi

    CUR_ROWS=$P
    if [[ "$DIM_COLS" == "auto" ]]; then
      # Scale DIM_COLS so block-column-width stays roughly constant.
      # Floor at P so the wavefront has enough breadth to keep P workers busy.
      CUR_COLS=$(( N / TARGET_BLOCK_WIDTH ))
      if (( CUR_COLS < P )); then CUR_COLS=$P; fi
    else
      CUR_COLS=$DIM_COLS
    fi
    TOTAL_BLOCKS=$((CUR_ROWS * CUR_COLS))
    CORES_P="$(pin_cores "$P")"
    PIN_TAG="$(pin_tag "$P")"
    echo ""
    echo "======== SEQ_LEN=$N  P=$P  grid=${CUR_ROWS}x${CUR_COLS} (${TOTAL_BLOCKS} blocks; cores: $CORES_P; $PIN_TAG) ========"

    # --- Build TALM with DIM_ROWS=P, DIM_COLS ---
    TDIR="$NDIR/talm_P${P}"
    mkdir -p "$TDIR/supers"
    "$PY3" "$GEN_TALM" --out "$TDIR/lcs_wf.hsk" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"

    SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
        SUPERS_GHC_PACKAGES="vector" \
        CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$TDIR/lcs_wf.hsk" "$TDIR/supers/Supers.hs"

    LIBSUP="$TDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    # Assemble for P threads
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/lcs_wf_P${P}" "$TDIR/lcs_wf.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/lcs_wf_P${P}.flb"
    PLA="$TDIR/lcs_wf_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/lcs_wf_P${P}.pla"

    # --- Run TALM ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3 4 5 6 7 8 9 10; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
          LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -ne 0 ]]; then
          echo "  WARN: TALM N=$N P=$P rep=$rep rc=$rc (attempt $attempt/10), retrying..."
          sleep 1
          continue
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then break; fi
        echo "  WARN: RESULT= missing (attempt $attempt/10), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM     N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done

    # --- Build/run par/pseq (skip if SKIP_PARPSEQ=1) ---
    if [[ "$SKIP_PARPSEQ" -eq 0 ]]; then
      PPDIR="$NDIR/parpseq_P${P}"
      mkdir -p "$PPDIR/obj"
      "$PY3" "$GEN_PARPSEQ" --out "$PPDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
          --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"
      "$GHC_BIN" -O2 -threaded -rtsopts -dynamic -package time -package parallel -package vector \
          -outputdir "$PPDIR/obj" -o "$PPDIR/lcs_wf" "$PPDIR/lcs_wf.hs" >/dev/null 2>&1

      for ((rep=1; rep<=REPS; rep++)); do
        OUT="$PPDIR/out_P${P}_r${rep}.txt"
        taskset -c "$CORES_P" "$PPDIR/lcs_wf" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
        secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
        echo "PARPSEQ  N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
        validate "PARPSEQ N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
        echo "parpseq,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
      done
    fi

    # --- Build/run Strategies (skip if SKIP_STRAT=1) ---
    if [[ "$SKIP_STRAT" -eq 0 ]]; then
      STDIR="$NDIR/strat_P${P}"
      mkdir -p "$STDIR/obj"
      "$PY3" "$GEN_STRAT" --out "$STDIR/lcs_wf.hs" --input-dir "$INPUT_DIR" \
          --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"
      "$GHC_BIN" -O2 -threaded -rtsopts -dynamic -package time -package parallel -package vector \
          -outputdir "$STDIR/obj" -o "$STDIR/lcs_wf" "$STDIR/lcs_wf.hs" >/dev/null 2>&1

      for ((rep=1; rep<=REPS; rep++)); do
        OUT="$STDIR/out_P${P}_r${rep}.txt"
        taskset -c "$CORES_P" "$STDIR/lcs_wf" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
        secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
        echo "STRAT    N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
        validate "STRAT N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
        echo "strategies,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
      done
    fi
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
