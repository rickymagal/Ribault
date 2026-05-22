#!/usr/bin/env bash
set -euo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

# ============================================================
# LCS Wavefront — Ribault + Rust super sweep (TALM-Rust)
#
# Same Trebuchet runtime as TALM-Hs / TALM-C; same dataflow
# graph (.fl) as gen_talm_input.py auto-DIM_COLS; but the leaf
# compute (lcsBlock) is implemented in Rust and linked into
# libsupers.so via the same supers_wrappers.c shell that
# TALM-C uses. This pairs naturally against Timely Dataflow
# (both are Rust per-block compute; the difference is the
# scheduler — Trebuchet's firing rule vs Timely's frontier
# epochs).
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront_ribault_rust}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_RS="$REPO/scripts/lcs_wavefront/gen_lcs_rust.py"
BUILD_RS="$REPO/tools/build_supers_rust.sh"

REPS=${REPS:-7}
SEED=${SEED:-42}
ALPHABET=${ALPHABET:-4}
NS=(${NS:-200000})
PS=(${PS:-1 2 4 8 16 24 32 40 48})
DIM_COLS=${DIM_COLS:-auto}
TARGET_BLOCK_WIDTH=${TARGET_BLOCK_WIDTH:-1000}
TALM_RTS_A=${TALM_RTS_A:-256m}

N_PHYS_CORES=${N_PHYS_CORES:-24}
N_LOG_CORES=${N_LOG_CORES:-48}

# Shared cargo target dir for Rust supers (keyed by N/grid below) to amortize
# the dependency build (libc, syn, etc.) across runs.
CARGO_TARGET_BASE="${CARGO_TARGET_BASE:-$HOME/.cache/cargo-target-talm-rust}"
mkdir -p "$CARGO_TARGET_BASE"

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"; else echo "0-$((p - 1))"; fi
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

validate() {
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^RESULT=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no RESULT= found in $file"; cat "$file" 2>/dev/null || true; exit 1
  fi
  if [[ "$expected" == "SKIP" ]]; then
    if [[ -z "${CROSS_EXPECTED:-}" ]]; then
      CROSS_EXPECTED="$got"; echo "  OK: RESULT=$got (first result, will cross-validate)"
    elif [[ "$got" != "$CROSS_EXPECTED" ]]; then
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED (cross-validate)"; exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    if [[ "$got" != "$expected" ]]; then
      echo "FATAL: $label -> got $got, expected $expected"; exit 1
    fi
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "================================================================"
echo " LCS Wavefront — Ribault + Rust super (TALM-Rust)"
echo "================================================================"
echo "   NS=${NS[*]}  DIM_ROWS=P  DIM_COLS=$DIM_COLS  ALPHABET=$ALPHABET"
echo "   reps=$REPS  seed=$SEED"
echo " P sweep: ${PS[*]}"
echo "================================================================"

for N in "${NS[@]}"; do
  echo ""
  echo "############################################"
  echo "# SEQ_LEN=$N"
  echo "############################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  CROSS_EXPECTED=""

  INPUT_DIR="$NDIR/input"
  "$PY3" "$GEN_INPUT" --N "$N" --alphabet "$ALPHABET" --seed "$SEED" --out-dir "$INPUT_DIR"
  EXPECTED="$(cat "$INPUT_DIR/expected.txt")"
  echo "  Expected LCS score: $EXPECTED"

  for P in "${PS[@]}"; do
    if [[ "$P" -gt "$N_PHYS_CORES" && "$OS_BANNER_SHOWN" -eq 0 ]]; then
      echo ""
      echo "  ################################################################"
      echo "  ##  OVERSUBSCRIPTION BEGINS HERE                              ##"
      echo "  ################################################################"
      OS_BANNER_SHOWN=1
    fi

    CUR_ROWS=$P
    if [[ "$DIM_COLS" == "auto" ]]; then
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

    BDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$BDIR/supers"
    "$PY3" "$GEN_RS" --out-dir "$BDIR" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"

    # Per-(N,grid) cargo target so deps are cached across P/reps.
    CARGO_TARGET_DIR_RUST="$CARGO_TARGET_BASE/N${N}_R${CUR_ROWS}_C${CUR_COLS}"
    mkdir -p "$CARGO_TARGET_DIR_RUST"
    CARGO_TARGET_DIR_RUST="$CARGO_TARGET_DIR_RUST" bash "$BUILD_RS" \
        "$BDIR/lcs_rs_supers" "$BDIR/supers"

    LIBSUP="$BDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"

    # Assemble for P threads.
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$BDIR/lcs_wf_P${P}" "$BDIR/lcs_wf.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$BDIR/lcs_wf_P${P}.flb"
    PLA="$BDIR/lcs_wf_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$BDIR/lcs_wf_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$BDIR/out_P${P}_r${rep}.txt"
      ERR="$BDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3 4 5; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
          LD_LIBRARY_PATH="$LIBDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          taskset -c "$CORES_P" "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -ne 0 ]]; then
          echo "  WARN: TALM-RUST N=$N P=$P rep=$rep rc=$rc (attempt $attempt/5), retrying..."
          sleep 1
          continue
        fi
        got="$(awk -F= '/^RESULT=/{print $2}' "$OUT" 2>/dev/null || true)"
        if [[ -n "$got" ]]; then break; fi
        echo "  WARN: RESULT= missing (attempt $attempt/5), retrying..."
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "TALM-RUST N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "TALM-RUST N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "ribault_rust,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "================================================================"
echo " TALM-RUST SWEEP COMPLETED"
echo " CSV: $CSV"
echo "================================================================"
