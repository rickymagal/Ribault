#!/usr/bin/env bash
set -euo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

# ============================================================
# LCS Wavefront — Timely Dataflow sweep (Rust crate `timely`)
#
# Same DIM_ROWS x DIM_COLS grid as TALM/MP/Repa. Compute backend
# in Rust (Timely is Rust-native; cross-language fairness caveat
# is the same as Sucuri/Rust).
#
# Wavefront: epoch-per-antidiagonal. Worker 0 feeds block coords
# at epoch d=i+j; Exchange distributes across workers; map runs
# compute_block via shared raw pointers; probe barriers between
# epochs.
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/lcs_wavefront_timely}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
GEN_INPUT="$REPO/scripts/lcs_wavefront/gen_input.py"
GEN_TIMELY="$REPO/scripts/lcs_wavefront/gen_rs_timely.py"

REPS=${REPS:-7}
SEED=${SEED:-42}
ALPHABET=${ALPHABET:-4}
NS=(${NS:-200000})
PS=(${PS:-1 2 4 8 16 24 32 40 48})
DIM_COLS=${DIM_COLS:-auto}
TARGET_BLOCK_WIDTH=${TARGET_BLOCK_WIDTH:-1000}

N_PHYS_CORES=${N_PHYS_CORES:-24}
N_LOG_CORES=${N_LOG_CORES:-48}

# Cargo build target dir shared across runs (to avoid recompiling deps).
CARGO_TARGET_DIR_BASE="${CARGO_TARGET_DIR_BASE:-$HOME/.cache/cargo-target-timely-lcs}"
mkdir -p "$CARGO_TARGET_DIR_BASE"

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
      echo "FATAL: $label -> got $got, expected $CROSS_EXPECTED"; exit 1
    else
      echo "  OK: RESULT=$got (cross-validated)"
    fi
  else
    [[ "$got" == "$expected" ]] || { echo "FATAL: got $got, expected $expected"; exit 1; }
    echo "  OK: RESULT=$got (correct)"
  fi
}

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds" > "$CSV"

echo "================================================================"
echo " LCS Wavefront — Timely Dataflow sweep"
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

    BDIR="$NDIR/timely_P${P}"
    mkdir -p "$BDIR"
    "$PY3" "$GEN_TIMELY" --project-dir "$BDIR" --input-dir "$INPUT_DIR" \
        --dim-rows "$CUR_ROWS" --dim-cols "$CUR_COLS"

    # Reuse a shared cargo target dir keyed by (N, ROWS, COLS) to avoid
    # rebuilding deps; only main.rs changes drive recompile.
    export CARGO_TARGET_DIR="$CARGO_TARGET_DIR_BASE/N${N}_R${CUR_ROWS}_C${CUR_COLS}"
    mkdir -p "$CARGO_TARGET_DIR"
    (cd "$BDIR" && cargo build --release --quiet) || {
      echo "FATAL: cargo build failed for N=$N P=$P"
      exit 1
    }
    BIN="$CARGO_TARGET_DIR/release/lcs_wf_timely"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$BDIR/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" -w "$P" -n 1 >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "TIMELY   N=$N P=$P ${CUR_ROWS}x${CUR_COLS} rep=$rep -> ${secs}s"
      validate "TIMELY N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "timely,$N,$CUR_ROWS,$CUR_COLS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "================================================================"
echo " TIMELY SWEEP COMPLETED"
echo " CSV: $CSV"
echo "================================================================"
