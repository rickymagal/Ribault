#!/usr/bin/env bash
# Master sweep for the attention end-to-end paper benchmark.
#
# 4 variants × 5 N values × 12 P values × REPS=7 = 1680 measurements
#   variants: ribault_c, ribault_rust, strategies, timely
#   N (sequence length):   1024 2048 4096 6144 8192
#   P (worker count):      2 3 4 7 8 13 16 19 23 24 (within physical) + 32 48 (HT)
#   REPS:                  7
#   D:                     512
#   N_HEADS:               8 (tunable)
#   D_FF:                  2048
#   VOCAB:                 256 (byte-level)
#
# All variants compute the same one-shot full-sequence transformer block:
# embed -> sinusoidal pos -> LN -> multi-head SA -> W_O + residual -> LN
# -> ReLU FFN -> residual -> unembed -> argmax -> output tokens.
# Cross-validated by integer checksum against numpy reference output.
set -uo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="$HOME/Ribault"
OUTROOT="${OUTROOT:-$HOME/results/attn_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/attn_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

NS="${NS:-1024 2048 4096 6144 8192}"
PS="${PS:-2 3 4 7 8 13 16 19 23 24 32 48}"
REPS="${REPS:-7}"
D="${D:-512}"
N_HEADS="${N_HEADS:-8}"
D_FF="${D_FF:-2048}"
VOCAB="${VOCAB:-256}"
SEED="${SEED:-42}"

GHC_BIN="${GHC:-ghc}"
PY3="${PY3:-python3}"
N_PHYS_CORES="${N_PHYS_CORES:-24}"
N_LOG_CORES="${N_LOG_CORES:-48}"

CSV="$OUTROOT/metrics.csv"
echo "variant,N,D,n_heads,n_blocks,P,rep,seconds,checksum,expected" > "$CSV"

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"; else echo "0-$((p - 1))"; fi
}

echo "==============================================================="
echo " ATTENTION END-TO-END PAPER SWEEP"
echo "==============================================================="
echo "  NS:        $NS"
echo "  PS:        $PS"
echo "  REPS:      $REPS"
echo "  D=$D  N_HEADS=$N_HEADS  D_FF=$D_FF  VOCAB=$VOCAB  SEED=$SEED"
echo "  Out:       $OUTROOT"
echo "  Logs:      $LOGDIR"
echo "  Variants:  ribault_c, ribault_rust, strategies, timely"
echo "  Start:     $(date)"
echo "==============================================================="

for N in $NS; do
  echo ""
  echo "########################################################"
  echo "#  N=$N    $(date)"
  echo "########################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  DATA="$NDIR/data"
  if [[ ! -f "$DATA/expected_checksum.txt" ]]; then
    "$PY3" "$REPO/scripts/attention/gen_attn_data.py" \
      --out-dir "$DATA" --N "$N" --D "$D" --n-heads "$N_HEADS" \
      --d-ff "$D_FF" --vocab "$VOCAB" --seed "$SEED" 2>&1 | tail -1
  fi
  EXPECTED="$(cat "$DATA/expected_checksum.txt")"
  echo "  Expected CHECKSUM=$EXPECTED"

  for P in $PS; do
    N_BLOCKS=$P  # one block per worker (matches LCS DIM_ROWS=P pattern)
    CORES_P="$(pin_cores "$P")"
    echo ""
    echo "  ======== N=$N  P=$P  n_blocks=$N_BLOCKS  cores=$CORES_P ========"

    # =====================================================================
    # Variant: ribault_c
    # =====================================================================
    VDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/attention/gen_attn_c.py" \
      --out-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    bash "$REPO/tools/build_supers_c.sh" "$VDIR/attn_c_supers.c" "$VDIR/supers" >/dev/null 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"
    PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A=256m LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      set -e
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo 0)"
      echo "    ribault_c    rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "ribault_c,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # =====================================================================
    # Variant: ribault_rust
    # =====================================================================
    VDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/attention/gen_attn_rust.py" \
      --out-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/attn_rs_supers" "$VDIR/supers" >/dev/null 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"
    PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A=256m LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      set -e
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo 0)"
      echo "    ribault_rust rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "ribault_rust,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # =====================================================================
    # Variant: strategies (GHC Strategies)
    # =====================================================================
    VDIR="$NDIR/strategies_P${P}"
    mkdir -p "$VDIR/obj"
    "$PY3" "$REPO/scripts/attention/gen_attn_hs_strategies.py" \
      --out "$VDIR/attn.hs" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    "$GHC_BIN" -O2 -threaded -rtsopts -dynamic \
      -package time -package parallel -package vector -package bytestring \
      -outputdir "$VDIR/obj" -o "$VDIR/attn" "$VDIR/attn.hs" >/dev/null 2>&1
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$VDIR/attn" +RTS -N"$P" -A256m -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      echo "    strategies   rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "strategies,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # =====================================================================
    # Variant: timely
    # =====================================================================
    VDIR="$NDIR/timely_P${P}"
    "$PY3" "$REPO/scripts/attention/gen_attn_rs_timely.py" \
      --project-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet 2>/dev/null)
    BIN="$VDIR/target/release/attn_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" -w "$P" -n 1 >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      echo "    timely       rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "timely,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done
  done
done

echo ""
echo "============================================================="
echo " ATTENTION SWEEP COMPLETE  $(date)"
echo " CSV: $CSV"
echo "============================================================="
wc -l "$CSV"
awk -F, 'NR>1{print $1}' "$CSV" | sort | uniq -c
