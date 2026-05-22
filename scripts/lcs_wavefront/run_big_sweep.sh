#!/usr/bin/env bash
# Master sweep: 5 N values (1M, 1.5M, 2M, 2.5M, 3M) × 5 variants
# (TALM-Hs, STRAT, monad-par, Ribault-C, Sucuri+Rust) × P sweep
#
# SEQ baseline is computed only at the smallest N (1M); for larger N we
# rely on cross-validation among parallel variants (gen_input.py already
# writes "SKIP" for N > 10000). The O(N²) extrapolation for speedup
# numbers is done at analysis time.
#
# Output: ~/results/lcs_bigN/<variant>/N_<N>/...
# Aggregate CSV per variant, one big table at the end.
set -uo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="$HOME/Ribault"
OUTROOT="$HOME/results/lcs_bigN"
LOGDIR="$HOME/runs/lcs_bigN_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$OUTROOT" "$LOGDIR"

NS=(1000000 2000000 3000000)
PS="${PS:-2 4 8 16 32 48}"
REPS="${REPS:-1}"
TARGET_BLOCK_WIDTH="${TARGET_BLOCK_WIDTH:-1000}"

echo "============================================================="
echo " BIG-N LCS WAVEFRONT SWEEP"
echo "============================================================="
echo "  NS:    ${NS[*]}"
echo "  PS:    $PS"
echo "  REPS:  $REPS"
echo "  TARGET_BLOCK_WIDTH: $TARGET_BLOCK_WIDTH (auto-DIM_COLS)"
echo "  Out:   $OUTROOT"
echo "  Logs:  $LOGDIR"
echo "============================================================="

run_phase() {
  local label="$1" outdir="$2" cmd="$3"
  local log="$LOGDIR/${label}.log"
  echo ""
  echo "=========================================================="
  echo "  $(date)  START  $label  (out=$outdir, log=$log)"
  echo "=========================================================="
  bash -c "$cmd" > "$log" 2>&1
  local rc=$?
  echo "  $(date)  END    $label  rc=$rc"
  return $rc
}

# Iterate N values, running each variant. SEQ baseline runs at every N
# (user explicitly requested Haskell-sequential as the comparison baseline).
for i in "${!NS[@]}"; do
  N="${NS[$i]}"
  SKIP_SEQ_FLAG=0
  echo ""
  echo "###############################################################"
  echo "#  N = $N   (SKIP_SEQ=$SKIP_SEQ_FLAG)"
  echo "###############################################################"

  # 1. TALM-Hs + STRAT (one runner, SEQ optional)
  run_phase "talm_stra_N${N}" "$OUTROOT/talm_stra_N${N}" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS SKIP_PARPSEQ=1 SKIP_SEQ=$SKIP_SEQ_FLAG \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_validated.sh '$OUTROOT/talm_stra_N${N}'
  "

  # 2. monad-par
  run_phase "monadpar_N${N}" "$OUTROOT/monadpar_N${N}" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_monadpar.sh '$OUTROOT/monadpar_N${N}'
  "

  # 3. Ribault-C
  run_phase "ribault_c_N${N}" "$OUTROOT/ribault_c_N${N}" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_ribault_c.sh '$OUTROOT/ribault_c_N${N}'
  "

  # 4. Sucuri+Rust
  run_phase "sucuri_N${N}" "$OUTROOT/sucuri_N${N}" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_sucuri.sh '$OUTROOT/sucuri_N${N}'
  "

done

echo ""
echo "============================================================="
echo " BIG-N SWEEP COMPLETED at $(date)"
echo "============================================================="
ls -la "$OUTROOT"
