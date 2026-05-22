#!/usr/bin/env bash
# Master sweep for the Ribault paper revision experiment (N-major order).
#
# For each N: run all 7 variants (SEQ baseline + 6 parallel) over the full
# P sweep before moving to the next N. This means cross-variant tables at
# any given N become available as soon as that N completes, instead of
# waiting for the whole experiment.
#
# Variants:
#   seq         Haskell sequential 2-row DP (Data.Vector.Unboxed.Mutable)
#   super       TALM-Hs (Ribault, natural Haskell super, Vector compute)
#   strategies  GHC Strategies (parList rseq, Vector compute)
#   ribault_c    TALM-C (Ribault with C super body)
#   ribault_rust TALM-Rust (Ribault with Rust super body, pairs with Timely)
#   sucuri       Sucuri dataflow + Rust leaves (Tiago's lib)
#   repa         Repa (Data.Array.Repa, computeP per anti-diagonal)
#   timely       Timely Dataflow (Rust, epoch-per-antidiagonal)
#
# Parameters fixed:
#   NS:    100000 200000 500000 800000 900000
#   PS:    2 4 8 16 24 32 48   (24 = physical-core boundary)
#   REPS:  3 per (variant, N, P)
#   SEED:  42, ALPHABET: 4
#   Grid:  DIM_ROWS = P, DIM_COLS = ceil(N/TARGET_BLOCK_WIDTH), floor P
#   TARGET_BLOCK_WIDTH: 1000
#
# Output structure:
#   $OUTROOT/N_<N>/{seq_talmhs_strat, talm_c, sucuri, repa, timely}/metrics.csv
#   $OUTROOT/master.csv (concatenated at the end)
set -uo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="$HOME/Ribault"
OUTROOT="${OUTROOT:-$HOME/results/lcs_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/lcs_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

NS="${NS:-100000 200000 500000 800000 900000}"
PS="${PS:-2 4 8 16 24 32 48}"
REPS="${REPS:-3}"
TARGET_BLOCK_WIDTH="${TARGET_BLOCK_WIDTH:-1000}"
SEED="${SEED:-42}"
ALPHABET="${ALPHABET:-4}"

echo "============================================================="
echo " RIBAULT PAPER FINAL SWEEP  (N-major)"
echo "============================================================="
echo "  NS:    $NS"
echo "  PS:    $PS   (24 = physical-core boundary; >24 = HT oversubscription)"
echo "  REPS:  $REPS"
echo "  SEED:  $SEED  ALPHABET: $ALPHABET"
echo "  TARGET_BLOCK_WIDTH: $TARGET_BLOCK_WIDTH (auto-DIM_COLS)"
echo "  Out:   $OUTROOT"
echo "  Logs:  $LOGDIR"
echo "  Start: $(date)"
echo "============================================================="

run_phase() {
  local label="$1" outdir="$2" cmd="$3"
  local log="$LOGDIR/${label}.log"
  echo ""
  echo "----------------------------------------------------------"
  echo "  $(date)  START  $label"
  echo "  out=$outdir  log=$log"
  echo "----------------------------------------------------------"
  bash -c "$cmd" > "$log" 2>&1
  local rc=$?
  echo "  $(date)  END    $label  rc=$rc"
  return $rc
}

for N in $NS; do
  echo ""
  echo "##############################################################"
  echo "#  N = $N    $(date)"
  echo "##############################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # 1. SEQ + TALM-Hs + STRAT
  run_phase "N${N}_seq_talmhs_strat" "$NDIR/seq_talmhs_strat" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SKIP_PARPSEQ=1 SKIP_STRAT=0 SKIP_SEQ=0 \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_validated.sh '$NDIR/seq_talmhs_strat'
  "

  # 2. TALM-C
  run_phase "N${N}_talm_c" "$NDIR/talm_c" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_ribault_c.sh '$NDIR/talm_c'
  "

  # 2b. TALM-Rust
  run_phase "N${N}_talm_rust" "$NDIR/talm_rust" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_ribault_rust.sh '$NDIR/talm_rust'
  "

  # 3. Sucuri + Rust
  run_phase "N${N}_sucuri" "$NDIR/sucuri" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_sucuri.sh '$NDIR/sucuri'
  "

  # 4. Repa
  run_phase "N${N}_repa" "$NDIR/repa" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_repa.sh '$NDIR/repa'
  "

  # 5. Timely Dataflow (Rust)
  run_phase "N${N}_timely" "$NDIR/timely" "
    cd $REPO && NS='$N' PS='$PS' REPS=$REPS \
      SEED=$SEED ALPHABET=$ALPHABET \
      TARGET_BLOCK_WIDTH=$TARGET_BLOCK_WIDTH \
      bash scripts/lcs_wavefront/run_timely.sh '$NDIR/timely'
  "

  # Per-N aggregate (useful for partial inspection mid-sweep)
  N_CSV="$NDIR/N_${N}_master.csv"
  {
    echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds"
    for f in "$NDIR"/seq_talmhs_strat/metrics.csv \
             "$NDIR"/talm_c/metrics.csv \
             "$NDIR"/talm_rust/metrics.csv \
             "$NDIR"/sucuri/metrics.csv \
             "$NDIR"/repa/metrics.csv \
             "$NDIR"/timely/metrics.csv; do
      [[ -f "$f" ]] && tail -n +2 "$f"
    done
  } > "$N_CSV"
  echo "  N=$N partial aggregate: $N_CSV ($(wc -l < "$N_CSV") rows)"
done

# Final aggregate across all N
MASTER_CSV="$OUTROOT/master.csv"
{
  echo "variant,seq_len,dim_rows,dim_cols,P,rep,seconds"
  for N in $NS; do
    for f in "$OUTROOT/N_${N}"/seq_talmhs_strat/metrics.csv \
             "$OUTROOT/N_${N}"/talm_c/metrics.csv \
             "$OUTROOT/N_${N}"/talm_rust/metrics.csv \
             "$OUTROOT/N_${N}"/sucuri/metrics.csv \
             "$OUTROOT/N_${N}"/repa/metrics.csv \
             "$OUTROOT/N_${N}"/timely/metrics.csv; do
      [[ -f "$f" ]] && tail -n +2 "$f"
    done
  done
} > "$MASTER_CSV"

echo ""
echo "============================================================="
echo " ALL PHASES COMPLETE  $(date)"
echo " Master CSV: $MASTER_CSV"
echo "============================================================="
wc -l "$MASTER_CSV"
echo "  Distinct variants:"
awk -F, 'NR>1{print $1}' "$MASTER_CSV" | sort -u
