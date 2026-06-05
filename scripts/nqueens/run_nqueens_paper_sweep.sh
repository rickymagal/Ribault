#!/usr/bin/env bash
# Master sweep for the N-Queens paper benchmark.
#
# Each variant implements the FULL recursive backtracking inside its
# own binary (NO pre-expanded prefix states shared across variants).
# Cross-validation: total count matches OEIS A000170.
#   N=11: 2680   N=12: 14200   N=13: 73712   N=14: 365596   N=15: 2279184
#
# Variants:
#   seq_haskell  : nq_seq.hs   (Data.Vector.Unboxed, V.snoc per call)
#   seq_c        : nq_seq.c    (int queens[16] stack, gcc -O3)
#   seq_rust     : nq_seq.rs   (i32 stack array, opt-level=3 lto=thin)
#   strategies   : nq_strat.hs (recursive parList rseq up to CUTOFF)
#   parpseq      : nq_parpseq.hs (recursive par/pseq up to CUTOFF)
#   timely       : nq_timely.rs (expand-then-distribute via .exchange)
#   ribault_hs   : .hss via codegen, recursive TALM with callsnd/retsnd

set -uo pipefail
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="${REPO:-$HOME/Ribault}"
OUTROOT="${OUTROOT:-$HOME/results/nqueens_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/nqueens_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

NS="${NS:-11 12 13 14 15}"
PS="${PS:-2 3 4 7 8 13 16 19 23 24 32 48}"
REPS="${REPS:-7}"
CUTOFF="${CUTOFF:-5}"
TALM_RTS_A="${TALM_RTS_A:-256m}"

GHC_BIN="${GHC:-ghc}"
PY3="${PY3:-python3}"
N_LOG_CORES="${N_LOG_CORES:-48}"
CODEGEN="${CODEGEN:-$REPO/codegen}"

CSV="$OUTROOT/metrics.csv"
[[ -f "$CSV" ]] || echo "variant,N,cutoff,P,rep,seconds,checksum,expected" > "$CSV"

GHC_VER="$("$GHC_BIN" --numeric-version 2>/dev/null || echo unknown)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir 2>/dev/null || echo "")"
SUPERS_CFLAGS="${CFLAGS:-}"
if [[ -z "$SUPERS_CFLAGS" ]]; then
  for cand in \
    "$GHC_LIBDIR/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/../lib/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/rts/include" "$GHC_LIBDIR/include"; do
    [[ -f "$cand/HsFFI.h" ]] && { SUPERS_CFLAGS="-O2 -fPIC -I$cand"; break; }
  done
  [[ -z "$SUPERS_CFLAGS" ]] && SUPERS_CFLAGS="-O2 -fPIC"
fi

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"
  else echo "0-$((p - 1))"; fi
}

echo "==============================================================="
echo " N-QUEENS PAPER SWEEP (recursive in-binary)"
echo "==============================================================="
echo "  NS=$NS  PS=$PS  REPS=$REPS  CUTOFF=$CUTOFF"
echo "  Out:  $OUTROOT"
echo "==============================================================="

SEQDIR="$OUTROOT/seq_native"; mkdir -p "$SEQDIR"
echo "[build] seq_c"
gcc -O3 -march=native -o "$SEQDIR/nq_seq_c" "$REPO/scripts/nqueens/nq_seq.c"

echo "[build] seq_rust"
mkdir -p "$SEQDIR/rust_build"
cp "$REPO/scripts/nqueens/Cargo.toml.nq_seq" "$SEQDIR/rust_build/Cargo.toml"
cp "$REPO/scripts/nqueens/nq_seq.rs"         "$SEQDIR/rust_build/nq_seq.rs"
(cd "$SEQDIR/rust_build" && cargo build --release --quiet)
RUST_SEQ_BIN="$SEQDIR/rust_build/target/release/nq_seq_rust"

for N in $NS; do
  echo ""
  echo "##############  N=$N  $(date)  ##############"
  NDIR="$OUTROOT/N_${N}"; mkdir -p "$NDIR"
  DATA="$NDIR/data"
  if [[ ! -f "$DATA/expected_checksum.txt" ]]; then
    "$PY3" "$REPO/scripts/nqueens/gen_input.py" --out-dir "$DATA" --N "$N" --cutoff "$CUTOFF" 2>&1 | tail -3
  fi
  EXPECTED="$(cat "$DATA/expected_checksum.txt")"
  echo "  expected Q(N)=$EXPECTED"

  for tier in seq_haskell strategies parpseq; do
    case "$tier" in
      seq_haskell) src="nq_seq.hs"     ; pkgs="-package time -package vector" ;;
      strategies)  src="nq_strat.hs"   ; pkgs="-package time -package parallel -package vector" ;;
      parpseq)     src="nq_parpseq.hs" ; pkgs="-package time -package parallel -package vector" ;;
    esac
    GDIR="$NDIR/${tier}"; mkdir -p "$GDIR/obj"
    if [[ "$tier" == "seq_haskell" ]]; then THR=""; else THR="-threaded"; fi
    "$GHC_BIN" -package-env - -O2 $THR -rtsopts -dynamic $pkgs \
      -outputdir "$GDIR/obj" -o "$GDIR/$tier" \
      "$REPO/scripts/nqueens/$src" >"$LOGDIR/${tier}_N${N}.build.log" 2>&1
    [[ -x "$GDIR/$tier" ]] || { echo "FATAL: $tier build failed N=$N"; cat "$LOGDIR/${tier}_N${N}.build.log"; exit 1; }
  done

  echo "  ---- Sequentials (P=1) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$NDIR/seq_haskell/out_r${rep}.txt"
    taskset -c 0 "$NDIR/seq_haskell/seq_haskell" "$DATA" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_haskell rep=$rep ${secs}s cs=$cs"
    echo "seq_haskell,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_c_N${N}_r${rep}.txt"
    taskset -c 0 "$SEQDIR/nq_seq_c" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_c       rep=$rep ${secs}s cs=$cs"
    echo "seq_c,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_rust_N${N}_r${rep}.txt"
    taskset -c 0 "$RUST_SEQ_BIN" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_rust    rep=$rep ${secs}s cs=$cs"
    echo "seq_rust,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  # ribault_hs via the recursive .hss path.
  #
  # Design notes (post-debug June 2026, see gen_nq_hss.py header):
  #
  # The naive list-cons or two-recursive-functions formulation hits
  # codegen lowering bugs (silently undercounts).  We use a single
  # TALM-recursive `solve` with column-axis unrolled at .hss-emit
  # time (one let-binding per column), plus two supers: `safeRec`
  # (diagonal check) and `solveRest` (sequential Haskell solver for
  # the bottom of the search tree).  CUTOFF is chosen per N to stay
  # inside the interpreter's deque limit: gen_nq_hss.py defaults to
  # CUTOFF=3 for N<=12 and CUTOFF=2 for N>=13.
  #
  # Verified against OEIS A000170 for N=4..15.
  RH_FL=""
  RH_DIR="$NDIR/ribault_hs"
  mkdir -p "$RH_DIR/supers"
  # gen_nq_hss.py picks CUTOFF automatically from N
  "$PY3" "$REPO/scripts/nqueens/gen_nq_hss.py" --out "$RH_DIR/nq.hss" --N "$N"
  if [[ -x "$CODEGEN" ]]; then
    if "$CODEGEN" "$RH_DIR/nq.hss" > "$RH_DIR/nq.fl" 2>"$LOGDIR/codegen_N${N}.log"; then
      RH_FL="$RH_DIR/nq.fl"
      CFLAGS="$SUPERS_CFLAGS" bash "$REPO/tools/build_supers.sh" \
        "$RH_DIR/nq.hss" "$RH_DIR/supers/Supers.hs" \
        >"$LOGDIR/ribault_hs_supers_N${N}.build.log" 2>&1 || RH_FL=""
    else
      echo "    [WARN] ribault_hs codegen failed for N=$N"
    fi
  fi

  for P in $PS; do
    CORES_P="$(pin_cores "$P")"
    echo "  ==== N=$N P=$P cores=$CORES_P ===="

    # strategies
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/strategies/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/strategies/strategies" "$DATA" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    strategies   rep=$rep ${secs}s cs=$cs"
      echo "strategies,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/parpseq/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/parpseq/parpseq" "$DATA" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    parpseq      rep=$rep ${secs}s cs=$cs"
      echo "parpseq,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # timely
    VDIR="$NDIR/timely_P${P}"; mkdir -p "$VDIR"
    cp "$REPO/scripts/nqueens/Cargo.toml.nq_timely" "$VDIR/Cargo.toml"
    cp "$REPO/scripts/nqueens/nq_timely.rs"         "$VDIR/nq_timely.rs"
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet \
      >"$LOGDIR/timely_N${N}_P${P}.build.log" 2>&1)
    BIN="$VDIR/target/release/nq_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" "$P" "$DATA" >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    timely       rep=$rep ${secs}s cs=$cs"
      echo "timely,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # ribault_hs via codegen-produced .fl
    if [[ -n "$RH_FL" && -f "$RH_FL" ]]; then
      RH_P="$RH_DIR/P${P}"; mkdir -p "$RH_P"
      pushd "$REPO/TALM/asm" >/dev/null
        "$PY3" assembler.py -a -n "$P" -o "$RH_P/nq_P${P}" "$RH_FL" >/dev/null 2>&1
      popd >/dev/null
      FLB="$RH_P/nq_P${P}.flb"
      PLA="$RH_P/nq_P${P}_auto.pla"; [[ -f "$PLA" ]] || PLA="$RH_P/nq_P${P}.pla"
      # ribault_hs supers built from the .hss have a Haskell sub-super
      # for nq_seq; codegen writes the Haskell file alongside.
      LIBSUP_DIR="$RH_DIR/supers"
      LIBSUP="$LIBSUP_DIR/libsupers.so"
      if [[ -f "$LIBSUP" ]]; then
        LIBDIR="$LIBSUP_DIR"; GHCDEPS="$LIBDIR/ghc-deps"
        for ((rep=1; rep<=REPS; rep++)); do
          OUT="$RH_P/out_r${rep}.txt"; ERR="$RH_P/err_r${rep}.txt"
          SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
            LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
            taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
          # interp emits the raw integer count on stdout (Ribault `print`);
          # accept either "CHECKSUM=N" or a bare integer on the first line.
          cs="$(awk -F= '/^CHECKSUM=/{print $2; exit}' "$OUT")"
          [[ -z "$cs" ]] && cs="$(awk 'NR==1 && $1 ~ /^[0-9]+$/ {print $1; exit}' "$OUT")"
          [[ -z "$cs" ]] && cs=0
          secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" || echo 0)"
          echo "    ribault_hs   rep=$rep ${secs}s cs=$cs"
          echo "ribault_hs,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      else
        echo "    [WARN] ribault_hs supers not built for N=$N — skipping"
      fi
    fi
  done
done

echo ""
echo "============= N-QUEENS SWEEP COMPLETE $(date) ============="
wc -l "$CSV"
