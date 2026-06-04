#!/usr/bin/env bash
# Master sweep for the Cascading Inference Pipeline paper benchmark.
#
# Same rigor as LCS / mergesort / dense_block_cholesky:
#   - per-language sequential denominators (taskset core 0, REPS=7)
#   - all 9 parallel variants with same DAG + same per-item kernels
#     + same final checksum
#   - bootstrap CI computed by regen_tables_ci.py after the sweep
#
# Variants:
#   Sequentials:
#     seq_haskell  : cip_seq.hs                 (raw Ptr Word8, +RTS -A256m)
#     seq_c        : cip_seq.c                  (gcc -O3 -march=native, STRICT IEEE)
#     seq_rust     : cip_seq.rs                 (rustc release, raw *mut)
#   Parallels:
#     ribault_c    : Trebuchet + C super body
#     ribault_rust : Trebuchet + Rust super body
#     ribault_hs   : Trebuchet + Haskell super body
#     strategies   : standalone GHC parList rseq, per-stage barrier
#     parpseq      : standalone GHC manual par/pseq, per-stage barrier
#     timely       : Timely Dataflow multi-epoch (one epoch per stage)
#     sucuri       : pyDF.DFGraph (Python 3.14t no-GIL) + PyO3 Rust kernel
#
# Per-language baseline policy:
#   ribault_hs, strategies, parpseq    -> seq_haskell
#   ribault_c                          -> seq_c
#   ribault_rust, timely, sucuri       -> seq_rust

set -uo pipefail
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="${REPO:-$HOME/Ribault}"
OUTROOT="${OUTROOT:-$HOME/results/cascading_inference_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/cascading_inference_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

NS="${NS:-100000 250000 500000 1000000 2000000}"
PS="${PS:-2 3 4 7 8 13 16 19 23 24 32 48}"
REPS="${REPS:-7}"
CHUNK_SIZE="${CHUNK_SIZE:-512}"
SEED="${SEED:-42}"
TALM_RTS_A="${TALM_RTS_A:-256m}"

GHC_BIN="${GHC:-ghc}"
PY3="${PY3:-python3}"
N_LOG_CORES="${N_LOG_CORES:-48}"

SUCURI_ROOT="${SUCURI_ROOT:-$HOME/Sucuri}"
PYTHON_NOGIL="${PYTHON_NOGIL:-$HOME/python3.14t/bin/python3.14t}"
PYTHON_LIBDIR="${PYTHON_LIBDIR:-$HOME/python3.14t/lib}"
SUCURI_ENABLED=0
if [[ -d "$SUCURI_ROOT" && -x "$PYTHON_NOGIL" ]]; then SUCURI_ENABLED=1; fi

CSV="$OUTROOT/metrics.csv"
[[ -f "$CSV" ]] || echo "variant,N,chunk_size,P,rep,seconds,checksum,expected" > "$CSV"

# ---- detect HsFFI include for build_supers ----
GHC_VER="$("$GHC_BIN" --numeric-version 2>/dev/null || echo unknown)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir 2>/dev/null || echo "")"
SUPERS_CFLAGS="${CFLAGS:-}"
if [[ -z "$SUPERS_CFLAGS" ]]; then
  for cand in \
    "$GHC_LIBDIR/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/../lib/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR/rts/include" \
    "$GHC_LIBDIR/include"; do
    if [[ -f "$cand/HsFFI.h" ]]; then
      SUPERS_CFLAGS="-O2 -fPIC -I$cand"; break
    fi
  done
  [[ -z "$SUPERS_CFLAGS" ]] && SUPERS_CFLAGS="-O2 -fPIC"
fi

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"
  else echo "0-$((p - 1))"; fi
}

echo "==============================================================="
echo " CASCADING INFERENCE PIPELINE PAPER SWEEP"
echo "==============================================================="
echo "  NS:        $NS"
echo "  PS:        $PS"
echo "  REPS:      $REPS"
echo "  CHUNK_SIZE: $CHUNK_SIZE"
echo "  Variants:  seq_{haskell,c,rust}, ribault_{c,rust,hs}, strategies, parpseq, timely$([[ $SUCURI_ENABLED -eq 1 ]] && echo ', sucuri')"
echo "  Out:  $OUTROOT"
echo "  Logs: $LOGDIR"
echo "==============================================================="

SEQDIR="$OUTROOT/seq_native"
mkdir -p "$SEQDIR"

# NOTE: STRICT IEEE on seq_c — no -ffast-math.  cip_seq.{rs,hs} use strict
# IEEE too; this is required for cross-language CHECKSUM equivalence.
echo "[build] seq_c (strict IEEE — no -ffast-math)"
gcc -O3 -march=native -o "$SEQDIR/cip_seq_c" "$REPO/scripts/cascading_inference/cip_seq.c" -lm

echo "[build] seq_rust"
mkdir -p "$SEQDIR/rust_build"
cp "$REPO/scripts/cascading_inference/Cargo.toml.cip_seq" "$SEQDIR/rust_build/Cargo.toml"
cp "$REPO/scripts/cascading_inference/cip_seq.rs"         "$SEQDIR/rust_build/cip_seq.rs"
(cd "$SEQDIR/rust_build" && cargo build --release --quiet)
RUST_SEQ_BIN="$SEQDIR/rust_build/target/release/cip_seq_rust"

for N in $NS; do
  echo ""
  echo "########################################################"
  echo "#  N=$N    $(date)"
  echo "########################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  DATA="$NDIR/data"

  if [[ ! -f "$DATA/expected_checksum.txt" ]]; then
    "$PY3" "$REPO/scripts/cascading_inference/gen_input.py" \
      --out-dir "$DATA" --N "$N" --chunk-size "$CHUNK_SIZE" --seed "$SEED" 2>&1 | tail -3
  fi
  # Canonical expected: whatever cip_seq_c produces, NOT what gen_input.py
  # wrote (Python float ordering can differ slightly from strict C).
  EXPECTED_REAL_FILE="$DATA/expected_seq_c.txt"
  if [[ ! -f "$EXPECTED_REAL_FILE" ]]; then
    "$SEQDIR/cip_seq_c" "$DATA" 2>/dev/null | awk -F= '/^CHECKSUM=/{print $2}' >"$EXPECTED_REAL_FILE"
  fi
  EXPECTED="$(cat "$EXPECTED_REAL_FILE")"
  echo "  Canonical CHECKSUM=$EXPECTED  (from cip_seq_c)"

  # Standalone GHC binaries.
  for tier in seq_haskell strategies parpseq; do
    case "$tier" in
      seq_haskell) src="cip_seq.hs"     ; pkgs="-package time -package bytestring" ;;
      strategies)  src="cip_strat.hs"   ; pkgs="-package time -package parallel -package bytestring" ;;
      parpseq)     src="cip_parpseq.hs" ; pkgs="-package time -package parallel -package bytestring" ;;
    esac
    GDIR="$NDIR/${tier}"
    mkdir -p "$GDIR/obj"
    if [[ "$tier" == "seq_haskell" ]]; then THR=""; else THR="-threaded"; fi
    "$GHC_BIN" -package-env - -O2 $THR -rtsopts -dynamic $pkgs \
      -outputdir "$GDIR/obj" -o "$GDIR/$tier" \
      "$REPO/scripts/cascading_inference/$src" \
      >"$LOGDIR/${tier}_N${N}.build.log" 2>&1
    [[ -x "$GDIR/$tier" ]] || { echo "FATAL: $tier build failed N=$N"; cat "$LOGDIR/${tier}_N${N}.build.log"; exit 1; }
  done

  echo ""
  echo "  ---- Sequentials (P=1, pinned core 0) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$NDIR/seq_haskell/out_r${rep}.txt"
    taskset -c 0 "$NDIR/seq_haskell/seq_haskell" "$DATA" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_haskell  rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_haskell,$N,$CHUNK_SIZE,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_c_N${N}_r${rep}.txt"
    taskset -c 0 "$SEQDIR/cip_seq_c" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_c        rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_c,$N,$CHUNK_SIZE,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_rust_N${N}_r${rep}.txt"
    taskset -c 0 "$RUST_SEQ_BIN" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_rust     rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_rust,$N,$CHUNK_SIZE,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  for P in $PS; do
    CORES_P="$(pin_cores "$P")"
    echo ""
    echo "  ======== N=$N P=$P  cores=$CORES_P ========"

    # -------- ribault_c --------
    VDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/cascading_inference/gen_cip_c.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    bash "$REPO/tools/build_supers_c.sh" "$VDIR/attn_c_supers.c" "$VDIR/supers" \
      >"$LOGDIR/ribault_c_N${N}_P${P}.build.log" 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"; PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" || echo 0)"
      echo "    ribault_c    rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "ribault_c,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_rust --------
    VDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/cascading_inference/gen_cip_rust.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/cip_rs_supers" "$VDIR/supers" \
      >"$LOGDIR/ribault_rust_N${N}_P${P}.build.log" 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"; PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" || echo 0)"
      echo "    ribault_rust rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "ribault_rust,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_hs --------
    VDIR="$NDIR/ribault_hs_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/cascading_inference/gen_cip_hs.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    SUPERS_INJECT_FILE="$VDIR/supers_inject.hs" \
      SUPERS_GHC_PACKAGES="bytestring" \
      CFLAGS="$SUPERS_CFLAGS" \
      bash "$REPO/tools/build_supers.sh" "$VDIR/attn.hsk" "$VDIR/supers/Supers.hs" \
      >"$LOGDIR/ribault_hs_N${N}_P${P}.build.log" 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"; PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
        LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" || echo 0)"
      echo "    ribault_hs   rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "ribault_hs,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- strategies --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/strategies/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/strategies/strategies" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    strategies   rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "strategies,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- parpseq --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/parpseq/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/parpseq/parpseq" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    parpseq      rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "parpseq,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- timely --------
    VDIR="$NDIR/timely_P${P}"
    mkdir -p "$VDIR"
    cp "$REPO/scripts/cascading_inference/Cargo.toml.cip_timely" "$VDIR/Cargo.toml"
    cp "$REPO/scripts/cascading_inference/cip_timely.rs"         "$VDIR/cip_timely.rs"
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet \
      >"$LOGDIR/timely_N${N}_P${P}.build.log" 2>&1)
    BIN="$VDIR/target/release/cip_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" "$P" "$DATA" >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    timely       rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "timely,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- sucuri (optional) --------
    if (( SUCURI_ENABLED == 1 )); then
      VDIR="$NDIR/sucuri_P${P}"
      mkdir -p "$VDIR"
      "$PY3" "$REPO/scripts/cascading_inference/gen_cip_sucuri.py" \
        --project-dir "$VDIR/crate" \
        --py-driver "$VDIR/run_sucuri.py" \
        --data-dir "$DATA" >/dev/null
      (cd "$VDIR/crate" && PYO3_PYTHON="$PYTHON_NOGIL" \
        CARGO_TARGET_DIR="$VDIR/crate/target" \
        cargo build --release --quiet \
        >"$LOGDIR/sucuri_N${N}_P${P}.build.log" 2>&1)
      RUST_SO="$VDIR/crate/target/release/libsucuri_cip.so"
      if [[ ! -f "$RUST_SO" ]]; then
        echo "    sucuri       BUILD FAILED — see $LOGDIR/sucuri_N${N}_P${P}.build.log"
      else
        for ((rep=1; rep<=REPS; rep++)); do
          OUT="$VDIR/out_r${rep}.txt"
          SUCURI_ROOT="$SUCURI_ROOT" \
            LD_LIBRARY_PATH="$PYTHON_LIBDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
            PYTHON_GIL=0 \
            taskset -c "$CORES_P" \
              "$PYTHON_NOGIL" "$VDIR/run_sucuri.py" \
                --rust-so "$RUST_SO" \
                --data-dir "$DATA" \
                --workers "$P" >"$OUT" 2>/dev/null
          cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
          secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
          echo "    sucuri       rep=$rep  ${secs}s  CHECKSUM=$cs"
          echo "sucuri,$N,$CHUNK_SIZE,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      fi
    fi
  done
done

echo ""
echo "============================================================="
echo " CIP SWEEP COMPLETE  $(date)"
echo " CSV: $CSV"
echo "============================================================="
wc -l "$CSV"
awk -F, 'NR>1{print $1}' "$CSV" | sort | uniq -c
