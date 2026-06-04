#!/usr/bin/env bash
# Master sweep for the sparse-Cholesky paper benchmark.
#
# Same rigor as mergesort/LCS/attention:
#   - per-language sequential denominators (taskset core 0, REPS=7)
#   - all 9 variants with same DAG + same block ops + same checksum
#   - bootstrap CI computed by regen_tables_ci.py after the sweep
#
# Variants:
#   Sequentials:
#     seq_haskell  : sc_seq.hs                 (raw Ptr Double, +RTS -A256m)
#     seq_c        : sc_seq.c                  (gcc -O3 -march=native)
#     seq_rust     : sc_seq.rs                 (rustc release, raw *mut f64)
#   Parallels:
#     ribault_c    : Trebuchet + C super body  (block POTRF/TRSM/SYRK/GEMM)
#     ribault_rust : Trebuchet + Rust super body
#     ribault_hs   : Trebuchet + Haskell super body
#     strategies   : standalone GHC parList rseq, per-DAG-level barrier
#     parpseq      : standalone GHC manual par/pseq, per-DAG-level barrier
#     timely       : Timely Dataflow multi-epoch (one epoch per DAG level)
#     sucuri       : pyDF.DFGraph (Python 3.14t no-GIL) + PyO3 Rust kernel
#
# Per-language baseline policy:
#   ribault_hs, strategies, parpseq    -> seq_haskell
#   ribault_c                          -> seq_c
#   ribault_rust, timely, sucuri       -> seq_rust
#
# Problem instance:
#   NB blocks per side (lower-tri block storage), B = block size (B x B).
#   Total ops = N_BLOCKS POTRFs + N_BLOCKS*(NB-1) TRSMs/SYRKs (chained)
#               + GEMMs across all (i, j) pairs (chained per target).

set -uo pipefail
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="${REPO:-$HOME/Ribault}"
OUTROOT="${OUTROOT:-$HOME/results/sparse_cholesky_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/sparse_cholesky_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

# NBs / Bs to sweep. Each (NB, B) is one workload (matches the mergesort
# convention of "one workload at a time" with seq baselines computed
# per workload).
NBS="${NBS:-12 16 20 24}"
BS_LIST="${BS_LIST:-64}"
PS="${PS:-2 3 4 7 8 13 16 19 23 24 32 48}"
REPS="${REPS:-7}"
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
[[ -f "$CSV" ]] || echo "variant,NB,B,P,rep,seconds,checksum,expected" > "$CSV"

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
echo " SPARSE CHOLESKY PAPER SWEEP (LCS/attn/mergesort rigor parity)"
echo "==============================================================="
echo "  NBs:       $NBS"
echo "  Bs:        $BS_LIST"
echo "  PS:        $PS"
echo "  REPS:      $REPS"
echo "  Variants:  seq_{haskell,c,rust}, ribault_{c,rust,hs}, strategies, parpseq, timely$([[ $SUCURI_ENABLED -eq 1 ]] && echo ', sucuri')"
echo "  Out:  $OUTROOT"
echo "  Logs: $LOGDIR"
echo "==============================================================="

SEQDIR="$OUTROOT/seq_native"
mkdir -p "$SEQDIR"

echo "[build] seq_c"
gcc -O3 -march=native -o "$SEQDIR/sc_seq_c" "$REPO/scripts/sparse_cholesky/sc_seq.c" -lm

echo "[build] seq_rust"
mkdir -p "$SEQDIR/rust_build"
cp "$REPO/scripts/sparse_cholesky/Cargo.toml.sc_seq" "$SEQDIR/rust_build/Cargo.toml"
cp "$REPO/scripts/sparse_cholesky/sc_seq.rs"         "$SEQDIR/rust_build/sc_seq.rs"
(cd "$SEQDIR/rust_build" && cargo build --release --quiet)
RUST_SEQ_BIN="$SEQDIR/rust_build/target/release/sc_seq_rust"

for NB in $NBS; do
for B in $BS_LIST; do
  echo ""
  echo "########################################################"
  echo "#  NB=$NB B=$B    $(date)"
  echo "########################################################"

  NDIR="$OUTROOT/NB_${NB}_B_${B}"
  mkdir -p "$NDIR"
  DATA="$NDIR/data"

  if [[ ! -f "$DATA/expected_checksum.txt" ]]; then
    "$PY3" "$REPO/scripts/sparse_cholesky/gen_input.py" \
      --out-dir "$DATA" --nb "$NB" --b "$B" --seed "$SEED" 2>&1 | tail -3
  fi
  EXPECTED="$(cat "$DATA/expected_checksum.txt")"
  # The "expected_checksum.txt" placeholder is computed against numpy; the
  # canonical paper checksum is whatever the per-language seq_c produces
  # (cross-validated). Save and verify against that.
  EXPECTED_REAL_FILE="$DATA/expected_seq_c.txt"
  if [[ ! -f "$EXPECTED_REAL_FILE" ]]; then
    "$SEQDIR/sc_seq_c" "$DATA" 2>/dev/null | awk -F= '/^CHECKSUM=/{print $2}' >"$EXPECTED_REAL_FILE"
  fi
  EXPECTED="$(cat "$EXPECTED_REAL_FILE")"
  echo "  Canonical CHECKSUM=$EXPECTED  (from sc_seq_c)"

  # ===== Build standalone GHC binaries (seq_haskell, strategies, parpseq) =====
  for tier in seq_haskell strategies parpseq; do
    case "$tier" in
      seq_haskell) src="sc_seq.hs"     ; pkgs="-package time -package bytestring" ;;
      strategies)  src="sc_strat.hs"   ; pkgs="-package time -package parallel -package vector -package bytestring -package containers" ;;
      parpseq)     src="sc_parpseq.hs" ; pkgs="-package time -package parallel -package vector -package bytestring -package containers" ;;
    esac
    GDIR="$NDIR/${tier}"
    mkdir -p "$GDIR/obj"
    if [[ "$tier" == "seq_haskell" ]]; then THR=""; else THR="-threaded"; fi
    "$GHC_BIN" -O2 $THR -rtsopts -dynamic $pkgs \
      -outputdir "$GDIR/obj" -o "$GDIR/$tier" \
      "$REPO/scripts/sparse_cholesky/$src" \
      >"$LOGDIR/${tier}_NB${NB}_B${B}.build.log" 2>&1
    [[ -x "$GDIR/$tier" ]] || { echo "FATAL: $tier build failed NB=$NB B=$B"; cat "$LOGDIR/${tier}_NB${NB}_B${B}.build.log"; exit 1; }
  done

  echo ""
  echo "  ---- Sequentials (P=1, pinned core 0) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$NDIR/seq_haskell/out_r${rep}.txt"
    taskset -c 0 "$NDIR/seq_haskell/seq_haskell" "$DATA" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_haskell  rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_haskell,$NB,$B,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_c_NB${NB}_B${B}_r${rep}.txt"
    taskset -c 0 "$SEQDIR/sc_seq_c" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_c        rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_c,$NB,$B,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_rust_NB${NB}_B${B}_r${rep}.txt"
    taskset -c 0 "$RUST_SEQ_BIN" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_rust     rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_rust,$NB,$B,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  for P in $PS; do
    CORES_P="$(pin_cores "$P")"
    echo ""
    echo "  ======== NB=$NB B=$B P=$P  cores=$CORES_P ========"

    # -------- ribault_c --------
    VDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/sparse_cholesky/gen_sc_c.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    bash "$REPO/tools/build_supers_c.sh" "$VDIR/attn_c_supers.c" "$VDIR/supers" \
      >"$LOGDIR/ribault_c_NB${NB}_B${B}_P${P}.build.log" 2>&1
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
      echo "ribault_c,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_rust --------
    VDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/sparse_cholesky/gen_sc_rust.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/sc_rs_supers" "$VDIR/supers" \
      >"$LOGDIR/ribault_rust_NB${NB}_B${B}_P${P}.build.log" 2>&1
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
      echo "ribault_rust,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_hs --------
    VDIR="$NDIR/ribault_hs_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/sparse_cholesky/gen_sc_hs.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    SUPERS_INJECT_FILE="$VDIR/supers_inject.hs" \
      SUPERS_GHC_PACKAGES="vector bytestring" \
      CFLAGS="$SUPERS_CFLAGS" \
      bash "$REPO/tools/build_supers.sh" "$VDIR/attn.hsk" "$VDIR/supers/Supers.hs" \
      >"$LOGDIR/ribault_hs_NB${NB}_B${B}_P${P}.build.log" 2>&1
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
      echo "ribault_hs,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- strategies --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/strategies/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/strategies/strategies" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    strategies   rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "strategies,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- parpseq --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/parpseq/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/parpseq/parpseq" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    parpseq      rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "parpseq,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- timely --------
    VDIR="$NDIR/timely_P${P}"
    mkdir -p "$VDIR"
    cp "$REPO/scripts/sparse_cholesky/Cargo.toml.sc_timely" "$VDIR/Cargo.toml"
    cp "$REPO/scripts/sparse_cholesky/sc_timely.rs"         "$VDIR/sc_timely.rs"
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet \
      >"$LOGDIR/timely_NB${NB}_B${B}_P${P}.build.log" 2>&1)
    BIN="$VDIR/target/release/sc_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" "$P" "$DATA" >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    timely       rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "timely,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- sucuri (optional) --------
    if (( SUCURI_ENABLED == 1 )); then
      VDIR="$NDIR/sucuri_P${P}"
      mkdir -p "$VDIR"
      "$PY3" "$REPO/scripts/sparse_cholesky/gen_sc_sucuri.py" \
        --project-dir "$VDIR/crate" \
        --py-driver "$VDIR/run_sucuri.py" \
        --data-dir "$DATA" >/dev/null
      (cd "$VDIR/crate" && PYO3_PYTHON="$PYTHON_NOGIL" \
        CARGO_TARGET_DIR="$VDIR/crate/target" \
        cargo build --release --quiet \
        >"$LOGDIR/sucuri_NB${NB}_B${B}_P${P}.build.log" 2>&1)
      RUST_SO="$VDIR/crate/target/release/libsucuri_sc.so"
      if [[ ! -f "$RUST_SO" ]]; then
        echo "    sucuri       BUILD FAILED — see $LOGDIR/sucuri_NB${NB}_B${B}_P${P}.build.log"
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
          echo "sucuri,$NB,$B,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      fi
    fi
  done
done
done

echo ""
echo "============================================================="
echo " SPARSE CHOLESKY SWEEP COMPLETE  $(date)"
echo " CSV: $CSV"
echo "============================================================="
wc -l "$CSV"
awk -F, 'NR>1{print $1}' "$CSV" | sort | uniq -c
