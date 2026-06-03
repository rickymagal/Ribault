#!/usr/bin/env bash
# Master sweep for the mergesort paper benchmark.
#
# Variants (LCS / attention rigor parity):
#   Sequentials (per-language denominators, REPS=7 each, taskset core 0):
#     seq_haskell  : monolithic GHC, Data.Vector.Unboxed.Mutable, +RTS -A256m
#     seq_c        : monolithic gcc -O3 -march=native, raw int arrays
#     seq_rust     : monolithic rustc release, raw pointers + unsafe inner loops
#   Parallels (REPS=7 each, taskset 0..P-1):
#     ribault_c    : Trebuchet + C super body (leaf insertion-sort + merge)
#     ribault_rust : Trebuchet + Rust super body (raw *mut i32 + unsafe)
#     ribault_hs   : Trebuchet + Haskell super body (raw Ptr Int32 + peekElemOff)
#     strategies   : standalone GHC parList rseq, per-level barrier
#     parpseq      : standalone GHC manual par/pseq, per-level barrier
#     timely       : Timely Dataflow multi-epoch (epoch 0 leaves, epochs 1..max merges)
#     sucuri       : pyDF.DFGraph (Python 3.14t no-GIL) + PyO3 Rust kernel
#
# Per-language baseline policy:
#   ribault_hs, strategies, parpseq    -> seq_haskell   (Haskell tier)
#   ribault_c                          -> seq_c         (C tier)
#   ribault_rust, timely, sucuri       -> seq_rust      (Rust tier)
#
# Binary tree DAG: N elements, leaf CUTOFF, depth=log2(N/CUTOFF). The
# .fl emits N_LEAVES leaf supers + N_MERGES merge supers connected by
# the deterministic top-down bisection topology. Tree.bin is shared
# across variants (written by gen_input.py).

set -uo pipefail
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="${REPO:-$HOME/Ribault}"
OUTROOT="${OUTROOT:-$HOME/results/mergesort_paper_final}"
LOGDIR="${LOGDIR:-$HOME/runs/mergesort_paper_final_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$OUTROOT" "$LOGDIR"

NS="${NS:-1000000 2000000 5000000 10000000}"
PS="${PS:-2 3 4 7 8 13 16 19 23 24 32 48}"
REPS="${REPS:-7}"
CUTOFF="${CUTOFF:-512}"
SEED="${SEED:-42}"
TALM_RTS_A="${TALM_RTS_A:-256m}"

GHC_BIN="${GHC:-ghc}"
PY3="${PY3:-python3}"
N_PHYS_CORES="${N_PHYS_CORES:-24}"
N_LOG_CORES="${N_LOG_CORES:-48}"

SUCURI_ROOT="${SUCURI_ROOT:-$HOME/Sucuri}"
PYTHON_NOGIL="${PYTHON_NOGIL:-$HOME/python3.14t/bin/python3.14t}"
PYTHON_LIBDIR="${PYTHON_LIBDIR:-$HOME/python3.14t/lib}"
SUCURI_ENABLED=0
if [[ -d "$SUCURI_ROOT" && -x "$PYTHON_NOGIL" ]]; then SUCURI_ENABLED=1; fi

CSV="$OUTROOT/metrics.csv"
[[ -f "$CSV" ]] || echo "variant,N,cutoff,P,rep,seconds,checksum,expected" > "$CSV"

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
echo " MERGESORT PAPER SWEEP (LCS/attn rigor parity)"
echo "==============================================================="
echo "  NS:        $NS"
echo "  PS:        $PS"
echo "  REPS:      $REPS"
echo "  CUTOFF:    $CUTOFF  (leaf size — single super sorts CUTOFF ints)"
echo "  SEED:      $SEED"
echo "  Variants: seq_{haskell,c,rust}, ribault_{c,rust,hs}, strategies, parpseq, timely$([[ $SUCURI_ENABLED -eq 1 ]] && echo ', sucuri')"
echo "  Out: $OUTROOT"
echo "  Logs: $LOGDIR"
echo "==============================================================="

# Build seq_c / seq_rust / seq_haskell builds ONCE (they read config at runtime).
SEQDIR="$OUTROOT/seq_native"
mkdir -p "$SEQDIR"

echo "[build] seq_c"
gcc -O3 -march=native -o "$SEQDIR/ms_seq_c" "$REPO/scripts/mergesort/ms_seq.c"

echo "[build] seq_rust"
mkdir -p "$SEQDIR/rust_build"
cp "$REPO/scripts/mergesort/Cargo.toml.ms_seq" "$SEQDIR/rust_build/Cargo.toml"
cp "$REPO/scripts/mergesort/ms_seq.rs"         "$SEQDIR/rust_build/ms_seq.rs"
(cd "$SEQDIR/rust_build" && cargo build --release --quiet)
RUST_SEQ_BIN="$SEQDIR/rust_build/target/release/ms_seq_rust"

# STRAT, parpseq, seq_haskell: same standalone .hs invocations per N
# (they read config.txt at runtime); built per N below to keep obj dirs separated.

for N in $NS; do
  echo ""
  echo "########################################################"
  echo "#  N=$N    $(date)"
  echo "########################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"
  DATA="$NDIR/data"

  if [[ ! -f "$DATA/expected_checksum.txt" ]]; then
    "$PY3" "$REPO/scripts/mergesort/gen_input.py" \
      --out-dir "$DATA" --N "$N" --cutoff "$CUTOFF" --seed "$SEED" 2>&1 | tail -3
  fi
  EXPECTED="$(cat "$DATA/expected_checksum.txt")"
  echo "  Expected CHECKSUM=$EXPECTED"

  # ===== Build standalone GHC binaries (seq_haskell, strategies, parpseq) =====
  for tier in seq_haskell strategies parpseq; do
    case "$tier" in
      seq_haskell) src="ms_seq.hs"     ; pkgs="-package time -package vector -package bytestring" ;;
      strategies)  src="ms_strat.hs"   ; pkgs="-package time -package parallel -package vector -package bytestring -package containers" ;;
      parpseq)     src="ms_parpseq.hs" ; pkgs="-package time -package parallel -package vector -package bytestring -package containers" ;;
    esac
    GDIR="$NDIR/${tier}"
    mkdir -p "$GDIR/obj"
    if [[ "$tier" == "seq_haskell" ]]; then THR=""; else THR="-threaded"; fi
    "$GHC_BIN" -O2 $THR -rtsopts -dynamic $pkgs \
      -outputdir "$GDIR/obj" -o "$GDIR/$tier" \
      "$REPO/scripts/mergesort/$src" \
      >"$LOGDIR/${tier}_N${N}.build.log" 2>&1
    [[ -x "$GDIR/$tier" ]] || { echo "FATAL: $tier build failed N=$N"; cat "$LOGDIR/${tier}_N${N}.build.log"; exit 1; }
  done

  # ===== Run sequentials (P=1, taskset core 0) =====
  echo ""
  echo "  ---- Sequentials (P=1, pinned core 0) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$NDIR/seq_haskell/out_r${rep}.txt"
    taskset -c 0 "$NDIR/seq_haskell/seq_haskell" "$DATA" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_haskell  rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_haskell,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_c_N${N}_r${rep}.txt"
    taskset -c 0 "$SEQDIR/ms_seq_c" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_c        rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_c,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR/seq_rust_N${N}_r${rep}.txt"
    taskset -c 0 "$RUST_SEQ_BIN" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
    echo "    seq_rust     rep=$rep  ${secs}s  CHECKSUM=$cs"
    echo "seq_rust,$N,$CUTOFF,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  # ===== Per-P sweep over parallel variants =====
  for P in $PS; do
    CORES_P="$(pin_cores "$P")"
    echo ""
    echo "  ======== N=$N  P=$P  cores=$CORES_P ========"

    # -------- ribault_c --------
    VDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/mergesort/gen_ms_c.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    bash "$REPO/tools/build_supers_c.sh" "$VDIR/attn_c_supers.c" "$VDIR/supers" \
      >"$LOGDIR/ribault_c_N${N}_P${P}.build.log" 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"
    pushd "$REPO/TALM/asm" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$VDIR/attn_P${P}" "$VDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$VDIR/attn_P${P}.flb"
    PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" || echo 0)"
      echo "    ribault_c    rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "ribault_c,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_rust --------
    VDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/mergesort/gen_ms_rust.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/ms_rs_supers" "$VDIR/supers" \
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
      echo "ribault_rust,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_hs --------
    VDIR="$NDIR/ribault_hs_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/mergesort/gen_ms_hs.py" \
      --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    SUPERS_INJECT_FILE="$VDIR/supers_inject.hs" \
      SUPERS_GHC_PACKAGES="vector bytestring" \
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
      echo "ribault_hs,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- strategies --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/strategies/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/strategies/strategies" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    strategies   rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "strategies,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- parpseq --------
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$NDIR/parpseq/out_P${P}_r${rep}.txt"
      taskset -c "$CORES_P" "$NDIR/parpseq/parpseq" "$DATA" \
        +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    parpseq      rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "parpseq,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- timely --------
    VDIR="$NDIR/timely_P${P}"
    mkdir -p "$VDIR"
    cp "$REPO/scripts/mergesort/Cargo.toml.ms_timely" "$VDIR/Cargo.toml"
    cp "$REPO/scripts/mergesort/ms_timely.rs"          "$VDIR/ms_timely.rs"
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet \
      >"$LOGDIR/timely_N${N}_P${P}.build.log" 2>&1)
    BIN="$VDIR/target/release/ms_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" "$DATA" -w "$P" -n 1 >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
      echo "    timely       rep=$rep  ${secs}s  CHECKSUM=$cs"
      echo "timely,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- sucuri (optional) --------
    if (( SUCURI_ENABLED == 1 )); then
      VDIR="$NDIR/sucuri_P${P}"
      mkdir -p "$VDIR"
      "$PY3" "$REPO/scripts/mergesort/gen_ms_sucuri.py" \
        --project-dir "$VDIR/crate" \
        --py-driver "$VDIR/run_sucuri.py" \
        --data-dir "$DATA" >/dev/null
      (cd "$VDIR/crate" && PYO3_PYTHON="$PYTHON_NOGIL" \
        CARGO_TARGET_DIR="$VDIR/crate/target" \
        cargo build --release --quiet \
        >"$LOGDIR/sucuri_N${N}_P${P}.build.log" 2>&1)
      RUST_SO="$VDIR/crate/target/release/libsucuri_ms.so"
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
          echo "sucuri,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      fi
    fi
  done
done

echo ""
echo "============================================================="
echo " MERGESORT SWEEP COMPLETE  $(date)"
echo " CSV: $CSV"
echo "============================================================="
wc -l "$CSV"
awk -F, 'NR>1{print $1}' "$CSV" | sort | uniq -c
