#!/usr/bin/env bash
# Master sweep for the attention end-to-end paper benchmark.
#
# Variants (LCS rigor parity):
#   Sequentials (per-language denominators, REPS=7 each, pinned to core 0):
#     seq_haskell  : monolithic GHC, Data.Vector.Storable.Mutable, +RTS -A256m
#     seq_c        : monolithic gcc -O3 -march=native, raw doubles
#     seq_rust     : monolithic rustc release, raw pointers + unsafe inner loops
#   Parallels (REPS=7 each, taskset 0..P-1):
#     ribault_c    : Trebuchet + C super body
#     ribault_rust : Trebuchet + Rust super body
#     ribault_hs   : Trebuchet + Haskell super body (Data.Vector.Storable.Mutable)
#     strategies   : GHC Strategies (parList rseq) over K row-blocks
#     timely       : Timely Dataflow 2-epoch driver
#     sucuri       : Tiago's pyDF.DFGraph (free-threaded Python 3.14t) + Rust PyO3
#                    (skipped if SUCURI_ROOT or PYTHON_NOGIL unset)
#
# Per-language baseline policy (mirrors lcs_paper_final):
#   Ribault-Hs, Strat            -> seq_haskell   (Haskell tier)
#   Ribault-C                    -> seq_c         (C tier)
#   Ribault-Rust, Timely, Sucuri -> seq_rust      (Rust tier)
#
# CSV schema: variant,N,D,n_heads,n_blocks,P,rep,seconds,checksum,expected
#   (sequentials have n_blocks=1 and P=1; expected is the numpy ref checksum.)
#
# Cross-validation: every measured run prints CHECKSUM=<x>; we record it
# in-line so a downstream script can flag any mismatch against expected.
#
# REPS=7, REPS=3 only for exploratory/sondagem runs (NOT paper data).

set -uo pipefail
export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

REPO="${REPO:-$HOME/Ribault}"
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
TALM_RTS_A="${TALM_RTS_A:-256m}"

GHC_BIN="${GHC:-ghc}"
PY3="${PY3:-python3}"
N_PHYS_CORES="${N_PHYS_CORES:-24}"
N_LOG_CORES="${N_LOG_CORES:-48}"

# Sucuri (optional — runs only if both env vars present + binaries exist)
SUCURI_ROOT="${SUCURI_ROOT:-$HOME/Sucuri}"
PYTHON_NOGIL="${PYTHON_NOGIL:-$HOME/python3.14t/bin/python3.14t}"
PYTHON_LIBDIR="${PYTHON_LIBDIR:-$HOME/python3.14t/lib}"
SUCURI_ENABLED=0
if [[ -d "$SUCURI_ROOT" && -x "$PYTHON_NOGIL" ]]; then
  SUCURI_ENABLED=1
fi

CSV="$OUTROOT/metrics.csv"
[[ -f "$CSV" ]] || echo "variant,N,D,n_heads,n_blocks,P,rep,seconds,checksum,expected" > "$CSV"

# ---- detect HsFFI include for build_supers ----
GHC_VER="$("$GHC_BIN" --numeric-version 2>/dev/null || echo "unknown")"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir 2>/dev/null || echo "")"
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

pin_cores() {
  local p="$1"
  if (( p > N_LOG_CORES )); then echo "0-$((N_LOG_CORES - 1))"; else echo "0-$((p - 1))"; fi
}

pin_tag() {
  local p="$1"
  if (( p <= N_PHYS_CORES )); then echo "physical, no HT"
  else echo "oversubscribed ($((p - N_PHYS_CORES)) HT siblings used)"; fi
}

echo "==============================================================="
echo " ATTENTION END-TO-END PAPER SWEEP (LCS rigor parity)"
echo "==============================================================="
echo "  NS:        $NS"
echo "  PS:        $PS"
echo "  REPS:      $REPS"
echo "  D=$D  N_HEADS=$N_HEADS  D_FF=$D_FF  VOCAB=$VOCAB  SEED=$SEED"
echo "  Variants:"
echo "    sequentials : seq_haskell, seq_c, seq_rust  (P=1, pinned core 0)"
if (( SUCURI_ENABLED == 1 )); then
  echo "    parallels   : ribault_c, ribault_rust, ribault_hs, strategies, timely, sucuri"
  echo "                  (sucuri: SUCURI_ROOT=$SUCURI_ROOT, python=$PYTHON_NOGIL)"
else
  echo "    parallels   : ribault_c, ribault_rust, ribault_hs, strategies, timely"
  echo "                  (sucuri SKIPPED: set SUCURI_ROOT + PYTHON_NOGIL to enable)"
fi
echo "  Per-language baselines: Ribault-Hs/Strat            -> seq_haskell"
echo "                         Ribault-C                    -> seq_c"
echo "                         Ribault-Rust/Timely/Sucuri   -> seq_rust"
echo "  Out:       $OUTROOT"
echo "  Logs:      $LOGDIR"
echo "  Start:     $(date)"
echo "==============================================================="

# =====================================================================
# Build seq_c and seq_rust ONCE globally (binaries take DATA_DIR argv;
# config.txt is read at runtime; no per-N rebuild needed).
# =====================================================================
SEQDIR_GLOBAL="$OUTROOT/seq_native"
mkdir -p "$SEQDIR_GLOBAL"

echo ""
echo "[build] seq_c (gcc -O3 -march=native)"
gcc -O3 -march=native -o "$SEQDIR_GLOBAL/attn_seq_c" \
    "$REPO/scripts/attention/attn_seq.c" -lm
echo "[build] seq_rust (cargo release: opt-level=3, lto=thin, codegen-units=1)"
mkdir -p "$SEQDIR_GLOBAL/rust_build"
cp "$REPO/scripts/attention/Cargo.toml.attn_seq" "$SEQDIR_GLOBAL/rust_build/Cargo.toml"
cp "$REPO/scripts/attention/attn_seq.rs"          "$SEQDIR_GLOBAL/rust_build/attn_seq.rs"
(cd "$SEQDIR_GLOBAL/rust_build" && cargo build --release --quiet)
RUST_SEQ_BIN="$SEQDIR_GLOBAL/rust_build/target/release/attn_seq_rust"
[[ -x "$RUST_SEQ_BIN" ]] || { echo "FATAL: seq_rust build failed"; exit 1; }

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

  # =====================================================================
  # Build seq_haskell once per N (constants baked at codegen time).
  # =====================================================================
  SEQ_HS_DIR="$NDIR/seq_haskell"
  mkdir -p "$SEQ_HS_DIR/obj"
  "$PY3" "$REPO/scripts/attention/gen_attn_hs_sequential.py" \
    --out "$SEQ_HS_DIR/attn_seq.hs" --data-dir "$DATA" >/dev/null
  "$GHC_BIN" -O2 -rtsopts -dynamic \
    -package time -package vector -package bytestring \
    -outputdir "$SEQ_HS_DIR/obj" -o "$SEQ_HS_DIR/attn_seq" \
    "$SEQ_HS_DIR/attn_seq.hs" >"$LOGDIR/seq_haskell_N${N}.build.log" 2>&1
  [[ -x "$SEQ_HS_DIR/attn_seq" ]] || { echo "FATAL: seq_haskell build failed for N=$N"; exit 1; }

  # =====================================================================
  # Run sequentials (P=1, pinned to core 0). REPS=7.
  # =====================================================================
  echo ""
  echo "  ---- Sequentials (P=1, pinned core 0) ----"
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQ_HS_DIR/out_r${rep}.txt"
    taskset -c 0 "$SEQ_HS_DIR/attn_seq" +RTS -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    echo "    seq_haskell  rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
    echo "seq_haskell,$N,$D,$N_HEADS,1,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR_GLOBAL/seq_c_N${N}_r${rep}.txt"
    taskset -c 0 "$SEQDIR_GLOBAL/attn_seq_c" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    echo "    seq_c        rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
    echo "seq_c,$N,$D,$N_HEADS,1,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SEQDIR_GLOBAL/seq_rust_N${N}_r${rep}.txt"
    taskset -c 0 "$RUST_SEQ_BIN" "$DATA" >"$OUT" 2>/dev/null
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
    echo "    seq_rust     rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
    echo "seq_rust,$N,$D,$N_HEADS,1,1,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
  done

  # =====================================================================
  # Parallel variants per P
  # =====================================================================
  for P in $PS; do
    N_BLOCKS=$P
    CORES_P="$(pin_cores "$P")"
    PIN_TAG="$(pin_tag "$P")"
    echo ""
    echo "  ======== N=$N  P=$P  n_blocks=$N_BLOCKS  cores=$CORES_P  ($PIN_TAG) ========"

    # -------- ribault_c --------
    VDIR="$NDIR/ribault_c_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/attention/gen_attn_c.py" \
      --out-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
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
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      set -e
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo 0)"
      echo "    ribault_c    rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "ribault_c,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_rust --------
    VDIR="$NDIR/ribault_rust_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/attention/gen_attn_rust.py" \
      --out-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/attn_rs_supers" "$VDIR/supers" \
      >"$LOGDIR/ribault_rust_N${N}_P${P}.build.log" 2>&1
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
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$(dirname "$LIBSUP")" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      set -e
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo 0)"
      echo "    ribault_rust rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "ribault_rust,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- ribault_hs (Trebuchet + Haskell super body) --------
    VDIR="$NDIR/ribault_hs_P${P}"
    mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/attention/gen_attn_hs.py" \
      --out-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
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
    FLB="$VDIR/attn_P${P}.flb"
    PLA="$VDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$VDIR/attn_P${P}.pla"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"; ERR="$VDIR/err_r${rep}.txt"
      set +e
      SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" \
        LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        taskset -c "$CORES_P" "$REPO/TALM/interp/interp" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
      set -e
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || echo 0)"
      echo "    ribault_hs   rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "ribault_hs,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- strategies (GHC parList rseq) --------
    VDIR="$NDIR/strategies_P${P}"
    mkdir -p "$VDIR/obj"
    "$PY3" "$REPO/scripts/attention/gen_attn_hs_strategies.py" \
      --out "$VDIR/attn.hs" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    "$GHC_BIN" -O2 -threaded -rtsopts -dynamic \
      -package time -package parallel -package vector -package bytestring \
      -outputdir "$VDIR/obj" -o "$VDIR/attn" "$VDIR/attn.hs" \
      >"$LOGDIR/strategies_N${N}_P${P}.build.log" 2>&1
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$VDIR/attn" +RTS -N"$P" -A"$TALM_RTS_A" -RTS >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      echo "    strategies   rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "strategies,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- timely (Timely Dataflow) --------
    VDIR="$NDIR/timely_P${P}"
    "$PY3" "$REPO/scripts/attention/gen_attn_rs_timely.py" \
      --project-dir "$VDIR" --data-dir "$DATA" --n-blocks "$N_BLOCKS" >/dev/null
    (cd "$VDIR" && CARGO_TARGET_DIR="$VDIR/target" cargo build --release --quiet \
       >"$LOGDIR/timely_N${N}_P${P}.build.log" 2>&1)
    BIN="$VDIR/target/release/attn_timely"
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$VDIR/out_r${rep}.txt"
      taskset -c "$CORES_P" "$BIN" -w "$P" -n 1 >"$OUT" 2>/dev/null
      cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
      echo "    timely       rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
      echo "timely,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # -------- sucuri (pyDF.DFGraph + Rust PyO3, free-threaded Python) --------
    if (( SUCURI_ENABLED == 1 )); then
      VDIR="$NDIR/sucuri_P${P}"
      mkdir -p "$VDIR"
      "$PY3" "$REPO/scripts/attention/gen_attn_sucuri.py" \
        --project-dir "$VDIR/crate" \
        --py-driver "$VDIR/run_sucuri.py" \
        --data-dir "$DATA" \
        --n-blocks "$N_BLOCKS" >/dev/null
      # PYO3_PYTHON pins PyO3 to the free-threaded Python so the cdylib has
      # the right ABI; otherwise the .so segfaults on load.
      (cd "$VDIR/crate" && \
         PYO3_PYTHON="$PYTHON_NOGIL" \
         CARGO_TARGET_DIR="$VDIR/crate/target" \
         cargo build --release --quiet \
         >"$LOGDIR/sucuri_N${N}_P${P}.build.log" 2>&1)
      # cargo emits libsucuri_attn.so (cdylib); rename so PyO3 importlib finds it.
      RUST_SO="$VDIR/crate/target/release/libsucuri_attn.so"
      if [[ ! -f "$RUST_SO" ]]; then
        echo "    sucuri       BUILD FAILED — see $LOGDIR/sucuri_N${N}_P${P}.build.log"
      else
        for ((rep=1; rep<=REPS; rep++)); do
          OUT="$VDIR/out_r${rep}.txt"
          set +e
          SUCURI_ROOT="$SUCURI_ROOT" \
            LD_LIBRARY_PATH="$PYTHON_LIBDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
            PYTHON_GIL=0 \
            taskset -c "$CORES_P" \
              "$PYTHON_NOGIL" "$VDIR/run_sucuri.py" \
                --rust-so "$RUST_SO" \
                --n-blocks "$N_BLOCKS" \
                --workers "$P" >"$OUT" 2>/dev/null
          set -e
          cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
          secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" 2>/dev/null || echo 0)"
          echo "    sucuri       rep=$rep  ${secs}s  CHECKSUM=$cs  (expected $EXPECTED)"
          echo "sucuri,$N,$D,$N_HEADS,$N_BLOCKS,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      fi
    fi
  done
done

echo ""
echo "============================================================="
echo " ATTENTION SWEEP COMPLETE  $(date)"
echo " CSV: $CSV"
echo "============================================================="
wc -l "$CSV"
awk -F, 'NR>1{print $1}' "$CSV" | sort | uniq -c
