#!/usr/bin/env bash
# Master sweep for the N-Queens paper benchmark.
#
# Algorithm: classic recursive backtracking. gen_input.py expands
# the search tree to a fixed depth `cutoff` (default 5) and emits
# one leaf-state per surviving prefix in states.bin. All variants
# read these prefixes and run a sequential `solve_sub` over each;
# parallel variants distribute prefixes across workers.
#
# Cross-validation: total count of solutions matches OEIS A000170.
#   N=11: 2680   N=12: 14200   N=13: 73712   N=14: 365596   N=15: 2279184

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

SUCURI_ROOT="${SUCURI_ROOT:-$HOME/Sucuri}"
PYTHON_NOGIL="${PYTHON_NOGIL:-$HOME/python3.14t/bin/python3.14t}"
PYTHON_LIBDIR="${PYTHON_LIBDIR:-$HOME/python3.14t/lib}"
SUCURI_ENABLED=0
[[ -d "$SUCURI_ROOT" && -x "$PYTHON_NOGIL" ]] && SUCURI_ENABLED=1

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
echo " N-QUEENS PAPER SWEEP"
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

  # Build standalone GHC binaries.
  for tier in seq_haskell strategies parpseq; do
    case "$tier" in
      seq_haskell) src="nq_seq.hs"     ; pkgs="-package time -package vector -package bytestring" ;;
      strategies)  src="nq_strat.hs"   ; pkgs="-package time -package parallel -package vector -package bytestring" ;;
      parpseq)     src="nq_parpseq.hs" ; pkgs="-package time -package parallel -package vector -package bytestring" ;;
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

  for P in $PS; do
    CORES_P="$(pin_cores "$P")"
    echo "  ==== N=$N P=$P cores=$CORES_P ===="

    # ribault_c
    VDIR="$NDIR/ribault_c_P${P}"; mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/nqueens/gen_nq_c.py" --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    bash "$REPO/tools/build_supers_c.sh" "$VDIR/attn_c_supers.c" "$VDIR/supers" >"$LOGDIR/ribault_c_N${N}_P${P}.build.log" 2>&1
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
      echo "    ribault_c    rep=$rep ${secs}s cs=$cs"
      echo "ribault_c,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # ribault_rust
    VDIR="$NDIR/ribault_rust_P${P}"; mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/nqueens/gen_nq_rust.py" --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    CARGO_TARGET_DIR_RUST="$VDIR/cargo_target" \
      bash "$REPO/tools/build_supers_rust.sh" "$VDIR/nq_rs_supers" "$VDIR/supers" \
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
      echo "    ribault_rust rep=$rep ${secs}s cs=$cs"
      echo "ribault_rust,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

    # ribault_hs
    VDIR="$NDIR/ribault_hs_P${P}"; mkdir -p "$VDIR/supers"
    "$PY3" "$REPO/scripts/nqueens/gen_nq_hs.py" --out-dir "$VDIR" --data-dir "$DATA" >/dev/null
    SUPERS_INJECT_FILE="$VDIR/supers_inject.hs" \
      SUPERS_GHC_PACKAGES="vector bytestring" \
      CFLAGS="$SUPERS_CFLAGS" \
      bash "$REPO/tools/build_supers.sh" "$VDIR/attn.hsk" "$VDIR/supers/Supers.hs" \
      >"$LOGDIR/ribault_hs_N${N}_P${P}.build.log" 2>&1
    LIBSUP="$VDIR/supers/libsupers.so"; LIBDIR="$(dirname "$LIBSUP")"; GHCDEPS="$LIBDIR/ghc-deps"
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
      echo "    ribault_hs   rep=$rep ${secs}s cs=$cs"
      echo "ribault_hs,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
    done

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

    if (( SUCURI_ENABLED == 1 )); then
      VDIR="$NDIR/sucuri_P${P}"; mkdir -p "$VDIR"
      "$PY3" "$REPO/scripts/nqueens/gen_nq_sucuri.py" \
        --project-dir "$VDIR/crate" --py-driver "$VDIR/run_sucuri.py" --data-dir "$DATA" >/dev/null
      (cd "$VDIR/crate" && PYO3_PYTHON="$PYTHON_NOGIL" \
        CARGO_TARGET_DIR="$VDIR/crate/target" cargo build --release --quiet \
        >"$LOGDIR/sucuri_N${N}_P${P}.build.log" 2>&1)
      RUST_SO="$VDIR/crate/target/release/libsucuri_nq.so"
      if [[ -f "$RUST_SO" ]]; then
        for ((rep=1; rep<=REPS; rep++)); do
          OUT="$VDIR/out_r${rep}.txt"
          SUCURI_ROOT="$SUCURI_ROOT" LD_LIBRARY_PATH="$PYTHON_LIBDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
            PYTHON_GIL=0 taskset -c "$CORES_P" \
            "$PYTHON_NOGIL" "$VDIR/run_sucuri.py" --rust-so "$RUST_SO" --data-dir "$DATA" --workers "$P" >"$OUT" 2>/dev/null
          cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" || echo 0)"
          secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT" || echo 0)"
          echo "    sucuri       rep=$rep ${secs}s cs=$cs"
          echo "sucuri,$N,$CUTOFF,$P,$rep,$secs,$cs,$EXPECTED" >> "$CSV"
        done
      fi
    fi
  done
done

echo ""
echo "============= N-QUEENS SWEEP COMPLETE $(date) ============="
wc -l "$CSV"
