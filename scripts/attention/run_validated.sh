#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Validated Attention benchmark with file-IO data pipeline
#
# Each parallel block reads Q.bin, K.bin, V.bin via BS.readFile
# (GHC heap allocation), then computes its row-block of
# O = softmax(Q * K^T / sqrt(D)) * V.
#
# Structure:
#   - Pre-generate binary Q/K/V per N
#   - Sequential baseline (P=1)
#   - Parallel: TALM, GHC Strategies, GHC par/pseq at each P
# ============================================================

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
OUTROOT="${1:-$REPO/results/attention}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

PY3="${PY3:-python3}"
INTERP="$REPO/TALM/interp/interp"
ASM_ROOT="$REPO/TALM/asm"
CODEGEN="$REPO/codegen"
BUILD_SUPERS="$REPO/tools/build_supers.sh"
GEN_DATA="$REPO/scripts/attention/gen_data.py"
GEN_TALM="$REPO/scripts/attention/gen_talm_input.py"
GEN_SEQ="$REPO/scripts/attention/gen_hs_sequential.py"
GEN_STRAT="$REPO/scripts/attention/gen_hs_strategies.py"
GEN_PARPSEQ="$REPO/scripts/attention/gen_hs_parpseq.py"

REPS=${REPS:-3}
N_FUNCS=${N_FUNCS:-14}
D=${D:-512}
PS=(${PS:-2 4 8 16})
TALM_RTS_A=${TALM_RTS_A:-256m}

NS=(${NS:-6144 6656 7168 7680 8192})

# Detect GHC
GHC_BIN="${GHC:-ghc}"
GHC_VER="$("$GHC_BIN" --numeric-version)"
GHC_LIBDIR="$("$GHC_BIN" --print-libdir)"

# Detect HsFFI include for supers build
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

validate() {
  local label="$1" file="$2" expected="$3"
  local got
  got="$(awk -F= '/^CHECKSUM=/{print $2}' "$file" 2>/dev/null || true)"
  if [[ -z "$got" ]]; then
    echo "FATAL: $label -> no CHECKSUM= found"
    exit 1
  fi
  if [[ "$got" != "$expected" ]]; then
    echo "FATAL: $label -> got $got, expected $expected"
    exit 1
  fi
  echo "  OK: CHECKSUM=$got (correct)"
}

CSV="$OUTROOT/metrics.csv"
echo "variant,N,D,n_funcs,P,rep,seconds" > "$CSV"

echo "========================================="
echo " Attention Data Pipeline Benchmark"
echo " N values: ${NS[*]}"
echo " D=$D  N_FUNCS=$N_FUNCS  P=${PS[*]}  reps=$REPS"
echo " TALM_RTS_A=$TALM_RTS_A"
echo " Output: $OUTROOT"
echo "========================================="

for N in "${NS[@]}"; do
  echo ""
  echo "############################################################"
  echo "# N=$N  (${N}x${D} attention, N_FUNCS=$N_FUNCS)"
  echo "############################################################"

  NDIR="$OUTROOT/N_${N}"
  mkdir -p "$NDIR"

  # --- Generate binary Q/K/V ---
  DATADIR="$NDIR/data"
  if [[ ! -f "$DATADIR/Q.bin" || ! -f "$DATADIR/K.bin" || ! -f "$DATADIR/V.bin" ]]; then
    "$PY3" "$GEN_DATA" --out-dir "$DATADIR" --N "$N" --D "$D"
  else
    echo "  [gen_data] reusing existing $DATADIR/{Q,K,V}.bin"
  fi

  # --- Build TALM ---
  TDIR="$NDIR/talm"
  mkdir -p "$TDIR/supers"
  "$PY3" "$GEN_TALM" --out "$TDIR/attn.hsk" --N "$N" --D "$D" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$CODEGEN" "$TDIR/attn.hsk" > "$TDIR/attn.fl" 2>/dev/null
  CFLAGS="$SUPERS_CFLAGS" SUPERS_INJECT_FILE="$TDIR/supers_inject.hs" \
    SUPERS_GHC_PACKAGES="bytestring" \
    bash "$BUILD_SUPERS" "$TDIR/attn.hsk" "$TDIR/supers/Supers.hs"
  LIBSUP="$TDIR/supers/libsupers.so"
  LIBDIR="$(dirname "$LIBSUP")"
  GHCDEPS="$LIBDIR/ghc-deps"

  # --- Build GHC Sequential ---
  SDIR="$NDIR/seq"
  mkdir -p "$SDIR/obj"
  "$PY3" "$GEN_SEQ" --out "$SDIR/attn.hs" --N "$N" --D "$D" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring \
      -outputdir "$SDIR/obj" -o "$SDIR/attn" "$SDIR/attn.hs" >/dev/null 2>&1

  # --- Build GHC Strategies ---
  GDIR="$NDIR/ghc"
  mkdir -p "$GDIR/obj"
  "$PY3" "$GEN_STRAT" --out "$GDIR/attn.hs" --N "$N" --D "$D" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package parallel -package bytestring \
      -outputdir "$GDIR/obj" -o "$GDIR/attn" "$GDIR/attn.hs" >/dev/null 2>&1

  # --- Build GHC par/pseq ---
  PDIR="$NDIR/parpseq"
  mkdir -p "$PDIR/obj"
  "$PY3" "$GEN_PARPSEQ" --out "$PDIR/attn.hs" --N "$N" --D "$D" --n-funcs "$N_FUNCS" --data-dir "$DATADIR"
  "$GHC_BIN" -O2 -threaded -rtsopts -package time -package bytestring \
      -outputdir "$PDIR/obj" -o "$PDIR/attn" "$PDIR/attn.hs" >/dev/null 2>&1

  echo "  Built all variants for N=$N"

  # ===== Sequential baseline (P=1) =====
  echo ""
  echo "  ---- Sequential Baseline (P=1) ----"
  EXPECTED=""
  for ((rep=1; rep<=REPS; rep++)); do
    OUT="$SDIR/out_r${rep}.txt"
    "$SDIR/attn" +RTS -N1 -RTS >"$OUT" 2>/dev/null
    secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
    cs="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT")"
    if [[ -z "$EXPECTED" ]]; then
      EXPECTED="$cs"
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s  (reference checksum: $EXPECTED)"
    else
      echo "  SEQ      N=$N P=1 rep=$rep -> ${secs}s"
      validate "Sequential N=$N P=1 rep=$rep" "$OUT" "$EXPECTED"
    fi
    echo "seq,$N,$D,$N_FUNCS,1,$rep,$secs" >> "$CSV"
  done

  # ===== Parallel benchmarks (P>=2) =====
  for P in "${PS[@]}"; do
    echo ""
    echo "  ---- P=$P ----"

    # --- TALM ---
    pushd "$ASM_ROOT" >/dev/null
      "$PY3" assembler.py -a -n "$P" -o "$TDIR/attn_P${P}" "$TDIR/attn.fl" >/dev/null 2>&1
    popd >/dev/null
    FLB="$TDIR/attn_P${P}.flb"
    PLA="$TDIR/attn_P${P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="$TDIR/attn_P${P}.pla"

    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$TDIR/out_P${P}_r${rep}.txt"
      ERR="$TDIR/err_P${P}_r${rep}.txt"
      for attempt in 1 2 3; do
        set +e
        SUPERS_RTS_N="$P" SUPERS_RTS_A="$TALM_RTS_A" LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
          "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$OUT" 2>"$ERR"
        rc=$?
        set -e
        if [[ $rc -eq 0 ]]; then
          got="$(awk -F= '/^CHECKSUM=/{print $2}' "$OUT" 2>/dev/null || true)"
          if [[ -n "$got" ]]; then break; fi
        fi
        echo "    WARN: TALM N=$N P=$P rep=$rep attempt=$attempt failed (rc=$rc)"
      done
      secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$ERR" 2>/dev/null || true)"
      echo "  TALM     N=$N P=$P rep=$rep -> ${secs}s"
      validate "TALM N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "super,$N,$D,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC Strategies ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$GDIR/out_P${P}_r${rep}.txt"
      "$GDIR/attn" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-Str  N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-Strategies N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "strat,$N,$D,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done

    # --- GHC par/pseq ---
    for ((rep=1; rep<=REPS; rep++)); do
      OUT="$PDIR/out_P${P}_r${rep}.txt"
      "$PDIR/attn" +RTS -N"$P" -RTS >"$OUT" 2>/dev/null
      secs="$(awk -F= '/^RUNTIME_SEC=/{print $2}' "$OUT")"
      echo "  GHC-PP   N=$N P=$P rep=$rep -> ${secs}s"
      validate "GHC-par/pseq N=$N P=$P rep=$rep" "$OUT" "$EXPECTED"
      echo "parpseq,$N,$D,$N_FUNCS,$P,$rep,$secs" >> "$CSV"
    done
  done
done

echo ""
echo "========================================="
echo " ALL RUNS PASSED VALIDATION"
echo " CSV: $CSV"
echo "========================================="
