#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# nqueens/run_talm.sh — TALM N-Queens benchmark runner
# Cutoff computed per (N, P): ceil(log_N(OVERSUB * P)), min 1
# ============================================================

N_CSV=""; REPS=1; OVERSUB=4
PROCS_CSV=""; OUTROOT=""; TAG="nq_talm"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
PY2="${PY2:-python3}"
PY3="${PY3:-python3}"

usage(){
  echo "uso: $0 --N \"8,10,...\" --reps R --procs \"1,2,...\""
  echo "        --interp PATH --asm-root PATH --codegen PATH --outroot DIR"
  echo "        [--tag TAG] [--oversub K]"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)        N_CSV="$2"; shift 2;;
    --reps)     REPS="$2"; shift 2;;
    --procs)    PROCS_CSV="$2"; shift 2;;
    --interp)   INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen)  CODEGEN_ROOT="$2"; shift 2;;
    --outroot)  OUTROOT="$2"; shift 2;;
    --tag)      TAG="$2"; shift 2;;
    --oversub)  OVERSUB="$2"; shift 2;;
    --cutoff)   shift 2;;  # ignored (auto-computed), kept for compat
    *) usage;;
  esac
done

[[ -n "$N_CSV" && -n "$PROCS_CSV" && -n "$INTERP" && -n "$ASM_ROOT" && -n "$CODEGEN_ROOT" && -n "$OUTROOT" ]] || usage

IFS=',' read -r -a NS    <<< "$N_CSV"
IFS=',' read -r -a PROCS  <<< "$PROCS_CSV"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$SCRIPT_DIR/gen_talm_input.py"
CODEGEN="$CODEGEN_ROOT/codegen"
BUILD_SUPERS="$CODEGEN_ROOT/tools/build_supers.sh"

[[ -x "$INTERP" ]]   || { echo "[ERRO] interp not executable: $INTERP"; exit 1; }
[[ -x "$CODEGEN" ]]  || { echo "[ERRO] codegen not found: $CODEGEN"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" ]] || { echo "[ERRO] assembler.py not found in: $ASM_ROOT"; exit 1; }
[[ -f "$BUILD_SUPERS" ]] || { echo "[ERRO] build_supers.sh not found: $BUILD_SUPERS"; exit 1; }

# Compute cutoff = ceil(log_N(OVERSUB * P)), minimum 1
compute_cutoff() {
  local n="$1" p="$2"
  "$PY3" -c "
import math
N, P, K = $n, $p, $OVERSUB
target = K * max(P, 1)
if N <= 1:
    print(1)
else:
    print(max(1, math.ceil(math.log(target) / math.log(N))))
"
}

echo "[env ] INTERP=${INTERP} ; CODEGEN=${CODEGEN} ; oversub=${OVERSUB}"

# Detect GHC include path for building supers
GHC_BIN="${GHC:-ghc}"
GHC_LIBDIR_RAW="$("$GHC_BIN" --print-libdir)"
GHC_VER="$("$GHC_BIN" --numeric-version)"
SUPERS_CFLAGS="${CFLAGS:-}"
if [[ -z "$SUPERS_CFLAGS" ]]; then
  HsFFI_INC=""
  for cand in \
    "$GHC_LIBDIR_RAW/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR_RAW/../lib/x86_64-linux-ghc-${GHC_VER}/rts-"*/include \
    "$GHC_LIBDIR_RAW/rts/include" \
    "$GHC_LIBDIR_RAW/include"; do
    if [[ -f "$cand/HsFFI.h" ]]; then
      HsFFI_INC="$cand"
      break
    fi
  done
  if [[ -n "$HsFFI_INC" ]]; then
    SUPERS_CFLAGS="-O2 -fPIC -I$HsFFI_INC"
    echo "[env ] HsFFI.h found at: $HsFFI_INC"
  else
    SUPERS_CFLAGS="-O2 -fPIC"
    echo "[warn] HsFFI.h not found, build_supers may fail"
  fi
fi

mkdir -p "$OUTROOT"
METRICS="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,cutoff,P,rep,seconds,rc" > "$METRICS"

declare -A NQ_EXPECTED
NQ_EXPECTED[1]=1; NQ_EXPECTED[2]=0; NQ_EXPECTED[3]=0; NQ_EXPECTED[4]=2
NQ_EXPECTED[5]=10; NQ_EXPECTED[6]=4; NQ_EXPECTED[7]=40; NQ_EXPECTED[8]=92
NQ_EXPECTED[9]=352; NQ_EXPECTED[10]=724; NQ_EXPECTED[11]=2680; NQ_EXPECTED[12]=14200
NQ_EXPECTED[13]=73712; NQ_EXPECTED[14]=365596; NQ_EXPECTED[15]=2279184

TOTAL_RUNS=$(( ${#NS[@]} * ${#PROCS[@]} * REPS ))
RUN_NUM=0

# Track which (N, cutoff) combos have been built
declare -A BUILT_CUTOFFS

for N in "${NS[@]}"; do
  expected="${NQ_EXPECTED[$N]:-}"
  [[ -n "$expected" ]] && echo "[nq  ] expected nqueens($N) = $expected"

  for P in "${PROCS[@]}"; do
    CUTOFF=$(compute_cutoff "$N" "$P")
    BUILD_KEY="${N}_${CUTOFF}"
    CASE_DIR="$OUTROOT/talm/N_${N}_C${CUTOFF}"

    # Build .hsk, .fl, supers only once per (N, cutoff)
    if [[ -z "${BUILT_CUTOFFS[$BUILD_KEY]:-}" ]]; then
      mkdir -p "$CASE_DIR/supers"
      HSK="$CASE_DIR/nqueens.hsk"
      FL="$CASE_DIR/nqueens.fl"

      "$PY3" "$GEN_PY" --out "$HSK" --N "$N" --cutoff "$CUTOFF"
      "$CODEGEN" "$HSK" > "$FL" 2>/dev/null

      echo "[sup ] building supers for N=${N} cutoff=${CUTOFF}..."
      CFLAGS="$SUPERS_CFLAGS" bash "$BUILD_SUPERS" "$HSK" "$CASE_DIR/supers/Supers.hs" 2>&1 \
        | sed 's/^/  /'

      if [[ -f "$CASE_DIR/supers/libsupers.so" ]]; then
        BUILT_CUTOFFS[$BUILD_KEY]="$CASE_DIR"
        echo "[sup ] built: $CASE_DIR/supers/libsupers.so"
      else
        echo "[ERRO] libsupers.so not found after build"
        BUILT_CUTOFFS[$BUILD_KEY]="FAILED"
      fi
    fi

    CASE_DIR="${BUILT_CUTOFFS[$BUILD_KEY]}"
    if [[ "$CASE_DIR" == "FAILED" ]]; then
      for ((rep=1; rep<=REPS; rep++)); do
        RUN_NUM=$((RUN_NUM + 1))
        echo "[${RUN_NUM}/${TOTAL_RUNS}] N=${N} C=${CUTOFF} P=${P} rep=${rep} -> NaN s rc=1 (build failed)"
        echo "super,${N},${CUTOFF},${P},${rep},NaN,1" >> "$METRICS"
      done
      continue
    fi

    FL="$CASE_DIR/nqueens.fl"
    LIBSUP="$CASE_DIR/supers/libsupers.so"

    # Assemble with P PEs
    PREFIX_P="$CASE_DIR/nqueens_P${P}"
    pushd "$ASM_ROOT" >/dev/null
      "$PY2" assembler.py -a -n "$P" -o "$PREFIX_P" "$FL" >/dev/null 2>&1
    popd >/dev/null

    FLB="${PREFIX_P}.flb"
    PLA="${PREFIX_P}_auto.pla"
    [[ -f "$PLA" ]] || PLA="${PREFIX_P}.pla"
    [[ -f "$FLB" ]] || { echo "[ERRO] .flb not found: $FLB"; continue; }

    LIBDIR="$(dirname "$LIBSUP")"
    GHCDEPS="$LIBDIR/ghc-deps"

    for ((rep=1; rep<=REPS; rep++)); do
      RUN_NUM=$((RUN_NUM + 1))
      local_rc=0
      outlog="$CASE_DIR/run_P${P}_rep${rep}.out"
      errlog="$CASE_DIR/run_P${P}_rep${rep}.err"

      set +e
      LD_LIBRARY_PATH="$LIBDIR:$GHCDEPS${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        "$INTERP" "$P" "$FLB" "$PLA" "$LIBSUP" >"$outlog" 2>"$errlog"
      local_rc=$?
      set -e

      secs="NaN"
      if [[ $local_rc -eq 0 ]]; then
        secs="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$errlog" 2>/dev/null || true)"
        [[ -z "$secs" ]] && secs="$(awk -F'=' '/^EXEC_TIME_S=/{print $2}' "$errlog" 2>/dev/null || true)"
        [[ -z "$secs" ]] && secs="$(awk -F'=' '/^EXEC_TIME_S=/{print $2}' "$outlog" 2>/dev/null || true)"

        result="$(awk -F= '/^RESULT=/{print $2}' "$outlog" 2>/dev/null || true)"
        if [[ -z "$result" ]]; then
          >&2 echo "[WARN] RESULT= missing from stdout"
        elif [[ -n "$expected" && "$result" != "$expected" ]]; then
          >&2 echo "[ERR ] WRONG nqueens($N): got '$result', expected '$expected'"
          local_rc=99
        fi
      fi
      [[ -z "$secs" ]] && secs="NaN"

      echo "[${RUN_NUM}/${TOTAL_RUNS}] N=${N} C=${CUTOFF} P=${P} rep=${rep} -> ${secs}s rc=${local_rc}"
      echo "super,${N},${CUTOFF},${P},${rep},${secs},${local_rc}" >> "$METRICS"
    done
  done
done

echo "[DONE] ${TOTAL_RUNS} runs; metrics: $METRICS"
