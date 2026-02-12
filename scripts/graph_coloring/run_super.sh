#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Ribault/TALM Graph Coloring Benchmark Runner
# ============================================================
#
# This script runs the graph coloring benchmark using Ribault's
# dataflow execution model with GHC-compiled superinstructions.
#
# Pipeline:
#   1. Generate .hsk file (gen_graph_input.py)
#   2. Compile to dataflow IR (codegen)
#   3. Assemble to bytecode (assembler.py)
#   4. Build superinstructions (build_supers.sh -> libsupers.so)
#   5. Execute via interpreter (interp)
#
# The dataflow model provides TRUE parallelism:
#   - All parallel operations encoded at compile time
#   - Binary merge tree for parallel reduction
#   - No spark scheduling overhead
#   - Work dispatched immediately to workers
#
# This is in contrast to GHC's spark-based speculative parallelism,
# which often fails to scale for fine-grained workloads.
#
# Usage:
#   bash run_super.sh \
#     --N "1000,5000" \
#     --procs "1,2,4,8" \
#     --reps 3 \
#     --interp /path/to/interp \
#     --asm-root /path/to/TALM/asm \
#     --codegen /path/to/Ribault \
#     --outroot ./results
#
# ============================================================

N_CSV=""; REPS=1
PROCS_CSV=""; EDGE_PROB="0.001"; SEED="42"
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; TAG="gc_super"
PY2="${PY2:-python3}"
PY3="${PY3:-python3}"
PLACE_MODE="${PLACE_MODE:-rr}"
SUPERS_FIXED="${SUPERS_FIXED:-}"

usage(){
  echo "Usage: $0 --N \"10000,50000,...\" --reps R --procs \"1,2,...\" \\"
  echo "          --interp PATH --asm-root PATH --codegen PATH --outroot DIR"
  echo "          [--edge-prob P] [--seed S] [--tag TAG]"
  echo ""
  echo "env: PLACE_MODE=rr|chunk  SUPERS_FIXED=/abs/path  PY2, PY3"
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --N)         N_CSV="$2"; shift 2;;
    --reps)      REPS="$2"; shift 2;;
    --procs)     PROCS_CSV="$2"; shift 2;;
    --edge-prob) EDGE_PROB="$2"; shift 2;;
    --seed)      SEED="$2"; shift 2;;
    --interp)    INTERP="$2"; shift 2;;
    --asm-root)  ASM_ROOT="$2"; shift 2;;
    --codegen)   CODEGEN_ROOT="$2"; shift 2;;
    --outroot)   OUTROOT="$2"; shift 2;;
    --tag)       TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$N_CSV" && -n "$PROCS_CSV" && -n "$INTERP" && -n "$ASM_ROOT" && -n "$CODEGEN_ROOT" && -n "$OUTROOT" ]] || usage

IFS=',' read -r -a NS    <<< "$N_CSV"
IFS=',' read -r -a PROCS <<< "$PROCS_CSV"

echo "[env ] PY3=${PY3} ; PY2=${PY2} ; N=${N_CSV} (${#NS[@]} values)"

[[ -x "$INTERP" ]] || { echo "[ERROR] interp not executable: $INTERP"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" ]] || { echo "[ERROR] ASM_ROOT invalid: $ASM_ROOT"; exit 1; }
if [[ -x "$CODEGEN_ROOT/codegen" ]]; then
  CODEGEN="${CODEGEN_ROOT}/codegen"
else
  echo "[ERROR] codegen not found in: $CODEGEN_ROOT"; exit 1
fi
echo "[talm] using codegen: $CODEGEN"

BUILD_SUPERS="${CODEGEN_ROOT}/tools/build_supers.sh"
[[ -f "$BUILD_SUPERS" ]] || BUILD_SUPERS="$(dirname "$0")/../../tools/build_supers.sh"
[[ -f "$BUILD_SUPERS" ]] || { echo "[ERROR] build_supers.sh not found"; exit 1; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$SCRIPT_DIR/gen_graph_input.py"
echo "[hsk ] using generator: $GEN_PY"

mkdir -p "$OUTROOT"
METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,edge_prob,seed,rep,seconds,colors,valid,rc" > "$METRICS_CSV"

# ---------- helpers ----------
abspath(){ local d b; d="$(cd "$(dirname "$1")" && pwd)"; b="$(basename "$1")"; printf "%s/%s" "$d" "$b"; }

gen_hsk() {
  local N="$1" P="$2" out_hsk="$3"
  mkdir -p "$(dirname "$out_hsk")"
  "$PY3" "$GEN_PY" --out "$out_hsk" --N "$N" --P "$P" --edge-prob "$EDGE_PROB" --seed "$SEED"
}

build_fl() {
  local hsk="$1" fl="$2"
  mkdir -p "$(dirname "$fl")"
  "$CODEGEN" "$hsk" > "$fl"
  [[ -s "$fl" ]] || { echo "[ERROR] .fl empty: $fl"; exit 1; }
}

assemble_baseline() {
  local fl_abs="$1" prefix_abs="$2"
  pushd "$ASM_ROOT" >/dev/null
    "$PY2" assembler.py -o "$prefix_abs" "$fl_abs" >/dev/null
  popd >/dev/null
  [[ -f "${prefix_abs}.flb" && -f "${prefix_abs}.pla" ]] || { echo "[ERROR] baseline failed"; exit 1; }
  cp -f "${prefix_abs}.pla" "${prefix_abs}.pla.base"
}

rewrite_pla_manual() {
  local prefix_abs="$1" P="$2" mode="$3"
  local base="${prefix_abs}.pla.base"
  local pla="${prefix_abs}.pla"
  local nt; nt="$(head -n1 "$base" | tr -d '\r')"
  [[ "$nt" =~ ^[0-9]+$ ]] || { echo "[ERROR] invalid pla header"; exit 1; }

  if [[ "$P" -le 1 ]]; then
    awk 'NR==1{print; next} {print 0}' "$base" > "$pla"
    return
  fi

  case "$mode" in
    rr|RR)
      awk -v P="$P" 'NR==1{print; next} {i=NR-2; print (i%P)}' "$base" > "$pla"
      ;;
    chunk|CHUNK)
      awk -v P="$P" -v N="$nt" 'NR==1{print; next} {i=NR-2; printf "%d\n", int((i*P)/N)}' "$base" > "$pla"
      ;;
    *) echo "[ERROR] invalid PLACE_MODE: $mode"; exit 1;;
  esac
}

run_interp() {
  local P="$1" flb_abs="$2" pla_abs="$3" lib="${4:-}" case_dir="$5"
  local logs="$case_dir/logs"; mkdir -p "$logs"
  local outlog="$logs/run.out" errlog="$logs/run.err"

  local rc=0
  if [[ -n "$lib" ]]; then
    local libdir; libdir="$(dirname "$lib")"
    local ghcdeps="$libdir/ghc-deps"
    SUPERS_FORCE_PAR=1 NUM_CORES="$P" \
      LD_LIBRARY_PATH="$libdir:$ghcdeps" \
      "$INTERP" "$P" "$flb_abs" "$pla_abs" "$lib" >"$outlog" 2>"$errlog" &
  else
    "$INTERP" "$P" "$flb_abs" "$pla_abs" >"$outlog" 2>"$errlog" &
  fi
  local pid=$!
  if ! wait "$pid"; then rc=$?; fi

  # Extract EXEC_TIME_S from stderr
  local secs="NaN"
  for f in "$errlog" "$outlog"; do
    if [[ -f "$f" ]]; then
      local et; et="$(grep -oP 'EXEC_TIME_S \K[0-9.]+' "$f" 2>/dev/null || true)"
      [[ -n "$et" ]] && { secs="$et"; break; }
    fi
  done

  # Extract colors and valid from stdout (first two numbers printed)
  local colors="0" valid="0"
  if [[ -f "$outlog" && "$rc" -eq 0 ]]; then
    colors="$(sed -n '1p' "$outlog" | tr -cd '0-9' || echo 0)"
    valid="$(sed -n '2p' "$outlog" | tr -cd '0-9' || echo 0)"
    [[ -z "$colors" ]] && colors="0"
    [[ -z "$valid" ]] && valid="0"
    if [[ "$valid" != "1" ]]; then
      rc=99
    fi
  fi

  printf "%s %s %s %d" "$secs" "$colors" "$valid" "$rc"
}

# Detect GHC RTS include path for HsFFI.h
detect_ghc_rts_include() {
  local ghc_ver; ghc_ver="$(ghc --numeric-version 2>/dev/null || echo "")"
  if [[ -z "$ghc_ver" ]]; then return; fi
  local cands=(
    "/usr/lib/ghc-${ghc_ver}/lib/x86_64-linux-ghc-${ghc_ver}/rts-"*"/include"
    "$HOME/.ghcup/ghc/${ghc_ver}/lib/ghc-${ghc_ver}/lib/x86_64-linux-ghc-${ghc_ver}/rts-"*"/include"
  )
  for d in "${cands[@]}"; do
    if [[ -d "$d" && -f "$d/HsFFI.h" ]]; then
      echo "$d"
      return
    fi
  done
}

# Build supers for N. Prints absolute path to supers dir.
get_supers_dir() {
  local N="$1"
  local d="$OUTROOT/supers_cache/N_${N}"
  if [[ -f "$d/libsupers.so" ]]; then
    d="$(cd "$d" && pwd)"
    echo "$d"
    return
  fi
  mkdir -p "$d"
  d="$(cd "$d" && pwd)"
  echo "[sup ] building supers for N=${N}..." >&2
  "$PY3" "$GEN_PY" --out "$d/representative.hsk" --N "$N" --P 1 --edge-prob "$EDGE_PROB" --seed "$SEED" >&2

  # Set CFLAGS to include GHC RTS headers
  local rts_inc; rts_inc="$(detect_ghc_rts_include)"
  local old_cflags="${CFLAGS:-}"
  if [[ -n "$rts_inc" ]]; then
    export CFLAGS="-O2 -fPIC -I$rts_inc"
  fi

  bash "$BUILD_SUPERS" "$d/representative.hsk" "$d/Supers.hs" >&2

  # Restore CFLAGS
  if [[ -n "$old_cflags" ]]; then
    export CFLAGS="$old_cflags"
  else
    unset CFLAGS
  fi

  [[ -f "$d/libsupers.so" ]] || { echo "[ERROR] super build failed for N=${N}" >&2; exit 1; }
  echo "[sup ] built: $d/libsupers.so" >&2
  echo "$d"
}

resolve_lib() {
  local N="$1"
  if [[ -n "$SUPERS_FIXED" ]]; then
    abspath "$SUPERS_FIXED/libsupers.so"
  else
    local d; d="$(get_supers_dir "$N")"
    echo "$d/libsupers.so"
  fi
}

# ----------------- main -----------------
TOTAL_RUNS=$(( ${#NS[@]} * ${#PROCS[@]} * REPS ))
RUN_NUM=0

for N in "${NS[@]}"; do
  echo ""
  echo "======== N=${N} ========"

  # Pre-build supers for this N
  if [[ -z "$SUPERS_FIXED" ]]; then
    get_supers_dir "$N" > /dev/null
  fi

  for P in "${PROCS[@]}"; do
    CASE_DIR="$OUTROOT/super/N_${N}/P_${P}"
    mkdir -p "$CASE_DIR"
    HSK="$CASE_DIR/graph_color.hsk"
    FL="$CASE_DIR/graph_color.fl"
    PREFIX="$CASE_DIR/graph_color"

    gen_hsk "$N" "$P" "$HSK"
    build_fl "$HSK" "$FL"

    FL_ABS="$(abspath "$FL")"
    PREFIX_ABS="$(abspath "$PREFIX")"

    assemble_baseline "$FL_ABS" "$PREFIX_ABS"
    rewrite_pla_manual "$PREFIX_ABS" "$P" "$PLACE_MODE"

    LIBSUP="$(resolve_lib "$N")"

    for ((rep=1; rep<=REPS; rep++)); do
      RUN_NUM=$((RUN_NUM + 1))
      set +e
      out="$(run_interp "$P" "${PREFIX_ABS}.flb" "${PREFIX_ABS}.pla" "$LIBSUP" "$CASE_DIR")"
      st=$?
      set -e
      secs="NaN"; colors="0"; valid="0"; rc=999
      if [[ $st -eq 0 ]]; then
        read -r secs colors valid rc <<< "$out" || { secs="NaN"; colors="0"; valid="0"; rc=998; }
      fi

      valid_str="False"
      [[ "$valid" == "1" ]] && valid_str="True"

      echo "[${RUN_NUM}/${TOTAL_RUNS}] N=${N} P=${P} rep=${rep} -> ${secs}s colors=${colors} valid=${valid_str} rc=${rc}"
      echo "super,${N},${P},${EDGE_PROB},${SEED},${rep},${secs},${colors},${valid_str},${rc}" >> "$METRICS_CSV"
    done
  done
done

echo ""
echo "[DONE] ${TOTAL_RUNS} runs; metrics: $METRICS_CSV"
