#!/usr/bin/env bash
set -euo pipefail

# ================= args =================
START_N=0; STEP=0; N_MAX=0; REPS=1
PROCS_CSV=""; VARIANTS=""
INTERP=""; ASM_ROOT=""; CODEGEN_ROOT=""
OUTROOT=""; VEC_MODE="range"; PLOTS="yes"; TAG="super_v_asm"
AP_TIMEOUT="${AP_TIMEOUT:-15}"   # segundos para timeout do autoplace

usage(){ echo "uso: $0 --start-N A --step B --n-max C --reps R --procs \"1,2,...\" --variants \"asm super\" --interp PATH --asm-root PATH --codegen PATH --outroot PATH [--vec range|rand] [--plots yes|no]"; exit 2; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --start-N) START_N="$2"; shift 2;;
    --step) STEP="$2"; shift 2;;
    --n-max) N_MAX="$2"; shift 2;;
    --reps) REPS="$2"; shift 2;;
    --procs) PROCS_CSV="$2"; shift 2;;
    --variants) VARIANTS="$2"; shift 2;;
    --interp) INTERP="$2"; shift 2;;
    --asm-root) ASM_ROOT="$2"; shift 2;;
    --codegen) CODEGEN_ROOT="$2"; shift 2;;
    --outroot) OUTROOT="$2"; shift 2;;
    --vec) VEC_MODE="$2"; shift 2;;
    --plots) PLOTS="$2"; shift 2;;
    --tag) TAG="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$START_N$STEP$N_MAX$REPS$PROCS_CSV$VARIANTS$INTERP$ASM_ROOT$CODEGEN_ROOT$OUTROOT" ]] || usage
[[ -x "$INTERP" ]] || { echo "interp não executável: $INTERP"; exit 1; }
[[ -f "$ASM_ROOT/assembler.py" && -f "$ASM_ROOT/scheduler.py" ]] || { echo "ASM_ROOT inválido: $ASM_ROOT"; exit 1; }
[[ -x "$CODEGEN_ROOT/codegen" ]] || { echo "codegen não encontrado em $CODEGEN_ROOT/codegen"; exit 1; }
[[ -x "$CODEGEN_ROOT/supersgen" ]] || { echo "supersgen não encontrado em $CODEGEN_ROOT/supersgen"; exit 1; }

MS_DIR="$(cd "$(dirname "$0")" && pwd)"
GEN_PY="$MS_DIR/_ms_gen_input.py"
PLOT_PY="$MS_DIR/plot.py"
PY2="${PY2:-python2}"

# ======= OUTROOT limpo SEMPRE =======
if [[ -e "$OUTROOT" ]]; then rm -rf "$OUTROOT"; fi
mkdir -p "$OUTROOT"

METRICS_CSV="$OUTROOT/metrics_${TAG}.csv"
echo "variant,N,P,rep,seconds" > "$METRICS_CSV"

IFS=',' read -r -a PROCS <<< "$PROCS_CSV"
read -r -a VARIANTS_ARR <<< "$VARIANTS"

# ======= normaliza aspas do scheduler.py (remove \" -> ") =======
fix_scheduler_quotes() {
  local sch="$ASM_ROOT/scheduler.py"
  python3 - "$sch" <<'PY'
import sys
p=sys.argv[1]
try:
    s=open(p,'r',encoding='utf-8',errors='ignore').read()
except FileNotFoundError:
    sys.exit(0)
t=s.replace('\\"','"')
if t!=s:
    open(p,'w',encoding='utf-8').write(t)
    print("[fix ] scheduler.py: aspas normalizadas", file=sys.stderr)
PY
}
fix_scheduler_quotes

# ================= helpers =================
gen_hsk() {
  local variant="$1" N="$2" P="$3" out_hsk="$4"
  echo "[build_fl] generating HSK (N=${N}, variant=${variant}, threads=${P})"
  python3 "$GEN_PY" --out "$out_hsk" --variant "$variant" --N "$N" --P "$P" --vec "$VEC_MODE"
}

build_fl() {
  local hsk="$1" fl="$2"
  echo "[talm ] codegen $hsk -> $fl"
  "$CODEGEN_ROOT/codegen" "$hsk" > "$fl"
}

build_supers_local() {
  local hsk="$1" case_dir="$2"
  local stage="$case_dir/supers/_stage"
  mkdir -p "$stage"
  echo "[supers] building libsupers para $(basename "$hsk") (dentro de results)" >&2

  # gera Supers.hs
  "$CODEGEN_ROOT/supersgen" "$hsk" > "$stage/Supers.hs"

  pushd "$stage" >/dev/null
    # compila C stub com includes do GHC; redireciona stdout para stderr
    GHC_LIBDIR="$(ghc --print-libdir)"
    if ! ghc -c -optc-fPIC "$CODEGEN_ROOT/tools/supers_rts_init.c" -o supers_rts_init.o 1>&2; then
      gcc -c -fPIC -I"$GHC_LIBDIR/include" "$CODEGEN_ROOT/tools/supers_rts_init.c" -o supers_rts_init.o 1>&2
    fi

    # linka lib; logs em stderr
    ghc -O2 -shared -dynamic -fPIC -no-hs-main \
        -hide-all-packages \
        -package base -package ghc-prim -package integer-gmp -package ghc-bignum \
        -no-user-package-db -package-env - \
        -optl -Wl,--disable-new-dtags \
        -optl -Wl,-z,noexecstack \
        -optl -Wl,-rpath,'$ORIGIN/ghc-deps:$ORIGIN' \
        -optl -Wl,--no-as-needed \
        Supers.hs supers_rts_init.o -o libsupers.so 1>&2

    mkdir -p ghc-deps
    cp "$GHC_LIBDIR"/rts/libHSrts*.so ghc-deps/ 2>/dev/null || true
    for d in ghc-prim-* base-* integer-gmp-* ghc-bignum-*; do
      test -d "$GHC_LIBDIR/$d" || continue
      cp "$GHC_LIBDIR/$d"/lib*.so ghc-deps/ 2>/dev/null || true
    done
    for g in /usr/lib64/libgmp.so.10 /usr/lib/libgmp.so.10; do
      [[ -f "$g" ]] && cp "$g" ghc-deps/ && break
    done
  popd >/dev/null

  [[ -f "$stage/libsupers.so" ]] || { echo "ERRO: libsupers.so não gerada" >&2; exit 1; }

  # empacota no case_dir, sem depender de rsync; todos logs -> stderr
  rm -rf "$case_dir/supers/pkg"
  mkdir -p "$case_dir/supers/pkg"
  (cd "$stage" && cp -a . "$case_dir/supers/pkg/") 1>&2

  # único STDOUT desta função: o caminho final da .so
  echo "$case_dir/supers/pkg/libsupers.so"
}

have_timeout() {
  command -v timeout >/dev/null 2>&1 || command -v /usr/bin/timeout >/dev/null 2>&1
}
timeout_cmd() {
  if command -v timeout >/dev/null 2>&1; then timeout "$@"; else /usr/bin/timeout "$@"; fi
}

assemble_with_place() {
  local fl="$1" prefix="$2" P="$3"
  pushd "$ASM_ROOT" >/dev/null

    # 1) baseline SEMPRE (pra garantir .flb e extrair N de tasks)
    echo "[asm  ] baseline sem -a (para obter N de tasks)" >&2
    "$PY2" assembler.py -o "$prefix" "$fl" 1>&2

    local nt; nt=$(head -n1 "${prefix}.pla" | tr -d '\r')
    [[ "$nt" =~ ^[0-9]+$ ]] || { echo "ERRO: primeira linha de ${prefix}.pla inválida" >&2; popd >/dev/null; exit 1; }
    cp "${prefix}.pla" "${prefix}.pla.base"

    if [[ "$P" -le 1 ]]; then
      echo "[asm  ] P=1: mantendo mapeamento sequencial (todos em 0)" >&2
      awk -v N="$nt" 'BEGIN{print N} NR>1{print 0}' "${prefix}.pla.base" > "${prefix}.pla"
      popd >/dev/null
      echo "${prefix}.flb|${prefix}.pla"
      return
    fi

    # 2) tenta autoplace, com timeout
    echo "[asm  ] autoplace -n $P (timeout=${AP_TIMEOUT}s)" >&2
    set +e
    if have_timeout; then
      timeout_cmd -k 5 ${AP_TIMEOUT} "$PY2" assembler.py -a -n "$P" -o "$prefix" "$fl" 1>&2
      rc=$?; [[ $rc -eq 124 ]] && { echo "[asm  ] autoplace: timeout após ${AP_TIMEOUT}s" >&2; rc=1; }
    else
      "$PY2" assembler.py -a -n "$P" -o "$prefix" "$fl" 1>&2; rc=$?
    fi
    set -e

    if [[ $rc -eq 0 && -f "${prefix}_auto.pla" ]]; then
      mv "${prefix}_auto.pla" "${prefix}.pla"
      echo "[asm  ] usando placement automático (${prefix}.pla)" >&2
    else
      # 3) fallback: particionamento em BLOCOS contíguos (melhor localidade que RR)
      echo "[pla ] fallback CHUNK: ntasks=${nt}, P=${P}" >&2
      awk -v P="$P" -v N="$nt" '
        BEGIN{print N}
        NR>1{
          i=NR-2;    # 0-based
          printf "%d\n", int((i*P)/N)
        }' "${prefix}.pla.base" > "${prefix}.pla"
    fi

  popd >/dev/null
  # stdout limpo para o caller:
  echo "${prefix}.flb|${prefix}.pla"
}

run_interp_time() {
  local P="$1" flb="$2" pla="$3" lib="${4:-}"
  local t0 t1
  t0=$(date +%s%N)
  if [[ -n "$lib" ]]; then
    "$INTERP" "$P" "$flb" "$pla" "$lib" >/dev/null 2>&1
  else
    "$INTERP" "$P" "$flb" "$pla" >/dev/null 2>&1
  fi
  t1=$(date +%s%N)
  awk -v A="$t0" -v B="$t1" 'BEGIN{printf "%.6f",(B-A)/1e9}'
}

# ================= main =================
for variant in "${VARIANTS_ARR[@]}"; do
  N="$START_N"
  while [[ "$N" -le "$N_MAX" ]]; do
    for P in "${PROCS[@]}"; do
      CASE_DIR="$OUTROOT/$variant/N_${N}/P_${P}"
      mkdir -p "$CASE_DIR"
      HSK="$CASE_DIR/mergesort_${variant}_N${N}_P${P}.hsk"
      FL="$CASE_DIR/mergesort_${variant}_N${N}_P${P}.fl"
      PREFIX="$CASE_DIR/mergesort_${variant}_N${N}_P${P}"

      gen_hsk "$variant" "$N" "$P" "$HSK"
      [[ -f "$HSK" ]] || { echo "ERRO: HSK não gerado: $HSK"; exit 1; }

      build_fl "$HSK" "$FL"
      [[ -f "$FL" ]] || { echo "ERRO: FL não gerado: $FL"; exit 1; }

      LIBSUP=""
      if [[ "$variant" == "super" ]]; then
        # captura SÓ a última linha (o caminho), logs vão em stderr
        LIBSUP="$(build_supers_local "$HSK" "$CASE_DIR" | tail -n1)"
        [[ -f "$LIBSUP" ]] || { echo "ERRO: libsupers.so não encontrada: $LIBSUP"; exit 1; }
      fi

      IFS='|' read -r FLB PLA < <(assemble_with_place "$FL" "$PREFIX" "$P")
      [[ -f "$FLB" && -f "$PLA" ]] || { echo "ERRO: FLB/PLA não gerados"; exit 1; }

      for ((rep=1; rep<=REPS; rep++)); do
        secs="$(run_interp_time "$P" "$FLB" "$PLA" "$LIBSUP")"
        echo "variant=${variant}, N=${N}, P=${P}, rep=${rep}, secs=${secs}"
        echo "${variant},${N},${P},${rep},${secs}" >> "$METRICS_CSV"
      done
    done
    N=$((N + STEP))
  done
done

# ======= plots =======
if [[ "$PLOTS" == "yes" ]]; then
  python3 "$PLOT_PY" --metrics "$METRICS_CSV" --outdir "$OUTROOT" --tag "$TAG"
fi

echo "[DONE] resultados em: $OUTROOT"
