#!/usr/bin/env bash
set -euo pipefail

# Espera estas variáveis vindas do 'make supers'
: "${EXE_SUPERS:?}" "${GHC:?}" "${GHC_LIBDIR:?}" "${GHC_RTS_DIR:?}" "${SUPERS_DIR:?}"
: "${DEPS_PATTERNS:?}" "${SUPERS_LINK_FLAGS:?}" "${PY_ALIAS:?}" "${PY_FIX:?}" "${TESTS:?}"
# Opcional: se definida, copia libgmp local
: "${GMP_CANDIDATES:=}"

# Lista de testes (sem espaços nos paths)
tests=( $TESTS )

for f in "${tests[@]}"; do
  base="${f##*/}"; base="${base%.hsk}"
  outdir="${SUPERS_DIR}/$base"
  depdir="$outdir/ghc-deps"
  sofile="$outdir/libsupers.so"
  mkdir -p "$outdir"

  echo "[SUPERS] $f → $outdir/Supers.hs"
  GHC_ENVIRONMENT=- "./$EXE_SUPERS" "$f" > "$outdir/Supers.hs.tmp"

  if [[ -s "$outdir/Supers.hs.tmp" ]]; then
    mv "$outdir/Supers.hs.tmp" "$outdir/Supers.hs"

    # Duplicar exports: "sN" também como "super(N-1)"
    python3 "$PY_ALIAS" "$outdir/Supers.hs"

    # Stub para inicializar o RTS dentro da .so
    echo "[C    ] $outdir/supers_rts_init.o"
    cc -fPIC -O2 -I"$GHC_LIBDIR/include" \
       -c tools/supers_rts_init.c -o "$outdir/supers_rts_init.o"

    # Escolhe o RTS não-threaded (libHSrts-ghc*.so)
    rtsbase="$(basename "$(ls "$GHC_RTS_DIR"/libHSrts-ghc*.so | head -n1)" .so)"
    rtsbase="${rtsbase#lib}"

    echo "[GHC  ] $sofile"
    GHC_ENVIRONMENT=- "$GHC" $SUPERS_LINK_FLAGS \
      -optl -L"$GHC_RTS_DIR" -optl -l"$rtsbase" \
      -o "$sofile" "$outdir/Supers.hs" "$outdir/supers_rts_init.o"

    mkdir -p "$depdir"

    # Copia TODAS as deps Haskell que podem ser NEEDED pela .so
    for pat in $DEPS_PATTERNS; do
      for so in "$GHC_LIBDIR"/$pat; do
        [[ -f "$so" ]] && cp -L "$so" "$depdir/"
      done
    done

    # Opcional: fechar libgmp localmente (se variável tiver sido passada)
    if [[ -n "$GMP_CANDIDATES" ]]; then
      for g in $GMP_CANDIDATES; do
        [[ -f "$g" ]] && { cp -L "$g" "$depdir/"; break; }
      done
    fi

    # RPATH curto apontando p/ bundle
    if command -v patchelf >/dev/null 2>&1; then
      patchelf --force-rpath --set-rpath '$ORIGIN/ghc-deps:$ORIGIN' "$sofile" || true
    fi

    # Garante stack não-executável em TUDO (a .so e cada dependência)
    echo "[STACK] removendo EXEC de $sofile e deps (via Python)"
    python3 "$PY_FIX" "$sofile" "$depdir"/*.so || true

  else
    rm -f "$outdir/Supers.hs.tmp" "$outdir/Supers.hs" "$sofile"
    rm -rf "$depdir"
  fi
done
