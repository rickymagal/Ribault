#!/usr/bin/env bash
set -euo pipefail

# Expected environment variables from "make supers"
: "${EXE_SUPERS:?}" "${GHC:?}" "${GHC_LIBDIR:?}" "${GHC_RTS_DIR:?}" "${SUPERS_DIR:?}"
: "${DEPS_PATTERNS:?}" "${SUPERS_LINK_FLAGS:?}" "${PY_ALIAS:?}" "${PY_FIX:?}" "${TESTS:?}"
# Optional: if defined, copy a local libgmp
: "${GMP_CANDIDATES:=}"

# Tests list (no spaces in paths)
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

    # Duplicate exports: "sN" also as "super(N-1)"
    python3 "$PY_ALIAS" "$outdir/Supers.hs"

    # Small C stub to initialize the RTS inside the .so
    echo "[C    ] $outdir/supers_rts_init.o"
    cc -fPIC -O2 -I"$GHC_LIBDIR/include" \
       -c tools/supers_rts_init.c -o "$outdir/supers_rts_init.o"

    echo "[GHC  ] $sofile"
    # IMPORTANT:
    # Let GHC handle RTS linking; do NOT inject -r or explicit libHSrts here.
    # We ignore SUPERS_LINK_FLAGS because they are the source of the -r + .so conflict.
    GHC_ENVIRONMENT=- "$GHC" \
      -shared -dynamic -fPIC -no-hs-main \
      -o "$sofile" "$outdir/Supers.hs" "$outdir/supers_rts_init.o"

    mkdir -p "$depdir"

    # Copy all Haskell shared-library dependencies that may be NEEDED by the .so
    for pat in $DEPS_PATTERNS; do
      for so in "$GHC_LIBDIR"/$pat; do
        [[ -f "$so" ]] && cp -L "$so" "$depdir/"
      done
    done

    # Optionally bundle a local libgmp if a candidate was provided
    if [[ -n "$GMP_CANDIDATES" ]]; then
      for g in $GMP_CANDIDATES; do
        if [[ -f "$g" ]]; then
          cp -L "$g" "$depdir/"
          break
        fi
      done
    fi

    # Short RPATH pointing to the local bundle
    if command -v patchelf >/dev/null 2>&1; then
      patchelf --force-rpath --set-rpath '$ORIGIN/ghc-deps:$ORIGIN' "$sofile" || true
    fi

    # Ensure non-executable stack on the .so and all its deps
    echo "[STACK] clearing EXEC flag from $sofile and deps (via Python)"
    python3 "$PY_FIX" "$sofile" "$depdir"/*.so || true

  else
    rm -f "$outdir/Supers.hs.tmp" "$outdir/Supers.hs" "$sofile"
    rm -rf "$depdir"
  fi
done
