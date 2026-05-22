#!/usr/bin/env bash
# Build libsupers.so from a Rust crate (staticlib) + the same supers_wrappers.c
# template that build_supers_c.sh uses. The Rust crate provides strong-symbol
# overrides for s10..s13 plus the supers_hs_* no-op stubs; the wrappers.c
# provides super0..super255 thunks that unmarshall oper_t into int64_t[] and
# call sN.
#
# Usage: build_supers_rust.sh <rust_project_dir> <output_dir>
# Inputs:
#   <rust_project_dir>: Cargo project with Cargo.toml + src/lib.rs (the
#                       autogen output of gen_lcs_rust.py:emit).
# Output:
#   <output_dir>/libsupers.so (loadable by the TALM interp).
#   <output_dir>/supers_wrappers.c (the C wrapper, for inspection).
set -euo pipefail

[[ $# -eq 2 ]] || { echo "usage: $0 <rust_project_dir> <output_dir>"; exit 2; }

RUST_DIR="$1"
OUT_DIR="$2"
[[ -d "$RUST_DIR" ]] || { echo "rust project not found: $RUST_DIR"; exit 1; }
mkdir -p "$OUT_DIR"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INTERP_INC="$REPO_ROOT/TALM/interp/include"
[[ -d "$INTERP_INC" ]] || { echo "missing $INTERP_INC"; exit 1; }

# 1) Build the Rust staticlib (libsupers_rust.a).
CARGO_TARGET_DIR_RUST="${CARGO_TARGET_DIR_RUST:-$OUT_DIR/cargo_target}"
mkdir -p "$CARGO_TARGET_DIR_RUST"
CARGO_TARGET_DIR_RUST="$(cd "$CARGO_TARGET_DIR_RUST" && pwd)"
(cd "$RUST_DIR" && CARGO_TARGET_DIR="$CARGO_TARGET_DIR_RUST" cargo build --release --quiet)

RUST_A="$CARGO_TARGET_DIR_RUST/release/libsupers_rust.a"
[[ -f "$RUST_A" ]] || { echo "static lib not produced: $RUST_A"; exit 1; }

# 2) Generate supers_wrappers.c (same flavor as build_supers_c.sh, max=256).
WRAPPERS_C="$OUT_DIR/supers_wrappers.c"
{
  echo '#include <stdint.h>'
  echo '#include <stdio.h>'
  echo '#include "queue.h"'
  echo '#include "interp.h"'
  echo ''
  echo '#define EXPORT_FN __attribute__((visibility("default")))'
  echo '#define WEAK_FN   __attribute__((weak))'
  echo '#define USED_FN   __attribute__((used))'
  echo ''
  echo 'static int64_t supers_missing(int n) {'
  echo '  fprintf(stderr, "[supers] missing symbol: s%d\n", n);'
  echo '  return 0;'
  echo '}'
  echo ''
  n=0
  while [[ "$n" -lt 256 ]]; do
    echo "extern void s${n}(int64_t *in, int64_t *out) WEAK_FN;"
    echo "EXPORT_FN USED_FN void super${n}(oper_t **oper, oper_t *result) {"
    echo "  int64_t in[64] = {0};"
    echo "  int64_t out[1];"
    echo "  for (int _i = 0; _i < 64; ++_i) {"
    echo "    if (oper[_i] == NULL) break;"
    echo "    in[_i] = (int64_t)oper[_i]->value.li;"
    echo "  }"
    echo "  if (s${n}) {"
    echo "    s${n}(in, out);"
    echo "    result[0].value.li = out[0];"
    echo "  } else {"
    echo "    result[0].value.li = supers_missing(${n});"
    echo "  }"
    echo "}"
    echo ""
    n=$((n + 1))
  done
} > "$WRAPPERS_C"

# 3) Link wrappers.o + libsupers_rust.a into libsupers.so.
#    --whole-archive ensures the strong sN symbols from Rust actually win
#    over the WEAK_FN forward decls in the wrappers.
gcc -O3 -fPIC -shared -I"$INTERP_INC" \
    "$WRAPPERS_C" \
    -Wl,--whole-archive "$RUST_A" -Wl,--no-whole-archive \
    -lpthread -ldl -lm \
    -o "$OUT_DIR/libsupers.so"

echo "[SUPERS-RS] built: $OUT_DIR/libsupers.so"
