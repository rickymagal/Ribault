#!/usr/bin/env bash
# Build libsupers.so from a single C file (no Haskell, no GHC).
# Produces a libsupers.so where the user-super functions (sN) are strong C
# symbols, overriding the WEAK_FN Haskell exports the Trebuchet wrappers
# normally expect.
#
# Usage:  build_supers_c.sh <c_source>  <output_dir>
# Output: <output_dir>/libsupers.so plus the small wrapper C generated to
#         forward Trebuchet's super calls to sN.
set -euo pipefail

[[ $# -eq 2 ]] || { echo "usage: $0 <user_supers.c> <output_dir>"; exit 2; }

C_SRC="$1"
OUT_DIR="$2"
[[ -f "$C_SRC" ]] || { echo "source not found: $C_SRC"; exit 1; }
mkdir -p "$OUT_DIR"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INTERP_INC="$REPO_ROOT/TALM/interp/include"
[[ -d "$INTERP_INC" ]] || { echo "missing $INTERP_INC"; exit 1; }

# Generate supers_wrappers.c (same flavor build_supers.sh uses, max=256).
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

cp "$C_SRC" "$OUT_DIR/user_supers.c"
gcc -O3 -fPIC -shared -I"$INTERP_INC" \
    "$OUT_DIR/user_supers.c" "$WRAPPERS_C" \
    -o "$OUT_DIR/libsupers.so"

echo "[SUPERS-C] built: $OUT_DIR/libsupers.so"
