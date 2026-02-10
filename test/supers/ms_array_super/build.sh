#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
RR="$(cd "$DIR/../../.." && pwd)"
CC="${CC:-gcc}"
CFLAGS="-O2 -fPIC -I$RR/TALM/interp/include"
MAX=256

cd "$DIR"

# Generate wrappers
{
  echo '#include <stdint.h>'
  echo '#include <stdio.h>'
  echo '#include "queue.h"'
  echo '#include "interp.h"'
  echo ''
  echo '#if defined(__GNUC__)'
  echo '#  define EXPORT_FN __attribute__((visibility("default")))'
  echo '#  define WEAK_FN   __attribute__((weak))'
  echo '#  define USED_FN   __attribute__((used))'
  echo '#else'
  echo '#  define EXPORT_FN'
  echo '#  define WEAK_FN'
  echo '#  define USED_FN'
  echo '#endif'
  echo ''
  echo 'static int64_t supers_missing(int n) {'
  echo '  fprintf(stderr, "[supers] missing symbol: s%d\n", n);'
  echo '  return 0;'
  echo '}'
  echo ''
  n=0
  while [ "$n" -lt "$MAX" ]; do
    echo "extern void s${n}(int64_t *in, int64_t *out) WEAK_FN;"
    echo "EXPORT_FN USED_FN void super${n}(oper_t **oper, oper_t *result) {"
    echo "  int64_t in[2];"
    echo "  int64_t out[1];"
    echo "  in[0] = (int64_t)oper[0]->value.li;"
    echo "  in[1] = 0;"
    echo "  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }"
    echo "  if (s${n}) {"
    echo "    s${n}(in, out);"
    echo "    result[0].value.li = out[0];"
    echo "  } else {"
    echo "    result[0].value.li = supers_missing(${n});"
    echo "  }"
    echo "}"
    echo ''
    n=$((n + 1))
  done
} > supers_wrappers.c

echo '/* empty */' > supers_aliases.c

$CC $CFLAGS -c c_supers.c -o c_supers.o
$CC $CFLAGS -c supers_wrappers.c -o supers_wrappers.o
$CC $CFLAGS -c supers_aliases.c -o supers_aliases.o
$CC -shared -o libsupers.so c_supers.o supers_wrappers.o supers_aliases.o -Wl,-rpath,'$ORIGIN'

echo "[OK] built $DIR/libsupers.so (C-only, no GHC)"
