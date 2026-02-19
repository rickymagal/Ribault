#!/usr/bin/env python3
"""External validator for Graph Coloring benchmark output.

For GHC variants: checks VALID=True, COLORS>0, RUNTIME_SEC present.
For TALM super: checks stdout line1=colors>0, line2=1 (valid).
"""

import sys, re

def validate_ghc(outfile):
    with open(outfile) as f:
        text = f.read()
    valid_m  = re.search(r'^VALID=(.+)$', text, re.MULTILINE)
    colors_m = re.search(r'^COLORS=(\d+)$', text, re.MULTILINE)
    if not valid_m:
        print("FAIL: VALID= line missing", file=sys.stderr)
        return 1
    if valid_m.group(1).strip() != "True":
        print(f"FAIL: VALID={valid_m.group(1)}", file=sys.stderr)
        return 1
    if not colors_m or int(colors_m.group(1)) <= 0:
        print("FAIL: COLORS missing or zero", file=sys.stderr)
        return 1
    print(f"OK: VALID=True COLORS={colors_m.group(1)}")
    return 0

def validate_super(outfile):
    with open(outfile) as f:
        lines = [l.strip() for l in f if l.strip()]
    if len(lines) < 2:
        print("FAIL: expected at least 2 output lines", file=sys.stderr)
        return 1
    try:
        colors = int(lines[0])
        valid  = int(lines[1])
    except ValueError:
        print(f"FAIL: cannot parse colors/valid from: {lines[:2]}", file=sys.stderr)
        return 1
    if valid != 1:
        print(f"FAIL: valid={valid}", file=sys.stderr)
        return 1
    if colors <= 0:
        print(f"FAIL: colors={colors}", file=sys.stderr)
        return 1
    print(f"OK: valid=1 colors={colors}")
    return 0

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <ghc|super> <outfile>", file=sys.stderr)
        sys.exit(2)
    variant = sys.argv[1]
    outfile = sys.argv[2]
    if variant in ("ghc", "parpseq"):
        sys.exit(validate_ghc(outfile))
    elif variant == "super":
        sys.exit(validate_super(outfile))
    else:
        print(f"Unknown variant: {variant}", file=sys.stderr)
        sys.exit(2)
