#!/usr/bin/env python3
"""External validator for MergeSort benchmark output."""

import sys, re

def validate(outfile, expected_n):
    with open(outfile) as f:
        text = f.read()

    sorted_m = re.search(r'^SORTED=(.+)$', text, re.MULTILINE)
    nout_m   = re.search(r'^N_OUT=(\d+)$', text, re.MULTILINE)
    head_m   = re.search(r'^SORTED_HEAD=\[(.+?)\]$', text, re.MULTILINE)

    if not sorted_m:
        print(f"FAIL: SORTED= line missing", file=sys.stderr)
        return 1
    if sorted_m.group(1).strip() != "True":
        print(f"FAIL: SORTED={sorted_m.group(1)}", file=sys.stderr)
        return 1
    if not nout_m:
        print(f"FAIL: N_OUT= line missing", file=sys.stderr)
        return 1
    nout = int(nout_m.group(1))
    if nout != expected_n:
        print(f"FAIL: N_OUT={nout} != expected {expected_n}", file=sys.stderr)
        return 1
    if head_m:
        vals = [int(x.strip()) for x in head_m.group(1).split(",")]
        expected_head = list(range(1, min(11, expected_n + 1)))
        if vals != expected_head[:len(vals)]:
            print(f"FAIL: SORTED_HEAD={vals} != expected {expected_head[:len(vals)]}", file=sys.stderr)
            return 1

    print(f"OK: SORTED=True N_OUT={nout}")
    return 0

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <outfile> <expected_N>", file=sys.stderr)
        sys.exit(2)
    sys.exit(validate(sys.argv[1], int(sys.argv[2])))
