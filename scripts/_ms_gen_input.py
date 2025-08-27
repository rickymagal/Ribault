#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import textwrap
from pathlib import Path

MS_SRC = """\
-- Merge two sorted lists into a single sorted list
merge xs ys = case xs of
    [] -> ys;
    (x:xt) -> case ys of
        [] -> xs;
        (y:yt) -> if x <= y
                  then x : merge xt ys
                  else y : merge xs yt;;
;

-- Split a list into two approximately equal halves
split lst = case lst of
  []   -> ([], []);
  [x]  -> ([x], []);
  x:y:zs ->
    case split zs of
      (xs, ys) -> (x:xs, y:ys);;
;

-- Merge Sort function
mergeSort lst = case lst of
    [] -> [];
    (x:[]) -> [x];
    _ -> case split lst of
           (left, right) -> merge (mergeSort left) (mergeSort right);;
;
"""

def ones_list(n: int) -> str:
    # lista literal: [1,1,1,...]
    if n <= 0:
        return "[]"
    return "[" + ",".join(["1"] * n) + "]"

def main():
    ap = argparse.ArgumentParser(description="Generate HSK MergeSort with a list of ones")
    ap.add_argument("--n", type=int, required=True, help="list size")
    ap.add_argument("--out", type=Path, required=True, help="output .hsk path")
    # flags futuras (placeholder para manter compatibilidade de chamada)
    ap.add_argument("--variant", choices=["asm","super"], default="asm")
    ap.add_argument("--threads", type=int, default=1)
    args = ap.parse_args()

    lst = ones_list(args.n)
    hsk = MS_SRC + "\n-- Main expression for testing\nmain = mergeSort " + lst + "\n"

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(hsk, encoding="utf-8")
    print(f"[ms_gen_input] wrote {args.out} (N={args.n})")

if __name__ == "__main__":
    main()
