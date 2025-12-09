#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import random

ASM_TMPL = """-- Merge Sort in the subset of the compiled functional language

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

-- Main expression for testing
main = mergeSort __VEC__
"""

SUPER_TMPL = """-- Merge Sort in the compiled functional subset
-- Everything ends with ";". When a sublist size becomes <= (originalSize / p),
-- we offload it to a SUPER that runs a full merge sort in Haskell.

-- length
len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

-- Merge two sorted lists into a single sorted list (pure language version)
merge xs ys = case xs of
  []      -> ys;
  (x:xt)  -> case ys of
    []       -> xs;
    (y:yt)   -> if x <= y
                then x : merge xt ys
                else y : merge xs yt;;
;

-- Split a list into two approximately equal halves (pure language version)
split lst = case lst of
  []     -> ([], []);
  [x]    -> ([x], []);
  x:y:zs -> case split zs of
    (xs, ys) -> (x:xs, y:ys);;
;

-- threshold divisor
p = __P__;

-- super: full merge sort for short lists
-- This runs a full recursive merge sort inside the SUPER (Haskell code),
-- so each call has a relatively large grain.
ms_super lst =
  super single input (lst) output (sorted)
#BEGINSUPER
    ms []  = []
    ms [x] = [x]
    ms xs  =
      let (l, r) = splitLocal xs
      in mergeLocal (ms l) (ms r)

    splitLocal []           = ([], [])
    splitLocal [x]          = ([x], [])
    splitLocal (x:y:rest)   =
      let (xs, ys) = splitLocal rest
      in (x:xs, y:ys)

    mergeLocal [] ys        = ys
    mergeLocal xs []        = xs
    mergeLocal (x:xt) (y:yt)
      | x <= y    = x : mergeLocal xt (y:yt)
      | otherwise = y : mergeLocal (x:xt) yt

    sorted = ms lst
#ENDSUPER;

-- entry function: compute N0 and call thresholded version
mergeSort0 lst = mergeSortT (len lst) lst;

-- Merge Sort with fallback to SUPER when the grain is small enough
mergeSortT n0 lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      ->
    let n = len lst;
    in
      if n <= (n0 / p)
      -- whole sublist is small: do a full merge sort in a single SUPER
      then ms_super lst
      else
        case split lst of
          (left, right) ->
            merge (mergeSortT n0 left) (mergeSortT n0 right);;
;

-- Example
main = mergeSort0 __VEC__
"""

def make_vec(n, kind):
    if kind == "range":
        # descending [n, n-1, ..., 1]
        return "[" + ",".join(str(i) for i in range(n, 0, -1)) + "]"
    elif kind == "rand":
        rnd = random.Random(1337)  # deterministic
        xs = [rnd.randint(0, n * 2) for _ in range(n)]
        return "[" + ",".join(map(str, xs)) + "]"
    else:
        raise SystemExit("vec must be 'range' or 'rand'")

def emit_hsk(path, variant, n, p, vec_kind):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    vec = make_vec(n, vec_kind)
    if variant == "asm":
        src = ASM_TMPL.replace("__VEC__", vec)
    elif variant == "super":
        src = SUPER_TMPL.replace("__VEC__", vec).replace("__P__", str(p))
    else:
        raise SystemExit("variant must be 'asm' or 'super'")
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[ms_gen_input] wrote {path} (N={n}, P={p}, variant={variant})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range", "rand"])
    # run.sh does not pass --variant; default needs to be SUPER
    ap.add_argument("--variant", default="super", choices=["asm", "super"])
    args = ap.parse_args()
    emit_hsk(args.out, args.variant, args.N, args.P, args.vec)

if __name__ == "__main__":
    main()
