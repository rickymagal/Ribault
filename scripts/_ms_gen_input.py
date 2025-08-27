#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
from pathlib import Path

ASM_HSK = """\
-- Merge Sort (ASM)
-- length
len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

-- Merge two sorted lists into a single sorted list
merge xs ys = case xs of
  []      -> ys;
  (x:xt)  -> case ys of
    []       -> xs;
    (y:yt)   -> if x <= y
                then x : merge xt ys
                else y : merge xs yt;;
;

-- Split a list into two approximately equal halves
split lst = case lst of
  []     -> ([], []);
  [x]    -> ([x], []);
  x:y:zs -> case split zs of
    (xs, ys) -> (x:xs, y:ys);;
;

mergeSort lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      -> case split lst of
    (left, right) -> merge (mergeSort left) (mergeSort right);;
;

-- Main
main = mergeSort __LIST__;
"""

SUPER_HSK = """\
-- Merge Sort (SUPER fallback)
-- length
len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

-- Merge two sorted lists into a single sorted list
merge xs ys = case xs of
  []      -> ys;
  (x:xt)  -> case ys of
    []       -> xs;
    (y:yt)   -> if x <= y
                then x : merge xt ys
                else y : merge xs yt;;
;

-- Split a list into two approximately equal halves
split lst = case lst of
  []     -> ([], []);
  [x]    -> ([x], []);
  x:y:zs -> case split zs of
    (xs, ys) -> (x:xs, y:ys);;
;

-- divisor do threshold (igual ao número de processadores)
p = __P__;

-- Função de entrada: calcula N0 e chama a versão com threshold
mergeSort0 lst = mergeSortT (len lst) lst;

-- Merge Sort com fallback para SUPER quando sublista <= N0 / p
mergeSortT n0 lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      -> if (len lst) <= (n0 / p)
            then super single input (lst) output (sorted)
                 #BEGINSUPER
                 -- Haskell: insertion sort para listas pequenas
                 insert x [] = [x]
                 insert x (y:ys)
                   | x <= y    = x:y:ys
                   | otherwise = y : insert x ys
                 isort []     = []
                 isort (h:t)  = insert h (isort t)
                 sorted = isort lst
                 #ENDSUPER
            else case split lst of
              (left, right) ->
                merge (mergeSortT n0 left) (mergeSortT n0 right);;
;

-- Main
main = mergeSort0 __LIST__;
"""

def ones_list(n: int) -> str:
    return "[" + ",".join("1" for _ in range(n)) + "]"

def desc_list(n: int) -> str:
    return "[" + ",".join(str(i) for i in range(n, 0, -1)) + "]"

def main():
    ap = argparse.ArgumentParser()
    # aceita --n e --N
    ap.add_argument("--n", "--N", dest="n", type=int, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--variant", choices=["asm","super"], default="asm")
    # aceita --P e --threads (alias)
    ap.add_argument("--P", "--threads", dest="P", type=int, default=1)
    ap.add_argument("--vec", choices=["ones","desc"], default="desc")
    args = ap.parse_args()

    lst = ones_list(args.n) if args.vec == "ones" else desc_list(args.n)

    if args.variant == "super":
        hsk = SUPER_HSK.replace("__P__", str(args.P)).replace("__LIST__", lst)
    else:
        hsk = ASM_HSK.replace("__LIST__", lst)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(hsk, encoding="utf-8")
    print(f"[ms_gen_input] wrote {args.out} (variant={args.variant} N={args.n} P={args.P} vec={args.vec})")

if __name__ == "__main__":
    main()
