#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, random

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

SUPER_TMPL = """-- Merge Sort (versão com SUPER e threshold)

len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

merge xs ys = case xs of
  []      -> ys;
  (x:xt)  -> case ys of
    []       -> xs;
    (y:yt)   -> if x <= y
                then x : merge xt ys
                else y : merge xs yt;;
;

split lst = case lst of
  []     -> ([], []);
  [x]    -> ([x], []);
  x:y:zs -> case split zs of
    (xs, ys) -> (x:xs, y:ys);;
;

p = __P__;

mergeSort0 lst = mergeSortT (len lst) lst;

mergeSortT n0 lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      -> if (len lst) <= (n0 / p)
            then super single input (lst) output (sorted)
                 #BEGINSUPER
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

main = mergeSort0 __VEC__;
"""

def make_vec(n, kind):
    if kind == "range":
        return "[" + ",".join(str(i) for i in range(n, 0, -1)) + "]"
    elif kind == "rand":
        rnd = random.Random(1337)
        xs = [rnd.randint(0, n*2) for _ in range(n)]
        return "[" + ",".join(map(str, xs)) + "]"
    else:
        raise SystemExit("vec precisa ser 'range' ou 'rand'")

def emit_hsk(path, variant, n, p, vec_kind):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    vec = make_vec(n, vec_kind)
    if variant == "asm":
        src = ASM_TMPL.replace("__VEC__", vec)
    elif variant == "super":
        src = SUPER_TMPL.replace("__VEC__", vec).replace("__P__", str(p))
    else:
        raise SystemExit("variant deve ser 'asm' ou 'super'")
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[ms_gen_input] wrote {path} (N={n}, P={p}, variant={variant})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--variant", required=True, choices=["asm","super"])
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range","rand"])
    args = ap.parse_args()
    emit_hsk(args.out, args.variant, args.N, args.P, args.vec)

if __name__ == "__main__":
    main()
