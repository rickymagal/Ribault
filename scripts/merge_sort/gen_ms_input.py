#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, random, sys

SUPER_TMPL = """-- Merge Sort (versão com SUPER e threshold)

-- length
len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

-- merge
merge xs ys = case xs of
  []      -> ys;
  (x:xt)  -> case ys of
    []       -> xs;
    (y:yt)   -> if x <= y
                then x : merge xt ys
                else y : merge xs yt;;
;

-- split em duas metades
split lst = case lst of
  []     -> ([], []);
  [x]    -> ([x], []);
  x:y:zs -> case split zs of
    (xs, ys) -> (x:xs, y:ys);;
;

-- divisor do threshold (n0 / p)
p = __P__;

-- entrada: calcula n0 e chama versão com threshold
mergeSort0 lst = mergeSortT (len lst) lst;

-- Merge Sort com fallback para SUPER quando sublista <= n0 / p
mergeSortT n0 lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      -> if (len lst) <= (n0 / p)
            then super single input (lst) output (sorted)
                 #BEGINSUPER
                 -- insertion sort serial para listas pequenas
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

-- main (vetor gerado pelo script)
main = mergeSort0 __VEC__;
"""

def make_vec(n, kind):
    if kind == "range":
        return "[" + ",".join(str(i) for i in range(n, 0, -1)) + "]"
    if kind == "rand":
        rnd = random.Random(1337)
        xs = [rnd.randint(0, n*2) for _ in range(n)]
        return "[" + ",".join(map(str, xs)) + "]"
    raise SystemExit("vec precisa ser 'range' ou 'rand'")

def emit_hsk(path, n, p, vec_kind):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    vec = make_vec(n, vec_kind)
    src = SUPER_TMPL.replace("__VEC__", vec).replace("__P__", str(p))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[ms_gen_input] wrote {path} (N={n}, P={p}, vec={vec_kind})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range","rand"])
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.vec)

if __name__ == "__main__":
    main()
