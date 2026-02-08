#!/usr/bin/env python3

import argparse
import os
import random

SUPER_BODY_SEQ = """    sorted =
      let
        splitLocal []         = ([], [])
        splitLocal [x]        = ([x], [])
        splitLocal (x:y:rest) =
          let (xs, ys) = splitLocal rest
          in (x:xs, y:ys)

        mergeLocal [] ys        = ys
        mergeLocal xs []        = xs
        mergeLocal (x:xt) (y:yt)
          | x <= y              = x : mergeLocal xt (y:yt)
          | otherwise           = y : mergeLocal (x:xt) yt

        ms []  = []
        ms [x] = [x]
        ms xs  =
          let (l, r) = splitLocal xs
          in mergeLocal (ms l) (ms r)
      in
        ms lst
"""

SUPER_BODY_PAR = """    sorted =
      let
        forceList []     = ()
        forceList (_:xs) = forceList xs

        splitLocal []         = ([], [])
        splitLocal [x]        = ([x], [])
        splitLocal (x:y:rest) =
          let (xs, ys) = splitLocal rest
          in (x:xs, y:ys)

        mergeLocal [] ys        = ys
        mergeLocal xs []        = xs
        mergeLocal (x:xt) (y:yt)
          | x <= y              = x : mergeLocal xt (y:yt)
          | otherwise           = y : mergeLocal (x:xt) yt

        ms []  = []
        ms [x] = [x]
        ms xs  =
          let (l, r) = splitLocal xs
              l' = ms l
              r' = ms r
          in forceList l' `par` (forceList r' `pseq` mergeLocal l' r')
      in
        ms lst
"""


WORKING_TMPL_SUPER = """-- Merge Sort in the compiled functional subset
-- Goal:
--  - Keep the parallel structure in the pure subset (DF graph can fork).
--  - Push only the small-grain full sort into a SUPER at the cutoff.
--  - Avoid per-node size propagation by using a root-computed depth.

len xs = case xs of
  []     -> 0;
  (_:ys) -> 1 + len ys;
;

merge xs ys = case xs of
  [] -> ys;
  (x:xt) -> case ys of
    [] -> xs;
    (y:yt) -> if x <= y
              then x : merge xt ys
              else y : merge xs yt;;
;

split lst = case lst of
  []   -> ([], []);
  (x:[])  -> ([x], []);
  x:y:zs ->
    case split zs of
      (xs, ys) -> (x:xs, y:ys);;
;

ms_pure lst = case lst of
  [] -> [];
  (x:[]) -> [x];
  _ ->
    case split lst of
      (left, right) -> merge (ms_pure left) (ms_pure right);;
;

p = __P__;
cutoff = __CUTOFF__;

depth n = if n <= cutoff then 0 else 1 + depth (n / 2);

-- SUPER: full merge sort for small lists
ms_super lst =
  super single input (lst) output (sorted)
#BEGINSUPER
__SUPER_BODY__
#ENDSUPER;

mergeSort0 lst =
  let d0 = depth (len lst)
  in mergeSortD d0 lst
;

mergeSortD d lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      ->
    if d <= 0
    then __LEAF_FN__ lst
    else
      case split lst of
        (left, right) ->
          merge (mergeSortD (d - 1) left) (mergeSortD (d - 1) right);;
;

main = mergeSort0 __VEC__;
"""


WORKING_TMPL_ASM = """-- Merge Sort in the compiled functional subset
-- Goal:
--  - Keep the parallel structure in the pure subset (DF graph can fork).
--  - Use pure assembly code at the cutoff (no SUPERs).
--  - Avoid per-node size propagation by using a root-computed depth.

len xs = case xs of
  []     -> 0;
  (_:ys) -> 1 + len ys;
;

merge xs ys = case xs of
  [] -> ys;
  (x:xt) -> case ys of
    [] -> xs;
    (y:yt) -> if x <= y
              then x : merge xt ys
              else y : merge xs yt;;
;

split lst = case lst of
  []   -> ([], []);
  (x:[])  -> ([x], []);
  x:y:zs ->
    case split zs of
      (xs, ys) -> (x:xs, y:ys);;
;

ms_pure lst = case lst of
  [] -> [];
  (x:[]) -> [x];
  _ ->
    case split lst of
      (left, right) -> merge (ms_pure left) (ms_pure right);;
;

p = __P__;
cutoff = __CUTOFF__;

depth n = if n <= cutoff then 0 else 1 + depth (n / 2);

mergeSort0 lst =
  let d0 = depth (len lst)
  in mergeSortD d0 lst
;

mergeSortD d lst = case lst of
  []     -> [];
  (x:[]) -> [x];
  _      ->
    if d <= 0
    then ms_pure lst
    else
      case split lst of
        (left, right) ->
          merge (mergeSortD (d - 1) left) (mergeSortD (d - 1) right);;
;

main = mergeSort0 __VEC__;
"""

COARSE_SUPER_TMPL = """-- Coarse-grained merge sort: split, merge, and leaf sort are ALL supers.
-- The DF graph only contains the fork/join recursion skeleton.
-- REQUIRES: DF_LIST_BUILTIN=1 (supers use C-list encoding via cfromList/ctoList).

p = __P__;

-- init_super: builds the test vector inside Haskell (avoids O(N) DF cons ops).
-- Input is a 1-element list [N]; the super generates [N, N-1, ..., 1].
init_super inp =
  super single input (inp) output (result)
#BEGINSUPER
    result = let n = head inp in [n, n-1 .. 1]
#ENDSUPER;

-- split_super: list -> [leftHandle, rightHandle]
split_super lst =
  super single input (lst) output (result)
#BEGINSUPER
    result =
      let
        sp []         = ([], [])
        sp [x]        = ([x], [])
        sp (x:y:rest) = let (xs, ys) = sp rest in (x:xs, y:ys)
        (l, r) = sp lst
      in [cfromList l, cfromList r]
#ENDSUPER;

-- merge_super: [leftHandle, rightHandle] -> merged list
merge_super pair =
  super single input (pair) output (result)
#BEGINSUPER
    result =
      let
        l  = ctoList (head pair)
        r  = ctoList (head (tail pair))
        mg [] ys        = ys
        mg xs []        = xs
        mg (x:xt) (y:yt)
          | x <= y     = x : mg xt (y:yt)
          | otherwise  = y : mg (x:xt) yt
      in mg l r
#ENDSUPER;

-- ms_super: list -> sorted list (leaf sort, sequential)
ms_super lst =
  super single input (lst) output (sorted)
#BEGINSUPER
__SUPER_BODY__
#ENDSUPER;

-- Pure fork/join skeleton: no DF list ops, only super calls.
mergeSortD d lst =
    if d <= 0
    then ms_super lst
    else
      case split_super lst of
        (lh : rest) -> case rest of
          (rh : _) ->
            merge_super (mergeSortD (d - 1) lh : mergeSortD (d - 1) rh : []);;
;

main = mergeSortD __DEPTH__ (init_super [__N__]);
"""

SEQ_TMPL = """-- Merge Sort in the subset of the compiled functional language

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
  (x:[])  -> ([x], []);
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

main = mergeSort __VEC__;
"""


ARRAY_SUPER_TMPL = """-- Array-based parallel merge sort (C supers, no Haskell).
-- DF graph = recursive binary splitting skeleton.
-- Supers operate on raw C arrays via pointers (zero encoding overhead).
-- Work stealing distributes independent recursive calls across PEs.

p = __P__;

init_super inp =
  super single input (inp) output (result)
#BEGINSUPER
    result = inp
#ENDSUPER;

sort_leaf pair =
  super single input (pair) output (result)
#BEGINSUPER
    result = pair
#ENDSUPER;

merge_pair quad =
  super single input (quad) output (result)
#BEGINSUPER
    result = quad
#ENDSUPER;

msort ptr size nparts =
  if nparts <= 1
  then sort_leaf (ptr : size : [])
  else
    let lsize = size / 2
        rsize = size - lsize
        rptr  = ptr + lsize * 8
        lnp   = nparts / 2
        rnp   = nparts - lnp
    in merge_pair (msort ptr lsize lnp : lsize : msort rptr rsize rnp : rsize : [])
;;
;

main = msort (init_super [__N__]) __N__ __P__;
"""


def compute_depth(n: int, cutoff: int) -> int:
    """Compute recursion depth: how many times we halve n before it drops to cutoff."""
    if n <= cutoff:
        return 0
    return 1 + compute_depth(n // 2, cutoff)


def make_vec(n: int, kind: str) -> str:
    if kind == "range":
        return "[" + ",".join(str(i) for i in range(n, 0, -1)) + "]"
    if kind == "rand":
        rnd = random.Random(1337)
        xs = [rnd.randint(0, n * 2) for _ in range(n)]
        return "[" + ",".join(map(str, xs)) + "]"
    raise SystemExit("vec must be 'range' or 'rand'")


def emit_hsk(path: str, n: int, p: int, cutoff: int, vec_kind: str, mode: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    vec = make_vec(n, vec_kind)
    if mode == "seq":
        src = SEQ_TMPL.replace("__VEC__", vec)
    elif mode == "array":
        src = (
            ARRAY_SUPER_TMPL.replace("__P__", str(p))
            .replace("__N__", str(n))
        )
    elif mode == "coarse":
        depth = compute_depth(n, cutoff)
        super_par = os.getenv("MS_SUPER_PAR", "0")
        super_body = SUPER_BODY_PAR if super_par and super_par != "0" else SUPER_BODY_SEQ
        src = (
            COARSE_SUPER_TMPL.replace("__P__", str(p))
            .replace("__DEPTH__", str(depth))
            .replace("__N__", str(n))
            .replace("__SUPER_BODY__", super_body)
        )
        if src.count("#BEGINSUPER") != 4:
            raise SystemExit("expected exactly four SUPER blocks (coarse)")
    else:
        leaf_mode = os.getenv("MS_LEAF", "super").lower()
        if leaf_mode == "asm":
            src = (
                WORKING_TMPL_ASM.replace("__P__", str(p))
                .replace("__CUTOFF__", str(cutoff))
                .replace("__VEC__", vec)
            )
        else:
            leaf_fn = "ms_super"
            super_par = os.getenv("MS_SUPER_PAR", "0")
            super_body = SUPER_BODY_PAR if super_par and super_par != "0" else SUPER_BODY_SEQ
            src = (
                WORKING_TMPL_SUPER.replace("__P__", str(p))
                .replace("__CUTOFF__", str(cutoff))
                .replace("__VEC__", vec)
                .replace("__LEAF_FN__", leaf_fn)
                .replace("__SUPER_BODY__", super_body)
            )
            if src.count("#BEGINSUPER") != 1:
                raise SystemExit("expected exactly one SUPER block (ms)")
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[ms_gen_input] wrote {path} (N={n}, P={p}, vec={vec_kind})")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range", "rand"])
    ap.add_argument("--cutoff", type=int, default=256)
    ap.add_argument("--mode", default="super", choices=["super", "seq", "coarse", "array"])
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.cutoff, args.vec, args.mode)


if __name__ == "__main__":
    main()
