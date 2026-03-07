#!/usr/bin/env python3
"""Generate TALM .hsk for merge sort benchmark.

Uses the exact pattern from test/21_merge_sort_super.hsk:
  - Pure DF recursion: mergeSortT (depth = log2(P))
  - Supers for O(N) linear work: len, splitCount, merge
  - ms_super at cutoff (n <= n0/p) with par/pseq inside
  - init_super to generate [N, N-1, ..., 1] (avoids inline vector)
  - verify_sorted to check result and print RESULT=

The .hsk goes through the standard codegen pipeline:
  codegen → .fl → assembler → .flb/.pla
  build_supers.sh → Supers.hs → libsupers.so
"""

import argparse, os


HSK_TMPL = r"""-- Merge Sort: TALM benchmark (matches test/21_merge_sort_super.hsk pattern)
-- N=__N__  P=__P__

-- init_super: takes [N], generates [N, N-1, ..., 1] as DF list
init_super inp =
  super single input (inp) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      let n = fromIntegral (head (toList inp)) :: Int
          build !k !acc
            | k > n     = return acc
            | otherwise = do
                cell <- mkPairIO (fromIntegral k) acc
                build (k + 1) cell
      build 1 handleNil
#ENDSUPER

-- len_super: walk DF list iteratively, count elements
len xs =
  super single input (xs) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      let walk !ptr !cnt
            | ptr == handleNil = return (fromIntegral cnt :: Int64)
            | otherwise = do
                (_, t) <- readPairIO ptr
                walk t (cnt + 1 :: Int)
      walk xs (0 :: Int)
#ENDSUPER

p = __P__

-- merge_super: takes DF pair (xs, ys), returns merged sorted DF list
merge pair =
  super single input (pair) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      (xsH, ysH) <- readPairIO pair
      let xs = toList xsH
          ys = toList ysH
          mergeL [] bs        = bs
          mergeL as []        = as
          mergeL (a:at) (b:bt)
            | a <= b          = a : mergeL at (b:bt)
            | otherwise       = b : mergeL (a:at) bt
      return (fromList (mergeL xs ys))
#ENDSUPER

-- splitCount_super: split DF list into two halves with counts
-- Returns DF-encoded (left, (right, (nl, nr)))
splitCount lst =
  super single input (lst) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      let walk !ptr !lAcc !rAcc !nl !nr !isLeft
            | ptr == handleNil = return (lAcc, rAcc, nl, nr)
            | otherwise = do
                (h, t) <- readPairIO ptr
                if isLeft
                  then do
                    cell <- mkPairIO h lAcc
                    walk t cell rAcc (nl + 1) nr False
                  else do
                    cell <- mkPairIO h rAcc
                    walk t lAcc cell nl (nr + 1) True
      (left, right, nl, nr) <- walk lst handleNil handleNil (0 :: Int) (0 :: Int) True
      counts <- mkPairIO (fromIntegral nl :: Int64) (fromIntegral nr :: Int64)
      rest <- mkPairIO right counts
      mkPairIO left rest
#ENDSUPER

-- SUPER: full merge sort for small lists (par/pseq inside)
ms_super lst =
  super single input (lst) output (sorted)
#BEGINSUPER
    sorted =
      let
        hlist = toList lst

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
        fromList (ms hlist)
#ENDSUPER

-- verify_sorted: walk DF list iteratively, check sorted order, return 1 or 0
verify_sorted lst =
  super single input (lst) output (out)
#BEGINSUPER
    out = unsafePerformIO $ do
      let walk !ptr !prev !cnt !sorted
            | ptr == handleNil = return (cnt, sorted)
            | otherwise = do
                (h, t) <- readPairIO ptr
                let !sorted' = if cnt > (0 :: Int) then sorted && h >= prev else sorted
                walk t h (cnt + 1) sorted'
      (cnt, sorted) <- walk lst (0 :: Int64) (0 :: Int) True
      return (if sorted && cnt == (__N__ :: Int) then 1 else 0 :: Int64)
#ENDSUPER

-- print_result: print RESULT=v to stdout
print_result v =
  super single input (v) output (out)
#BEGINSUPER
    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show v); hFlush stdout; pure 0)
#ENDSUPER

mergeSort0 lst =
  let n0 = len lst

  in mergeSortT n0 n0 lst

mergeSortT n0 n lst = case lst of
  []     -> []

  (x:[]) -> [x]

  _      ->
    if n <= (n0 / p)
    then ms_super lst
    else
      case splitCount lst of
        (left, rest) ->
          case rest of
            (right, counts) ->
              case counts of
                (nl, nr) ->
                  merge (mergeSortT n0 nl left, mergeSortT n0 nr right)

main = print_result (verify_sorted (mergeSort0 (init_super [__N__])))
"""


def emit(path, input_dir, p):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        n = int(f.read().strip())

    src = HSK_TMPL
    src = src.replace("__N__", str(n))
    src = src.replace("__P__", str(p))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_talm] wrote {path}  (N={n}, P={p})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="Output .hsk path")
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--P", type=int, required=True, help="Parallelism level (cutoff = N/P)")
    args = ap.parse_args()
    emit(args.out, args.input_dir, args.P)


if __name__ == "__main__":
    main()
