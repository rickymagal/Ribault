#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, random

HS_TMPL = r"""-- Auto-generated: parallel Merge Sort (GHC +RTS -N)
{-# LANGUAGE BangPatterns #-}
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- split em duas metades
split2 :: [Int] -> ([Int],[Int])
split2 []         = ([],[])
split2 [x]        = ([x],[])
split2 (x:y:zs)   = let (xs,ys) = split2 zs in (x:xs, y:ys)

-- merge (estável)
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- mergesort com paralelismo explícito (2 sparks por nível)
msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  =
  let (a,b) = split2 xs
      goA   = msort a
      goB   = msort b
  in runEval $ do
       a' <- rpar (force goA)
       b' <- rpar (force goB)
       rseq a'
       rseq b'
       return (merge a' b')

-- garante avaliação total
forceList :: NFData a => [a] -> ()
forceList xs = xs `deepseq` ()

-- entrada (gerada pelo script)
xsInput :: [Int]
xsInput = __VEC__

main :: IO ()
main = do
  let !_ = length xsInput  -- force spine
  t0 <- getCurrentTime
  let ys = msort xsInput
  forceList ys `seq` return ()
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RUNTIME_SEC=" ++ show secs
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

def emit_hs(path, n, vec_kind):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    vec = make_vec(n, vec_kind)
    src = HS_TMPL.replace("__VEC__", vec)
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[hs_gen_input] wrote {path} (N={n}, vec={vec_kind})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--vec", default="range", choices=["range","rand"])
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.vec)

if __name__ == "__main__":
    main()
