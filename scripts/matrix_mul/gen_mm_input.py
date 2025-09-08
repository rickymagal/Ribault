#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, random

TMPL = """-- ===============================================================
--  MatrixMul.hsk  –  multiplicação de matrizes (Float)
-- ===============================================================

-- nº de processadores (seu script altera este valor)
p x = __P__;

-- 1. utilidades --------------------------------------------------

heads xss = case xss of
  []             -> [];
  ((x:_):xss1)   -> x : heads xss1;
;

tails xss = case xss of
  []               -> [];
  ((_ : xs):xss1)  -> xs : tails xss1;
;

append xs ys = case xs of
  []      -> ys;
  (h:ts)  -> h : append ts ys;
;

len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

takeN k xs = if k <= 0 then [] else case xs of
  []      -> [];
  (y:ys)  -> y : takeN (k - 1) ys;
;

dropN k xs = if k <= 0 then xs else case xs of
  []      -> [];
  (_:ys)  -> dropN (k - 1) ys;
;

splitAtN k xs = (takeN k xs, dropN k xs);

-- 2. transposição -----------------------------------------------

transpose xss = case xss of
  []     -> [];
  (_:_)  ->
    let hs = heads xss in
    hs : transpose (tails xss);
;

-- 3. produto escalar (Float) ------------------------------------

dot xs ys = case xs of
  []        -> 0.0;
  (x:xs1)   -> case ys of
    []        -> 0.0;
    (y:ys1)   ->
      let xf = x * 1.0 in
      let yf = y * 1.0 in
      (xf * yf) + dot xs1 ys1;;
;

-- 4. linha × matriz transposta ----------------------------------

rowTimes row colsT = case colsT of
  []      -> [];
  (c:cs)  ->
    let h  = dot row c in
    let ts = rowTimes row cs in
    h : ts;
;

-- 5. multiplicação sequencial ----------------------------------

mmult rows colsT = case rows of
  []       -> [];
  (r:rs1)  ->
    let h  = rowTimes r colsT in
    let ts = mmult rs1 colsT in
    h : ts;
;

-- 6. multiplicação paralela guiada por p -----------------------

mmultPar rows colsT =
  let n     = len rows in
  let cores = p 0 in
  let k0    = n / cores in
  let k     = if k0 <= 0 then 1 else k0 in
  if n <= k
  then mmult rows colsT
  else
    case splitAtN k rows of
      (lhs, rhs) ->
        let left  = mmultPar lhs colsT in
        let right = mmultPar rhs colsT in
        append left right;
;

matrixMul a b = mmultPar a (transpose b);

-- 7. exemplo de uso ---------------------------------------------

main =
  let a = __A__ in
  let b = __B__ in
  matrixMul a b;
"""

def mat_range(n):
    # A[i][j] = float(i*n + j + 1), B[i][j] = float((i+j) % n + 1)
    A = [[float(i*n + j + 1) for j in range(n)] for i in range(n)]
    B = [[float((i + j) % n + 1) for j in range(n)] for i in range(n)]
    return A, B

def mat_rand(n, seed=1337):
    rnd = random.Random(seed)
    A = [[round(rnd.uniform(0.0, 10.0), 4) for _ in range(n)] for _ in range(n)]
    B = [[round(rnd.uniform(0.0, 10.0), 4) for _ in range(n)] for _ in range(n)]
    return A, B

def to_hsk_matrix(M):
    rows = []
    for r in M:
        # garantir sufixo .0 quando inteiro
        xs = []
        for v in r:
            if float(v).is_integer():
                xs.append(f"{int(v)}.0")
            else:
                xs.append(f"{v}")
        rows.append("[" + ", ".join(xs) + "]")
    return "[" + ", ".join(rows) + "]"

def emit_hsk(path, n, p, kind):
    os.makedirs(os.path.dirname(path), exist_ok=True)

    if kind == "range":
        A, B = mat_range(n)
    elif kind == "rand":
        A, B = mat_rand(n)
    else:
        raise SystemExit("mat precisa ser 'range' ou 'rand'")

    a_txt = to_hsk_matrix(A)
    b_txt = to_hsk_matrix(B)

    src = (TMPL
           .replace("__P__", str(p))
           .replace("__A__", a_txt)
           .replace("__B__", b_txt))

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[mm_gen_input] wrote {path} (N={n}, P={p}, mat={kind})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--mat", default="range", choices=["range","rand"])
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.mat)

if __name__ == "__main__":
    main()
