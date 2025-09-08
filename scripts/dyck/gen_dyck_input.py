#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, textwrap

TMPL = """-- dyck_path.hsk (auto-generated)
-- ------------------------------------------------------------
-- Template para o problema do Caminho de Dyck
-- * Controle de Tamanho (n)
-- * Controle de Desequilíbrio de Parênteses (delta)
-- * Controle de Desequilíbrio de Trabalho (imb 0-100)
-- * Nº de Processadores (p)
-- ------------------------------------------------------------

-- PARÂMETROS (alterados por script)

n x     = __N__;
p x     = __P__;
imb x   = __IMB__;
delta x = __DELTA__;

-- ------------------------------------------------------------
-- Funções auxiliares (todas terminam com ';')
-- ------------------------------------------------------------

len xs = case xs of
  []      -> 0;
  (_:ys)  -> 1 + len ys;
;

takeN k xs = if k <= 0 then [] else case xs of
  []     -> [];
  (y:ys) -> y : takeN (k-1) ys;
;

dropN k xs = if k <= 0 then xs else case xs of
  []     -> [];
  (_:ys) -> dropN (k-1) ys;
;

splitAtN k xs = (takeN k xs, dropN k xs);

replicateN k x = if k <= 0 then [] else x : replicateN (k-1) x;

-- ------------------------------------------------------------
-- Geração da sequência de parênteses controlada
-- '(' →  1 ; ')' → -1
-- ------------------------------------------------------------

repeatDyck m acc = if m == 0 then acc else repeatDyck (m - 1) (1 : -1 : acc);

append xs ys = case xs of
[] -> ys;
(h:ts) -> h : append ts ys;
;

generateDyck len d =
         let base = repeatDyck (len / 2) [] in
         if d == 0
         then base
         else if d > 0
              then append base (replicateN d 1)
          else append base (replicateN (0 - d) (-1))
;

inputSeq = generateDyck (n 0) (delta 0);

-- ------------------------------------------------------------
-- SUPER  ─  processamento sequencial de um fragmento
-- devolve (somaTotal, minPrefixo)
-- ------------------------------------------------------------

analyseChunk lst = super single input (lst) output (res)
  #BEGINSUPER
    aux s mn []     = (s, mn)
    aux s mn (x:xs) =
      let s1  = s + x in
      let mn1 = if s1 < mn then s1 else mn in
      aux s1 mn1 xs
    (tot, mn) = aux 0 0 lst
    res = [tot, mn]
  #ENDSUPER
;

chunkPair lst = case analyseChunk lst of
  [s, mn] -> (s, mn);
;

-- ------------------------------------------------------------
-- Divisão desequilibrada controlada por IMB (work-skew)
-- IMB = 0 → 50/50 ; IMB ≈100 → ~75/25
-- ------------------------------------------------------------

splitImb xs =
  splitAtN (((len xs) * (100 + (imb 0))) / 200) xs
;

-- ------------------------------------------------------------
-- Recursão paralela com fallback para SUPER
-- ------------------------------------------------------------

threshold = (n 0) / (p 0);

checkRec n0 lst = if (len lst) <= n0
  then chunkPair lst
  else case splitImb lst of
    (lft, rgt) ->
      case checkRec n0 lft of
        (s1, m1) ->
          case checkRec n0 rgt of
            (s2, m2) ->
              ( s1 + s2
              , if m1 < (s1 + m2) then m1 else (s1 + m2) );;;
;

-- ------------------------------------------------------------
-- Verificação final
-- ------------------------------------------------------------

validateDyck lst = case checkRec threshold lst of
  (tot, mn) -> (tot == 0) && (mn >= 0);
;

main = validateDyck inputSeq;
"""

def emit_hsk(path, N, P, IMB, DELTA):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    src = (TMPL
           .replace("__N__", str(N))
           .replace("__P__", str(P))
           .replace("__IMB__", str(IMB))
           .replace("__DELTA__", str(DELTA)))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[dyck_gen_input] wrote {path} (N={N}, P={P}, imb={IMB}, delta={DELTA})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--imb", type=int, required=True)
    ap.add_argument("--delta", type=int, required=True)
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.P, args.imb, args.delta)

if __name__ == "__main__":
    main()
