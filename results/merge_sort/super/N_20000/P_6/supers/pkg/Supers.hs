{-# LANGUAGE ForeignFunctionInterface #-}
-- Gerado automaticamente para o programa: 21_merge_sort_super
module Supers where

import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import Data.Int (Int64)

-- Perfil B: sN :: Ptr Int64 -> Ptr Int64 -> IO ()
-- Contrato: lê in[0] e escreve em out[0].

-- Helpers de codificação compatíveis com o Builder:
pairBase :: Int64
pairBase = 1000003

nil :: Int64
nil = -1

encPair :: Int64 -> Int64 -> Int64
encPair a b = a * pairBase + b

fstDec :: Int64 -> Int64
fstDec p = p `div` pairBase

sndDec :: Int64 -> Int64
sndDec p = p - (p `div` pairBase) * pairBase

toList :: Int64 -> [Int64]
toList n = go 0 n
  where
    go k m
      | m == nil  = []
      | m == 0    = []  -- trata entrada não inicializada/lixo
      | k > 2000000 = [] -- trava-dura anti-loop
      | otherwise = let h = fstDec m; t = sndDec m in h : go (k+1) t

fromList :: [Int64] -> Int64
fromList []     = nil
fromList (h:ts) = encPair h (fromList ts)

-- s1
foreign export ccall "s1" s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
foreign export ccall "super0" s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 pin pout = do
  x <- peek pin
  let r = s1_impl x
  poke pout r

-- Função pura interna:
-- - decodifica a entrada Int64 para lista em 'lst'
-- - executa o corpo salvo na AST (declarações + definição de 'sorted')
-- - codifica 'sorted' de volta para Int64
s1_impl :: Int64 -> Int64
s1_impl x =
  let
    lst = toList x
    -- serial mergesort fallback (pure)
    mmerge xs ys = case xs of
      []     -> ys;
      (x:xt) -> case ys of
        []     -> xs;
        (y:yt) -> if x <= y
                  then x : mmerge xt (y:yt)
                  else y : mmerge (x:xt) yt;;
    ;
    
    msplit lst = case lst of
      []     -> ([], []);
      [x]    -> ([x], []);
      x:y:zs -> case msplit zs of
        (xs, ys) -> (x:xs, y:ys);;
    ;
    
    msort lst = case lst of
      []     -> [];
      (x:[]) -> [x];
      _      -> case msplit lst of
        (l, r) -> mmerge (msort l) (msort r);;
    ;
    
    sorted = msort lst
  in fromList sorted
