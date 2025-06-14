{-# LANGUAGE DeriveGeneric #-}

-- | LibSupers: construtores e matcher genérico de padrões
module Lib.Supers
  ( Value(..)
  , PatternDef(..)
  , makeList
  , makeTuple
  , matchSeq
  ) where

import           GHC.Generics (Generic)

-- | Representação genérica de valores em tempo de execução
data Value
  = VInt   Int
  | VBool  Bool
  | VList  [Value]
  | VTup   [Value]        -- tupla de aridade arbitrária
  deriving (Show, Eq, Generic)

-- | Definição de um único padrão atômico
data PatternDef
  = PVar               -- corresponde a qualquer valor
  | PWildcard          -- ignora o valor
  | PConstInt   Int    -- corresponde apenas a esse inteiro exato
  | PConstBool  Bool   -- corresponde apenas a esse booleano exato
  deriving (Show, Eq, Generic)

-- | Constrói uma lista em Value a partir de N elementos
makeList :: [Value] -> Value
makeList xs = VList xs

-- | Constrói uma tupla em Value a partir de N elementos
makeTuple :: [Value] -> Value
makeTuple xs = VTup xs

-- | Testa uma sequência de padrões (para listas ou tuplas) contra um Value.
--   Retorna True se e somente se:
--     - value for VList vs ou VTup vs
--     - length ps == length vs
--     - cada PatternDef ps[i] casa com vs[i]
matchSeq :: [PatternDef] -> Value -> Bool
matchSeq ps val = case val of
  VList vs
    | length vs == length ps
    -> and (zipWith matchValue ps vs)
    | otherwise
    -> False

  VTup vs
    | length vs == length ps
    -> and (zipWith matchValue ps vs)
    | otherwise
    -> False

  _ ->
    False

-- | Testa um PatternDef atômico contra um Value
matchValue :: PatternDef -> Value -> Bool
matchValue PVar           _         = True
matchValue PWildcard      _         = True
matchValue (PConstInt i)  (VInt j)  = i == j
matchValue (PConstBool b) (VBool c) = b == c
matchValue _              _         = False
