{-# LANGUAGE LambdaCase #-}

-- Extrai as Supers da AST já com nomes s# (depois de assignSuperNames).
module Synthesis.SuperExtract
  ( SuperSpec(..)
  , collectSupers
  ) where

import Syntax
import Data.List (nubBy)

-- Metadados necessários para emitir o módulo de supers
data SuperSpec = SuperSpec
  { ssName :: Ident        -- "s1", "s2", ...
  , ssKind :: SuperKind    -- metadado (não usamos na ABI)
  , ssInp  :: Ident        -- nome lógico da entrada
  , ssOut  :: Ident        -- nome lógico da saída
  , ssBody :: String       -- corpo textual salvo na AST (declarações/expressão)
  }

-- Remove duplicatas por nome (se a Super aparecer múltiplas vezes na AST)
dedupByName :: [SuperSpec] -> [SuperSpec]
dedupByName = nubBy (\a b -> ssName a == ssName b)

collectSupers :: Program -> [SuperSpec]
collectSupers (Program decls) = dedupByName (concatMap declSupers decls)
  where
    declSupers :: Decl -> [SuperSpec]
    declSupers (FunDecl _ _ e) = exprSupers e

    exprSupers :: Expr -> [SuperSpec]
    exprSupers = \case
      Var _            -> []
      Lit _            -> []
      Lambda _ b       -> exprSupers b
      If a b c         -> exprSupers a ++ exprSupers b ++ exprSupers c
      Case scr alts    -> exprSupers scr ++ concat [ exprSupers rhs | (_pat, rhs) <- alts ]
      Let ds body      -> concatMap declSupers ds ++ exprSupers body
      App f x          -> exprSupers f ++ exprSupers x
      BinOp _ l r      -> exprSupers l ++ exprSupers r
      UnOp  _ e        -> exprSupers e
      List xs          -> concatMap exprSupers xs
      Tuple xs         -> concatMap exprSupers xs
      Cons h t         -> exprSupers h ++ exprSupers t
      Super nm k i o s -> [ SuperSpec nm k i o s ]
