{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Node
  ( -- * Literais e operadores
    Literal(..)
  , BinOp(..)
  , UnOp(..)

    -- * Nó
  , DNode(..)

    -- * Helpers
  , nodeName
  , outPort, truePort, falsePort
  ) where

import           Types            (NodeId)
import           Data.Text        (Text)

----------------------------------------------------------------------
-- Literais (para const)
----------------------------------------------------------------------
data Literal
  = LInt    Int
  | LFloat  Double
  | LBool   Bool
  | LChar   Char
  | LString Text
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Operadores binários
----------------------------------------------------------------------
data BinOp
  = BAdd | BSub | BMul | BDiv
  | BAddI | BSubI | BMulI | BDivI
  | BAnd | BOr | BXor | BNot
  | BEq | BNeq
  | BLt | BLeq | BGt | BGeq
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Operador unário (só not, se quiser mais adicione aqui)
----------------------------------------------------------------------
data UnOp = UNot
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Nós de instrução TALM/DFG
----------------------------------------------------------------------
data DNode
  = InstConst     { nId :: NodeId, lit :: Literal }

  -- Aritméticos
  | InstAdd       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstSub       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstMul       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstDiv       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }

  | InstAddI      { nId :: NodeId, lhs :: NodeId, imm :: Int }
  | InstSubI      { nId :: NodeId, lhs :: NodeId, imm :: Int }
  | InstMulI      { nId :: NodeId, lhs :: NodeId, imm :: Int }
  | InstDivI      { nId :: NodeId, lhs :: NodeId, imm :: Int }

  -- Booleanos e lógica
  | InstAnd       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstOr        { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstXor       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstNot       { nId :: NodeId, arg :: NodeId }

  -- Comparação
  | InstEq        { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstNeq       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstLt        { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstLeq       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstGt        { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }
  | InstGeq       { nId :: NodeId, lhs :: NodeId, rhs :: NodeId }

  -- Fluxo de controle
  | InstSteer     { nId :: NodeId, predN :: NodeId }

  -- Chamadas de função/dataflow
  | InstCallGroup { nId :: NodeId, groupName :: Text }
  | InstCallSnd   { nId :: NodeId, groupName :: Text, callGroupId :: NodeId, argId :: NodeId }
  | InstRetSnd    { nId :: NodeId, groupName :: Text, callGroupId :: NodeId, retValId :: NodeId }
  | InstRet       { nId :: NodeId, valId :: NodeId, retSndIds :: [NodeId] }

  -- Tags/dataflow
  | InstIncTag    { nId :: NodeId, base :: NodeId }
  | InstTagOp     { nId :: NodeId, arg :: NodeId }

  -- Superinstrução
  | InstSuper     { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }
  | InstSuperInst { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }

  -- Composição e paralelismo
  | InstMerge     { nId :: NodeId, inputs :: [NodeId] }
  | InstSplit     { nId :: NodeId, input :: NodeId, n :: Int }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
nodeName :: DNode -> String
nodeName n = prefix n ++ show (nId n)
  where
    prefix InstConst{}     = "const"
    prefix InstAdd{}       = "add"
    prefix InstSub{}       = "sub"
    prefix InstMul{}       = "mul"
    prefix InstDiv{}       = "div"
    prefix InstAddI{}      = "addi"
    prefix InstSubI{}      = "subi"
    prefix InstMulI{}      = "muli"
    prefix InstDivI{}      = "divi"
    prefix InstAnd{}       = "and"
    prefix InstOr{}        = "or"
    prefix InstXor{}       = "xor"
    prefix InstNot{}       = "not"
    prefix InstEq{}        = "eq"
    prefix InstNeq{}       = "neq"
    prefix InstLt{}        = "lt"
    prefix InstLeq{}       = "leq"
    prefix InstGt{}        = "gt"
    prefix InstGeq{}       = "geq"
    prefix InstSteer{}     = "steer"
    prefix InstCallGroup{} = "callgroup"
    prefix InstCallSnd{}   = "callsnd"
    prefix InstRetSnd{}    = "retsnd"
    prefix InstRet{}       = "ret"
    prefix InstIncTag{}    = "inctag"
    prefix InstTagOp{}     = "tagop"
    prefix InstSuper{}     = "super"
    prefix InstSuperInst{} = "superinst"
    prefix InstMerge{}     = "merge"
    prefix InstSplit{}     = "split"

outPort, truePort, falsePort :: String
outPort  = "out"
truePort = "t"
falsePort = "f"
