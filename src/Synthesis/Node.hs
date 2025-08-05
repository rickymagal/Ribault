{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Node
  ( -- * Literais e operadores
    Literal(..)
  , BinOp(..)
  , UnaryOp(..)

    -- * Nó
  , DNode(..)

    -- * Helpers
  , nodeName
  , outPort, truePort, falsePort
  ) where

import           Types            (NodeId)
import           Data.Text        (Text)
import qualified Data.Text        as T

----------------------------------------------------------------------
-- Literais
----------------------------------------------------------------------
data Literal
  = LInt    Int
  | LFloat  Double
  | LBool   Bool
  | LChar   Char
  | LString Text
  | LUnit
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Operadores
----------------------------------------------------------------------
data BinOp
  = BAdd | BSub | BMul | BDiv | BMod
  | BAnd | BOr  | BXor
  | BLt  | BGt  | BLe  | BGe | BEq | BNe
  | BCons
  deriving (Eq, Show)

data UnaryOp = UNeg | UNot | UIsNil
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Nós de instrução TALM/DFG
----------------------------------------------------------------------
data DNode
  = InstConst   { nId :: NodeId, lit   :: Literal }
  | InstBinop   { nId :: NodeId, op    :: BinOp,  lhs, rhs :: NodeId }
  | InstBinopI  { nId :: NodeId, opI   :: BinOp,  lhs :: NodeId, imm :: Int }
  | InstUnary   { nId :: NodeId, unop  :: UnaryOp, arg :: NodeId }
  | InstSteer   { nId :: NodeId, predN :: NodeId }
  | InstIncTag  { nId :: NodeId, base  :: NodeId }
  | InstTuple   { nId :: NodeId, fields :: [NodeId] }
  | InstProj    { nId :: NodeId, idx :: Int, tuple :: NodeId }
  -- | DEPRECATED: Só usado para transição, não mais emitido
  | InstSuper   { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }
  | InstPar     { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }

  -- Novos nós TALM para chamadas de função!
  | InstCallGroup { nId :: NodeId, groupName :: Text }   -- callgroup('foo12', 'foo')
  | InstCallSnd   { nId :: NodeId, groupName :: Text, callGroupId :: NodeId, argId :: NodeId } -- callsnd foo[1], flowInst9, foo12
  | InstRetSnd    { nId :: NodeId, groupName :: Text, callGroupId :: NodeId, retValId :: NodeId } -- retsnd foo[0], flowInst10, foo12
  | InstRet       { nId :: NodeId, valId :: NodeId, retSndIds :: [NodeId] }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
nodeName :: DNode -> String
nodeName n = prefix n ++ show (nId n)
  where
    prefix InstConst{}     = "flowInstConst"
    prefix InstBinop{}     = "flowInstBinop"
    prefix InstBinopI{}    = "flowInstBinopI"
    prefix InstUnary{}     = "flowInstUnary"
    prefix InstSteer{}     = "flowInstSteer"
    prefix InstIncTag{}    = "flowInstIncTag"
    prefix InstTuple{}     = "flowInstTuple"
    prefix InstProj{}      = "flowInstProj"
    prefix InstSuper{}     = "flowInstSuper"     -- (DEPRECATED)
    prefix InstPar{}       = "flowInstPar"       -- (DEPRECATED)
    prefix InstCallGroup{} = "flowInstCallGroup"
    prefix InstCallSnd{}   = "flowInstCallSnd"
    prefix InstRetSnd{}    = "flowInstRetSnd"
    prefix InstRet{}       = "flowInstRet"

outPort, truePort, falsePort :: String
outPort  = "out"
truePort = "t"
falsePort = "f"
