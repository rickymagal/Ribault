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
  | LFloat  Double          -- ←  NOVO  ------------------------------
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
-- Nós de instrução  (idêntico ao anterior; só omitido para brevidade)
----------------------------------------------------------------------
data DNode
  = InstConst  { nId :: NodeId, lit   :: Literal }
  | InstBinop  { nId :: NodeId, op    :: BinOp,  lhs, rhs :: NodeId }
  | InstBinopI { nId :: NodeId, opI   :: BinOp,  lhs :: NodeId, imm :: Int }
  | InstUnary  { nId :: NodeId, unop  :: UnaryOp, arg :: NodeId }
  | InstSteer  { nId :: NodeId, predN :: NodeId }
  | InstIncTag { nId :: NodeId, base  :: NodeId }
  | InstTuple  { nId :: NodeId, fields :: [NodeId] }
  | InstProj   { nId :: NodeId, idx :: Int, tuple :: NodeId }
  | InstSuper  { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }
  | InstPar    { nId :: NodeId, name :: Text, ins :: [NodeId], outs :: Int }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
nodeName :: DNode -> String
nodeName n = prefix n ++ show (nId n)
  where
    prefix InstConst{}  = "flowInstConst"
    prefix InstBinop{}  = "flowInstBinop"
    prefix InstBinopI{} = "flowInstBinopI"
    prefix InstUnary{}  = "flowInstUnary"
    prefix InstSteer{}  = "flowInstSteer"
    prefix InstIncTag{} = "flowInstIncTag"
    prefix InstTuple{}  = "flowInstTuple"
    prefix InstProj{}   = "flowInstProj"
    prefix InstSuper{}  = "flowInstSuper"
    prefix InstPar{}    = "flowInstPar"

outPort, truePort, falsePort :: String
outPort  = "out"
truePort = "t"
falsePort = "f"
