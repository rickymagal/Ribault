{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Node
  ( DNode(..)
  , nodeName
  , nOutputs
  , outPort, out1Port, truePort, falsePort
  ) where

import           Types (NodeId)
import           Port  (Port(..))

-- Nó de dataflow (espelha os mnemônicos do assembly)
data DNode
  -- Constantes
  = NConstI  { nName :: !String, cInt   :: !Int }
  | NConstF  { nName :: !String, cFloat :: !Float }
  | NConstD  { nName :: !String, cDouble:: !Double }

  -- ALU binárias
  | NAdd     { nName :: !String }
  | NSub     { nName :: !String }
  | NMul     { nName :: !String }
  | NDiv     { nName :: !String }      -- 2 saídas
  | NFAdd    { nName :: !String }
  | NDAdd    { nName :: !String }
  | NBand    { nName :: !String }

  -- ALU imediatas
  | NAddI    { nName :: !String, iImm :: !Int }
  | NSubI    { nName :: !String, iImm :: !Int }
  | NMulI    { nName :: !String, iImm :: !Int }
  | NFMulI   { nName :: !String, fImm :: !Float }
  | NDivI    { nName :: !String, iImm :: !Int }  -- 2 saídas

  -- Comparações e steer
  | NLThan   { nName :: !String }
  | NGThan   { nName :: !String }
  | NEqual   { nName :: !String }
  | NLThanI  { nName :: !String, iImm :: !Int }
  | NGThanI  { nName :: !String, iImm :: !Int }
  | NSteer   { nName :: !String }                -- saídas "t"/"f"

  -- Chamadas (instrumentação TALM)
  | NCallGroup { nName :: !String }              -- callgroup (gera tag)
  | NCallSnd   { nName :: !String, taskId :: !Int }  -- callsnd <tid>
  | NRetSnd    { nName :: !String, taskId :: !Int }  -- retsnd <tid>
  | NRet       { nName :: !String }              -- ret

  -- Conversores tag <-> valor
  | NTagVal  { nName :: !String }
  | NValTag  { nName :: !String }

  -- DMA / commit / especulação
  | NCpHToDev  { nName :: !String }
  | NCpDevToH  { nName :: !String }
  | NCommit    { nName :: !String }             -- 2 saídas
  | NStopSpec  { nName :: !String }             -- 2 saídas

  -- Super-instrução (só quando a AST trouxer explicitamente)
  | NSuper
      { nName      :: !String
      , superNum   :: !Int
      , superOuts  :: !Int
      , superImm   :: !(Maybe Int)
      , superSpec  :: !Bool
      }
  deriving (Eq, Show)

nodeName :: DNode -> String
nodeName = nName

nOutputs :: DNode -> Int
nOutputs = \case
  NDiv{}      -> 2
  NDivI{}     -> 2
  NSteer{}    -> 2
  NCommit{}   -> 2
  NStopSpec{} -> 2
  NSuper{..}  -> superOuts
  _           -> 1

outPort  :: NodeId -> Port
outPort  nid = InstPort nid "0"
out1Port :: NodeId -> Port
out1Port nid = InstPort nid "1"
truePort :: NodeId -> Port
truePort nid = SteerPort nid "t"
falsePort :: NodeId -> Port
falsePort nid = SteerPort nid "f"
