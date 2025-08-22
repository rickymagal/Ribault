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

-- Data-flow node: one constructor per assembler mnemonic
data DNode
  -- Constants
  = NConstI  { nName :: !String, cInt    :: !Int    }
  | NConstF  { nName :: !String, cFloat  :: !Float  }
  | NConstD  { nName :: !String, cDouble :: !Double }

  -- Binary ALU
  | NAdd     { nName :: !String }
  | NSub     { nName :: !String }
  | NMul     { nName :: !String }
  | NDiv     { nName :: !String }                    -- 2 outputs
  | NFAdd    { nName :: !String }
  | NDAdd    { nName :: !String }
  | NBand    { nName :: !String }

  -- Immediate ALU
  | NAddI    { nName :: !String, iImm :: !Int   }
  | NSubI    { nName :: !String, iImm :: !Int   }
  | NMulI    { nName :: !String, iImm :: !Int   }
  | NFMulI   { nName :: !String, fImm :: !Float }
  | NDivI    { nName :: !String, iImm :: !Int   }    -- 2 outputs

  -- Comparisons / control
  | NLThan   { nName :: !String }
  | NGThan   { nName :: !String }
  | NEqual   { nName :: !String }
  | NLThanI  { nName :: !String, iImm :: !Int }
  | NGThanI  { nName :: !String, iImm :: !Int }
  | NSteer   { nName :: !String }                    -- outputs “.t” / “.f”

  -- Tag increment
  | NIncTag  { nName :: !String }
  | NIncTagI { nName :: !String, iImm :: !Int }

  -- Calls
  | NCallGroup { nName :: !String }
  | NCallSnd   { nName :: !String, taskId :: !Int }
  | NRetSnd    { nName :: !String, taskId :: !Int }
  | NRet       { nName :: !String }

  -- Tag ↔ value converters
  | NTagVal  { nName :: !String }
  | NValTag  { nName :: !String }

  -- GPU / memory
  | NCpHToDev { nName :: !String }
  | NCpDevToH { nName :: !String }

  -- Speculation / commit
  | NCommit   { nName :: !String }                  -- 2 outputs
  | NStopSpec { nName :: !String }                  -- 2 outputs

  -- Super-instruction
  | NSuper
      { nName     :: !String
      , superNum  :: !Int
      , superOuts :: !Int
      , superImm  :: !(Maybe Int)
      , superSpec :: !Bool
      }
  deriving (Eq, Show)

-- Helpers ---------------------------------------------------------------------

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
outPort  nid = InstPort  nid "0"

out1Port :: NodeId -> Port
out1Port nid = InstPort  nid "1"

truePort :: NodeId -> Port
truePort nid = SteerPort nid "t"

falsePort :: NodeId -> Port
falsePort nid = SteerPort nid "f"
