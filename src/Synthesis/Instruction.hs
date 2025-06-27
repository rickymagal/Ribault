{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RecordWildCards #-}

-- | Low‑level data‑flow IR nodes equivalent to Couillard/TALM’s Python classes.
--   Pure data – no traversal – plus small helpers (fresh IDs, parallel check).
--   Imported by Builder → GraphViz → Assembly.
--   Every constructor’s first field is its unique 'NodeId', so
--   `instance Show Inst` shows just that ID, replicating Couillard output.

module Synthesis.Instruction
  ( -- * Node identity
    NodeId(..)
  , freshNodeId
    -- * Ports & Signals
  , SteerBranch(..)
  , ParAttr(..)
  , Signal(..)
    -- * Data‑flow instructions
  , Inst(..)
    -- * Utilities
  , IsParallel(..)
  ) where

import           Control.DeepSeq       (NFData)
import           Control.Monad.State   (State, get, put)
import           GHC.Generics          (Generic)

--------------------------------------------------------------------------------
-- Node IDs --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Unique identifier for every instruction node (e.g. "n42").
newtype NodeId = NodeId { unN :: Int }
  deriving (Eq, Ord, Generic, NFData)

instance Show NodeId where
  show (NodeId n) = 'n' : show n

-- | Monad‑state helper to allocate fresh IDs.
freshNodeId :: State Int NodeId
freshNodeId = do
  n <- get
  put (n + 1)
  pure (NodeId n)

--------------------------------------------------------------------------------
-- Ports & Signals --------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Branch selector used by Steer output ports.
data SteerBranch = T | F
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Attributes attached to super‑instruction parallel outputs.
--   Mirrors Couillard’s a/s/op/val/local/starter/last flags.
data ParAttr = ParAttr
  { parIndex   :: !Int      -- ^ position in the output list
  , parAll     :: !Bool     -- ^ broadcast to all tasks (a)
  , parSelf    :: !Bool     -- ^ connect to same‑index task (s)
  , parOp      :: !(Maybe Char) -- ^ '+' or '-' from mytid ± k
  , parVal     :: !Int      -- ^ k in mytid ± k
  , parLocal   :: !Bool     -- ^ local input/output?
  , parStarter :: !Bool     -- ^ starter‑edge (control‑only)
  , parLast    :: !Bool     -- ^ last task special case
  } deriving (Eq, Ord, Show, Generic, NFData)

-- | Connection between nodes (folds InstPort/SteerPort/ReturnPort).
data Signal
  = SigInstPort   { sigNode :: !NodeId, sigPort :: !Int, sigPar :: !(Maybe ParAttr) }
  | SigSteerPort  { sigNode :: !NodeId, sigBranch :: !SteerBranch }
  | SigReturnPort { retFunc :: !String, retGroup :: !String }
  deriving (Eq, Ord, Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Instruction nodes ------------------------------------------------------------
--------------------------------------------------------------------------------

data Inst
  -- Literals & arithmetic ------------------------------------------------------
  = InstConst  { nodeId :: !NodeId, litVal :: !Integer, litType :: !String }
  | InstBinop  { nodeId :: !NodeId, binOp  :: !String, typStr  :: !String
               , leftSrc :: ![Signal], rightSrc :: ![Signal] }
  | InstBinopI { nodeId :: !NodeId, binOp  :: !String, typStr  :: !String
               , immedVal :: !Integer, uniSrc :: ![Signal] }
  -- Control --------------------------------------------------------------------
  | InstSteer  { nodeId :: !NodeId, steerExpr :: ![Signal], steerInp :: ![Signal] }
  | InstIncTag { nodeId :: !NodeId, tagInp    :: ![Signal]
               , overrFlags :: ![Bool] }
  -- Super‑instructions ---------------------------------------------------------
  | InstSuper  { nodeId :: !NodeId, superNum :: !Int
               , superInp :: ![[Signal]], superOutN :: !Int
               , inProp    :: ![[ParAttr]] }
  | InstPar    { nodeId :: !NodeId, parNum :: !Int
               , parInp :: ![[Signal]], parOutN :: !Int
               , inProp  :: ![[ParAttr]] }
  -- Calls & returns ------------------------------------------------------------
  | InstReturn { nodeId :: !NodeId, funName :: !String
               , retExpr :: ![Signal], retSend :: ![Signal] }
  | InstCallGrp { nodeId :: !NodeId, cgFun :: !String, cgName :: !String }
  | InstCallSnd { nodeId :: !NodeId, csFun :: !String, csGroup :: !String
                , csIdx :: !Int, csOper :: ![Signal] }
  | InstRetSnd  { nodeId :: !NodeId, rsFun :: !String, rsGroup :: !String
                , rsOper :: ![Signal] }
  deriving (Eq, Generic, NFData)

-- | DOT / ASM want only the ID when printing a node.
instance Show Inst where
  show = show . nodeId

--------------------------------------------------------------------------------
-- Parallelism helper -----------------------------------------------------------
--------------------------------------------------------------------------------

class IsParallel a where
  isParallel :: a -> Bool

instance IsParallel Inst where
  isParallel InstPar{}         = True
  isParallel InstSuper{}       = False
  isParallel InstSteer{..}     = any isParallel steerInp
  isParallel InstIncTag{..}    = any isParallel tagInp
  isParallel InstBinop{..}     = any isParallel leftSrc || any isParallel rightSrc
  isParallel InstBinopI{..}    = any isParallel uniSrc
  isParallel InstConst{}       = False
  isParallel InstReturn{..}    = any isParallel retExpr
  isParallel InstCallSnd{..}   = any isParallel csOper
  isParallel InstRetSnd{..}    = any isParallel rsOper
  isParallel _                 = False  -- CallGrp has no dynamic value

instance IsParallel Signal where
  isParallel SigInstPort{..} = maybe False parLocal sigPar
  isParallel _               = False
