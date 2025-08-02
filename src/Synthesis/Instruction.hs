{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RecordWildCards #-}

-- | IR de fluxo-de-dados (nó e fio) inspirado no Couillard/TALM.
--   Apenas dados puros; percursos ficam em Builder / GraphViz / Codegen.

module Synthesis.Instruction
  ( -- * Identidade de nó
    NodeId(..)
  , freshNodeId

    -- * Portas & sinais
  , SteerBranch(..)
  , ParAttr(..)
  , Signal(..)

    -- * Instruções
  , Inst(..)

    -- * Utilidades
  , inputs              -- ^ entradas de uma instrução
  , IsParallel(..)
  ) where

import           Control.DeepSeq       (NFData)
import           Control.Monad.State   (State, get, put)
import           GHC.Generics          (Generic)

--------------------------------------------------------------------------------
-- NodeId ----------------------------------------------------------------------
--------------------------------------------------------------------------------
newtype NodeId = NodeId { unN :: Int }
  deriving (Eq, Ord, Generic, NFData)

instance Show NodeId where
  show (NodeId n) = 'n' : show n

freshNodeId :: State Int NodeId
freshNodeId = do n <- get; put (n+1); pure (NodeId n)

--------------------------------------------------------------------------------
-- Portas & Sinais -------------------------------------------------------------
--------------------------------------------------------------------------------
data SteerBranch = T | F
  deriving (Eq, Ord, Show, Generic, NFData)

data ParAttr = ParAttr
  { parIndex   :: !Int
  , parAll     :: !Bool
  , parSelf    :: !Bool
  , parOp      :: !(Maybe Char)
  , parVal     :: !Int
  , parLocal   :: !Bool
  , parStarter :: !Bool
  , parLast    :: !Bool
  } deriving (Eq, Ord, Show, Generic, NFData)

data Signal
  = SigInstPort   { sigNode :: !NodeId, sigPort :: !Int, sigPar :: !(Maybe ParAttr) }
  | SigSteerPort  { sigNode :: !NodeId, sigBranch :: !SteerBranch }
  | SigReturnPort { retFunc :: !String, retGroup :: !String }
  deriving (Eq, Ord, Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Instruções ------------------------------------------------------------------
--------------------------------------------------------------------------------
data Inst
  -- Literais & operações escalares --------------------------------------------
  = InstConst  { nodeId :: !NodeId, litVal :: !Integer, litType :: !String }
  | InstBinop  { nodeId :: !NodeId, binOp  :: !String, typStr  :: !String
               , leftSrc :: ![Signal], rightSrc :: ![Signal] }
  | InstBinopI { nodeId :: !NodeId, binOp  :: !String, typStr  :: !String
               , immedVal :: !Integer, uniSrc :: ![Signal] }

  -- Tuplas --------------------------------------------------------------------
  | InstMkTuple   { nodeId :: !NodeId, tupArity :: !Int
                  , tupInp  :: ![[Signal]] }
  | InstTupleProj { nodeId :: !NodeId, tupArity :: !Int
                  , fieldIx :: !Int, tupSrc :: ![Signal] }

  -- Listas --------------------------------------------------------------------
  | InstMkList    { nodeId :: !NodeId, listLen :: !Int
                  , listElems :: ![[Signal]] }
  | InstCons      { nodeId :: !NodeId
                  , consHead :: ![Signal], consTail :: ![Signal] }

  -- Controle ------------------------------------------------------------------
  | InstSteer  { nodeId :: !NodeId, steerExpr :: ![Signal], steerInp :: ![Signal] }
  | InstIncTag { nodeId :: !NodeId, tagInp    :: ![Signal]
               , overrFlags :: ![Bool] }

  -- Super / Par ---------------------------------------------------------------
  | InstSuper  { nodeId :: !NodeId, superNum :: !Int
               , superInp :: ![[Signal]], superOutN :: !Int
               , inProp    :: ![[ParAttr]] }
  | InstPar    { nodeId :: !NodeId, parNum :: !Int
               , parInp :: ![[Signal]], parOutN :: !Int
               , inProp  :: ![[ParAttr]] }

  -- Chamadas & retornos -------------------------------------------------------
  | InstReturn   { nodeId :: !NodeId, funName :: !String
                 , retExpr :: ![Signal], retSend :: ![Signal] }
  | InstCallGrp  { nodeId :: !NodeId, cgFun :: !String, cgName :: !String }
  | InstCallSnd  { nodeId :: !NodeId, csFun :: !String, csGroup :: !String
                 , csIdx :: !Int, csOper :: ![Signal] }
  | InstRetSnd   { nodeId :: !NodeId, rsFun :: !String, rsGroup :: !String
                 , rsOper :: ![Signal] }
  deriving (Eq, Generic, NFData)

instance Show Inst where show = show . nodeId

--------------------------------------------------------------------------------
-- Paralelismo -----------------------------------------------------------------
--------------------------------------------------------------------------------
class IsParallel a where isParallel :: a -> Bool

instance IsParallel Inst where
  isParallel = \case
    InstPar{}             -> True
    InstSuper{}           -> False
    InstSteer{..}         -> any isParallel steerInp
    InstIncTag{..}        -> any isParallel tagInp
    InstBinop{..}         -> any isParallel leftSrc || any isParallel rightSrc
    InstBinopI{..}        -> any isParallel uniSrc
    InstMkTuple{..}       -> any (any isParallel) tupInp
    InstTupleProj{..}     -> any isParallel tupSrc
    InstMkList{..}        -> any (any isParallel) listElems
    InstCons{..}          -> any isParallel consHead || any isParallel consTail
    InstConst{}           -> False
    InstReturn{..}        -> any isParallel retExpr
    InstCallSnd{..}       -> any isParallel csOper
    InstRetSnd{..}        -> any isParallel rsOper
    _                     -> False

instance IsParallel Signal where
  isParallel SigInstPort{..} = maybe False parLocal sigPar
  isParallel _               = False

--------------------------------------------------------------------------------
-- Entradas de uma instrução ---------------------------------------------------
--------------------------------------------------------------------------------
inputs :: Inst -> [Signal]
inputs = \case
  InstConst{}               -> []
  InstBinop{..}             -> leftSrc ++ rightSrc
  InstBinopI{..}            -> uniSrc
  InstMkTuple{..}           -> concat tupInp
  InstTupleProj{..}         -> tupSrc
  InstMkList{..}            -> concat listElems
  InstCons{..}              -> consHead ++ consTail
  InstSteer{..}             -> steerExpr ++ steerInp
  InstIncTag{..}            -> tagInp
  InstSuper{..}             -> concat superInp
  InstPar{..}               -> concat parInp
  InstReturn{..}            -> retExpr ++ retSend
  InstCallSnd{..}           -> csOper
  InstRetSnd{..}            -> rsOper
  InstCallGrp{}             -> []
