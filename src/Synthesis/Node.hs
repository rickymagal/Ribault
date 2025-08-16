{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- | Declaração dos nós do grafo dataflow (payload por NodeId).
--   Cada construtor corresponde diretamente a um mnemônico do assembler.
--   Convenções de portas:
--     - Saída padrão: "0"
--     - Segunda saída (quando houver): "1"
--     - Steer: "t" e "f"
--   Use os helpers 'outPort', 'out1Port', 'truePort', 'falsePort'.
module Node
  ( -- * Nó do grafo
    DNode(..)

    -- * Metadados
  , nodeName
  , nOutputs

    -- * Port helpers (para construir arestas)
  , outPort
  , out1Port
  , truePort
  , falsePort
  ) where

import           Types (NodeId)
import           Port  (Port(..))

--------------------------------------------------------------------------------
-- Tipo de nó (payload)
--------------------------------------------------------------------------------

-- | Nó de dataflow correspondente aos mnemônicos do assembler.
--   Campos de nome (nName) existem para rótulo humano; o 'NodeId' vem do grafo.
data DNode
  -- Constantes
  = NConstI  { nName :: !String, cInt   :: !Int }      -- const
  | NConstF  { nName :: !String, cFloat :: !Float }    -- fconst
  | NConstD  { nName :: !String, cDouble:: !Double }   -- dconst

  -- ALU binárias (1 saída)
  | NAdd     { nName :: !String }  -- add (int)
  | NSub     { nName :: !String }  -- sub (int)
  | NMul     { nName :: !String }  -- mul (int)
  | NFAdd    { nName :: !String }  -- fadd (float)
  | NDAdd    { nName :: !String }  -- dadd (double)
  | NBand    { nName :: !String }  -- band (bitwise AND)

  -- ALU binárias (2 saídas)
  | NDiv     { nName :: !String }  -- div (int -> quociente e resto/seg. saída)

  -- ALU com imediato (1 fonte + immed)
  | NAddI    { nName :: !String, iImm :: !Int   } -- addi
  | NSubI    { nName :: !String, iImm :: !Int   } -- subi
  | NMulI    { nName :: !String, iImm :: !Int   } -- muli
  | NFMulI   { nName :: !String, fImm :: !Float } -- fmuli
  | NDivI    { nName :: !String, iImm :: !Int   } -- divi (2 saídas)

  -- Comparações e steer
  | NLThan   { nName :: !String }                 -- lthan
  | NGThan   { nName :: !String }                 -- gthan
  | NEqual   { nName :: !String }                 -- equal
  | NLThanI  { nName :: !String, iImm :: !Int }   -- lthani
  | NGThanI  { nName :: !String, iImm :: !Int }   -- gthani
  | NSteer   { nName :: !String }                 -- steer (2 saídas: t/f)

  -- Controle de tags / retorno / call
  | NIncTag  { nName :: !String }                 -- inctag
  | NIncTagI { nName :: !String, iImm :: !Int }   -- inctagi
  | NCallSnd { nName :: !String, taskId :: !Int } -- callsnd (imm = task id)
  | NRetSnd  { nName :: !String, taskId :: !Int } -- retsnd (imm = task id)
  | NRet     { nName :: !String }                 -- ret

  -- Converters tag<->val
  | NTagVal  { nName :: !String }                 -- tagval
  | NValTag  { nName :: !String }                 -- valtag

  -- Cópias GPU/host
  | NCpHToDev  { nName :: !String }               -- cphtodev (size, dst, src)
  | NCpDevToH  { nName :: !String }               -- cpdevtoh (size, dst, src)

  -- Commit / especulação
  | NCommit    { nName :: !String }               -- commit (2 saídas)
  | NStopSpec  { nName :: !String }               -- stopspec (2 saídas)

  -- Super-instruções
  | NSuper
      { nName      :: !String
      , superNum   :: !Int          -- ^ número da super-instrução (soma ao opcode base)
      , superOuts  :: !Int          -- ^ quantidade de saídas
      , superImm   :: !(Maybe Int)  -- ^ imediato opcional (presente em 'superi' / 'specsuperi')
      , superSpec  :: !Bool         -- ^ especulativa? (specsuper/specsuperi)
      }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Metadados
--------------------------------------------------------------------------------

-- | Nome legível do nó (rótulo).
nodeName :: DNode -> String
nodeName = nName

-- | Quantidade de saídas desse nó (para nomear portas corretamente).
nOutputs :: DNode -> Int
nOutputs = \case
  NDiv{}      -> 2
  NDivI{}     -> 2
  NSteer{}    -> 2
  NCommit{}   -> 2
  NStopSpec{} -> 2
  NSuper{..}  -> superOuts
  _           -> 1

--------------------------------------------------------------------------------
-- Helpers de porta (para usar com Port.(-->))
--------------------------------------------------------------------------------

-- | Porta de saída padrão ("0").
outPort :: NodeId -> Port
outPort nid = InstPort nid "0"

-- | Segunda saída ("1") — para nós com duas saídas (Div/DivI/Commit/StopSpec).
out1Port :: NodeId -> Port
out1Port nid = InstPort nid "1"

-- | Saída verdadeira ("t") — exclusiva de 'NSteer'.
truePort :: NodeId -> Port
truePort nid = SteerPort nid "t"

-- | Saída falsa ("f") — exclusiva de 'NSteer'.
falsePort :: NodeId -> Port
falsePort nid = SteerPort nid "f"
