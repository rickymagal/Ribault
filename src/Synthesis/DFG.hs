{-# LANGUAGE RecordWildCards #-}

-- | Estrutura de grafo usada pelos visitantes (GraphViz / Codegen).
--   Cada instrução vira um nó; as arestas vêm dos sinais-fonte.

module Synthesis.DFG
  ( Node(..)        -- nó = inst + id
  , DFG(..)         -- grafo = nós + adjacências
  , emptyDFG
  , addNode         -- inserir nó e arestas
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Synthesis.Instruction

-- ───────────────────────
data Node = Node
  { nId   :: NodeId   -- ^ identificador “n42”
  , nInst :: Inst     -- ^ instrução original
  } deriving (Eq, Show)

data DFG = DFG
  { dNodes :: M.Map NodeId Node          -- ^ todos os nós
  , dSuccs :: M.Map NodeId (S.Set NodeId) -- ^ sucessores
  } deriving Show

emptyDFG :: DFG
emptyDFG = DFG M.empty M.empty

-- | Adiciona nó + arestas src→dst.
addNode :: Inst      -- ^ instrução (destino)
        -> [NodeId]  -- ^ fontes
        -> DFG -> DFG
addNode inst srcs g@DFG{..} =
  let dst      = nodeId inst
      n        = Node dst inst
      dNodes'  = M.insert dst n dNodes
      bump m s = M.insertWith S.union s (S.singleton dst) m
      dSuccs'  = foldl bump dSuccs srcs
  in g{ dNodes = dNodes', dSuccs = dSuccs' }
