-- Source/Types.hs
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Tipos centrais da fase de geração de Data-flow.
--   Mantemos tudo genérico em @n@ para que outros módulos (Node, Builder…)
--   escolham o payload de cada nó.
module Types
  ( NodeId
  , PortId
  , Edge
  , DGraph(..)
  , emptyGraph
  , addNode
  , addEdge
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Identificadores básicos
--------------------------------------------------------------------------------

-- | Identificador único de um nó/instrução no grafo.
type NodeId = Int

-- | Nome da porta dentro de um nó (\"out\", \"lhs\", \"rhs\", \"t\", \"f\"…).
type PortId = String

--------------------------------------------------------------------------------
-- Arestas e grafos
--------------------------------------------------------------------------------

-- | Aresta direcionada: (nó de origem, porta de origem, nó destino, porta destino).
type Edge = (NodeId, PortId, NodeId, PortId)

-- | Grafo paramétrico: permite qualquer tipo de nó como payload.
data DGraph n = DGraph
  { dgNodes :: Map NodeId n   -- ^ Todos os nós, indexados pelo seu 'NodeId'.
  , dgEdges :: [Edge]         -- ^ Lista de arestas direcionadas.
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------
-- Utilidades de construção
--------------------------------------------------------------------------------

-- | Grafo vazio — ponto de partida para o 'Builder'.
emptyGraph :: DGraph n
emptyGraph = DGraph Map.empty []

-- | Insere (ou substitui) um nó.
addNode :: NodeId -> n -> DGraph n -> DGraph n
addNode nid n g = g { dgNodes = Map.insert nid n (dgNodes g) }

-- | Adiciona uma aresta (é mais barato ‘consar’; inverta depois se a ordem importar).
addEdge :: Edge -> DGraph n -> DGraph n
addEdge e g = g { dgEdges = e : dgEdges g }
