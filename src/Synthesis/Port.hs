-- Source/Port.hs
{-# LANGUAGE RecordWildCards #-}

-- | Abstrações de portas de nó (equivalente aos SteerPort / InstPort do compilador-C).
--   Também define helpers para construir arestas de maneira segura.
module Port
  ( -- * Tipos
    Port(..)
  , portNode          -- extrai o NodeId
  , portId            -- extrai o nome da porta

    -- * Conectores
  , (-->), edge
  ) where

import           Types  (NodeId, PortId, Edge)

--------------------------------------------------------------------------------
-- Porta interna
--------------------------------------------------------------------------------

-- | “Saída de nó” ou “ponto de conexão” em ‘GraphViz’.
data Port
  = InstPort  { pNode :: !NodeId, pName :: !PortId }   -- ^ porta genérica
  | SteerPort { pNode :: !NodeId, pName :: !PortId }   -- ^ "t" / "f"
  deriving (Eq, Ord, Show)

-- | Extrai o ‘NodeId’.
portNode :: Port -> NodeId
portNode = pNode

-- | Extrai o nome da porta.
portId :: Port -> PortId
portId = pName

--------------------------------------------------------------------------------
-- Construção de arestas
--------------------------------------------------------------------------------

infixr 1 -->
-- | Syntactic sugar: @a --> b@ monta uma aresta ‘out → in’.
(--> ) :: Port            -- ^ origem
       -> Port            -- ^ destino
       -> Edge
a --> b = ( portNode a, portId a
          , portNode b, portId b )

-- | Constrói uma aresta explicitando todos os nomes de porta.
edge :: NodeId -> PortId -> NodeId -> PortId -> Edge
edge = (,,,)
