{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- coloque este arquivo em src/Synthesis/SSA.hs
module Synthesis.SSA (ssaTransform) where

import           Types
import           Node
import           Unique                    ( Unique, evalUnique, freshId )

import qualified Data.Map.Strict           as Map
import           Data.Graph                ( SCC(..), stronglyConnComp )
import           Control.Monad.State.Strict

----------------------------------------------------------------------
-- | Transforma um DGraph em SSA, inserindo InstIncTag onde houver ciclos
----------------------------------------------------------------------
ssaTransform :: DGraph DNode -> DGraph DNode
ssaTransform g0@DGraph{..}
  | null backEdges = g0
  | otherwise      = evalUnique $ execStateT (mapM_ insertInc backEdges) g0
  where
    -- mapeia cada nó ao seu SCC
    sccMap :: Map.Map NodeId (Map.Map NodeId ())
    sccMap = nodeToScc g0

    -- toda aresta que permanece dentro de um mesmo SCC (e não é trivial) é "back‐edge"
    backEdges :: [Edge]
    backEdges =
      [ e
      | e@(s, _, d, _) <- dgEdges
      , Just cs <- [Map.lookup s sccMap]
      , Just cd <- [Map.lookup d sccMap]
      , cs == cd, s /= d, Map.size cs > 1
      ]

----------------------------------------------------------------------
-- | Insere, para cada back‐edge, um nó InstIncTag (phi‐like)
----------------------------------------------------------------------
type M = StateT (DGraph DNode) Unique

insertInc :: Edge -> M ()
insertInc (src, sp, dst, dp) = do
  nid <- lift freshId
  let tagNode = InstIncTag nid src
      e1      = (src, sp,  nid, "in")
      e2      = (nid, "out", dst, dp)
  modify' $ \g@DGraph{..} ->
    g { dgNodes = Map.insert nid tagNode dgNodes
      , dgEdges = e1 : e2 : filter (/= (src,sp,dst,dp)) dgEdges
      }

----------------------------------------------------------------------
-- | Constroi mapa NodeId → SCC
----------------------------------------------------------------------
nodeToScc :: DGraph DNode -> Map.Map NodeId (Map.Map NodeId ())
nodeToScc DGraph{..} = Map.fromList pairs
  where
    vertices = [ (nId n, nId n, succs (nId n)) | n <- Map.elems dgNodes ]
    succs v  = [ d | (s, _, d, _) <- dgEdges, s == v ]
    comps    = stronglyConnComp vertices

    toPairs (AcyclicSCC v) = [(v, Map.singleton v ())]
    toPairs (CyclicSCC vs) =
      let set = Map.fromList [ (x,()) | x <- vs ]
      in  [ (v, set) | v <- vs ]

    pairs = concatMap toPairs comps
