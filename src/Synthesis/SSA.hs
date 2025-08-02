-- src/Synthesis/SSA.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SSA (ssaTransform) where

import           Types
import           Node
import           Unique                    ( Unique, evalUnique, freshId )

import qualified Data.Map.Strict           as Map
import           Data.Graph                ( SCC(..), stronglyConnComp )
import           Control.Monad.State.Strict

----------------------------------------------------------------------
-- API
----------------------------------------------------------------------
ssaTransform :: DGraph DNode -> DGraph DNode
ssaTransform g@DGraph{..}
  | null backEdges = g
  | otherwise      = evalUnique $ execStateT (mapM_ insertInc backEdges) g
  where
    sccMap :: Map.Map NodeId (Map.Map NodeId ())
    sccMap = nodeToScc g

    backEdges :: [Edge]
    backEdges =
      [ e
      | e@(s, _, d, _) <- dgEdges
      , let ms = Map.lookup s sccMap
            md = Map.lookup d sccMap
      , Just cs <- [ms]
      , Just cd <- [md]
      , cs == cd, s /= d, Map.size cs > 1
      ]

----------------------------------------------------------------------
-- Inserção do InstIncTag
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
-- Mapa NodeId ↦ SCC
----------------------------------------------------------------------
nodeToScc :: DGraph DNode -> Map.Map NodeId (Map.Map NodeId ())
nodeToScc DGraph{..} = Map.fromList pairs
  where
    vertices = [ (nId n, nId n, succs (nId n)) | n <- Map.elems dgNodes ]
    succs v  = [ d | (s, _, d, _) <- dgEdges, s == v ]
    comps    = stronglyConnComp vertices

    pairs = concatMap toPairs comps
    toPairs (AcyclicSCC v) = [(v, Map.singleton v ())]
    toPairs (CyclicSCC vs) =
      let set = Map.fromList (map (,()) vs)
      in  [ (v, set) | v <- vs ]
