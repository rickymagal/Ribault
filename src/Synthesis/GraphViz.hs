-- src/Synthesis/GraphViz.hs
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Synthesis.GraphViz
  ( toDot                       -- :: DGraph DNode -> Text
  ) where

import           Types                     (DGraph (..), Edge)
import           Node                      (DNode (..), nodeName)
import qualified Data.Map.Strict           as Map
import qualified Data.Text.Lazy            as TL

-- | Convert a data-flow graph to DOT.
toDot :: DGraph DNode -> TL.Text
toDot DGraph{dgNodes = nodes, dgEdges = edges} =
  TL.unlines . fmap TL.pack $
       [ "digraph DF {"
       , "  rankdir=LR;"
       , "  node [shape=box,fontname=\"monospace\"];"
       ]
    ++ map renderNode (Map.toList nodes)
    ++ map renderEdge edges
    ++ [ "}" ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

renderNode :: (Int, DNode) -> String
renderNode (nid, n) =
  "  " ++ show nid ++ " [label=\"" ++ label n ++ "\"];"
  where
    label :: DNode -> String
    label NConstI{cInt}    = "const "  ++ show cInt
    label NConstF{cFloat}  = "fconst " ++ show cFloat
    label NConstD{cDouble} = "dconst " ++ show cDouble
    label other            = nodeName other

renderEdge :: Edge -> String
renderEdge (src, sPort, dst, dPort) =
  "  " ++ show src ++ ":" ++ sPort ++ " -> " ++ show dst ++ ":" ++ dPort ++ ";"
