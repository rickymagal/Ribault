{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Synthesis.GraphViz
-- Description : Render a 'DGraph' of 'DNode's to GraphViz DOT.
-- Maintainer  : ricardofilhoschool@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pretty-prints a dataflow graph to DOT (GraphViz). Nodes are rendered as
-- rounded boxes with short mnemonics, and edges are labeled with
-- @(sourcePort,targetPort)@. Edges are sorted primarily by destination id,
-- then by destination port number, which tends to produce stable layouts.

module Synthesis.GraphViz (toDot) where

import qualified Data.Map       as M
import           Data.Char      (isDigit)
import           Data.List      (sortOn)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Data.Text      (Text)

import           Types          (DGraph(..), NodeId)
import           Node           (DNode(..))

-- | Convert a graph of 'DNode's to a DOT 'TL.Text'.
toDot :: DGraph DNode -> TL.Text
toDot g =
  TL.fromStrict . T.unlines $
    [ "digraph G {"
    , "  rankdir=LR;"
    , "  node [shape=box, style=rounded, fontsize=12];"
    ]
    ++ map (ppNode g) (sortedNodes g)
    ++ map ppEdge        (sortedEdges g)
    ++ [ "}" ]

-- | Nodes sorted by their 'NodeId' for reproducible output.
sortedNodes :: DGraph a -> [(NodeId, a)]
sortedNodes g = sortOn fst (M.toList (dgNodes g))

-- | Edges sorted by (destId, destPortNumeric, srcId) to stabilize the layout.
sortedEdges :: DGraph n -> [(NodeId, Text, NodeId, Text)]
sortedEdges g =
  let es = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
  in sortOn (\(s,_sp,d,dp) -> (d, toInt dp, s)) es

-- | Parse a positive integer 'Text' (fallback 0).
toInt :: Text -> Int
toInt t | T.all isDigit t && not (T.null t) = read (T.unpack t)
toInt _ = 0

-- | Render a single node with its mnemonic label.
ppNode :: DGraph DNode -> (NodeId, DNode) -> Text
ppNode _ (nid, dn) =
  T.concat [ "  n", T.pack (show nid), " [label=\"", opSymbol dn, "\"];" ]

-- | Mnemonic for each 'DNode' variant.
opSymbol :: DNode -> Text
opSymbol = \case
  -- Constants
  NConstI{..}   -> T.pack ("const "  ++ show cInt)
  NConstF{..}   -> T.pack ("fconst " ++ show cFloat)
  NConstD{..}   -> T.pack ("dconst " ++ show cDouble)

  -- Integer
  NAdd{}        -> "+"
  NSub{}        -> "-"
  NMul{}        -> "*"
  NDiv{}        -> "/"

  -- Float
  NFAdd{}       -> "f+"
  NFSub{}       -> "f-"
  NFMul{}       -> "f*"
  NFDiv{}       -> "f/"

  -- Double (add only for now)
  NDAdd{}       -> "d+"

  -- Bool/bitwise and control
  NBand{}       -> "band"
  NSteer{}      -> "steer"
  NLThan{}      -> "<"
  NGThan{}      -> ">"
  NEqual{}      -> "=="

  -- Immediates
  NLThanI{..}   -> T.pack ("<i "  ++ show iImm)
  NGThanI{..}   -> T.pack (">i "  ++ show iImm)
  NAddI{..}     -> T.pack ("+i "  ++ show iImm)
  NSubI{..}     -> T.pack ("-i "  ++ show iImm)
  NMulI{..}     -> T.pack ("*i "  ++ show iImm)
  NFMulI{..}    -> T.pack ("*fi " ++ show fImm)
  NDivI{..}     -> T.pack ("/i "  ++ show iImm)

  -- Calls
  NCallGroup{..}-> T.pack ("callgroup " <> nName)
  NCallSnd{..}  -> T.pack ("callsnd "   <> nName)
  NRetSnd{..}   -> T.pack ("retsnd "    <> nName)
  NRet{..}      -> T.pack ("ret "       <> nName)

  -- Other
  NTagVal{}     -> "tagval"
  NValTag{}     -> "valtag"
  NIncTag{}     -> "inctag"
  NIncTagI{..}  -> T.pack ("inctagi " ++ show iImm)
  NCpHToDev{}   -> "cphtodev"
  NCpDevToH{}   -> "cpdevtoh"
  NCommit{}     -> "commit"
  NStopSpec{}   -> "stopspec"
  NArg{..}      -> T.pack ("arg "       <> nName)
  NSuper{}      -> "super"

-- | Render a directed edge with a small @(\"srcPort\",\"dstPort\")@ label.
ppEdge :: (NodeId, Text, NodeId, Text) -> Text
ppEdge (s, sp, d, dp) =
  T.concat
    [ "  n", T.pack (show s), " -> n", T.pack (show d)
    , " [label=\"(", if T.null sp then "0" else T.toUpper sp, ",", dp, ")\", fontsize=10];"
    ]
