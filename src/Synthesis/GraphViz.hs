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
-- Pretty-prints a dataflow graph to DOT (GraphViz). Two modes:
--
-- * 'toDot' — full graph with all nodes (infrastructure included).
-- * 'toCleanDot' — semantic-only graph, omitting tag infrastructure
--   and reconnecting edges through removed nodes.

module Synthesis.GraphViz (toDot, toCleanDot) where

import qualified Data.Map       as M
import qualified Data.Set       as S
import           Data.Char      (isDigit)
import           Data.List      (sortOn, nub)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Data.Text      (Text)

import           Types          (DGraph(..), NodeId)
import           Node           (DNode(..))

--------------------------------------------------------------------------------
-- Full graph (unchanged)
--------------------------------------------------------------------------------

-- | Convert a graph of 'DNode's to a DOT 'TL.Text' (full, all nodes).
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

--------------------------------------------------------------------------------
-- Clean graph
--------------------------------------------------------------------------------

-- | Convert a graph to a clean DOT, omitting infrastructure nodes and
-- reconnecting edges through them.
toCleanDot :: DGraph DNode -> TL.Text
toCleanDot g =
  let nodes    = M.toList (dgNodes g)
      edges    = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
      -- Detect zeroing subs: NSub where both inputs come from same source node
      inEdges  = M.fromListWith (++)
                   [ (d, [s]) | (s, _sp, d, _dp) <- edges ]
      zeroSubs = S.fromList
                   [ nid | (nid, NSub{}) <- nodes
                         , let srcs = M.findWithDefault [] nid inEdges
                         , not (null srcs)
                         , S.size (S.fromList srcs) == 1 ]
      -- Nodes feeding exclusively into infrastructure are also infrastructure.
      -- For now, mark by type + zeroing sub detection.
      kept     = S.fromList [ nid | (nid, dn) <- nodes
                                  , not (isInfra dn)
                                  , not (S.member nid zeroSubs) ]
      -- Build forward adjacency for removed nodes
      fwdAdj   = M.fromListWith (++)
                   [ (s, [(sp, d, dp)]) | (s, sp, d, dp) <- edges
                                        , not (S.member s kept) ]
      -- Bypass removed nodes
      cleanEdges = nub $ concatMap (resolveEdge kept fwdAdj) edges
      -- Remove self-loops and duplicates
      finalEdges = nub [ (s, sp, d, dp) | (s, sp, d, dp) <- cleanEdges, s /= d ]
      cleanNodes = sortOn fst [ (nid, dn) | (nid, dn) <- nodes
                                           , S.member nid kept ]
  in TL.fromStrict . T.unlines $
       [ "digraph G {"
       , "  rankdir=TB;"
       , "  node [fontsize=12];"
       ]
       ++ map ppCleanNode cleanNodes
       ++ map ppCleanEdge (sortOn (\(s,_,d,_) -> (d,s)) finalEdges)
       ++ [ "}" ]

-- | Infrastructure nodes to filter out.
isInfra :: DNode -> Bool
isInfra = \case
  NValTag{}       -> True
  NTagVal{}       -> True
  NBand{}         -> True
  NCommit{}       -> True
  NStopSpec{}     -> True
  NIncTag{}       -> True
  NIncTagI{}      -> True
  -- Tag manipulation immediates (small positives are tag offsets)
  NAddI{iImm=k}  -> k >= 0 && k <= 7
  NSubI{iImm=k}  -> k >= 0 && k <= 7
  NMulI{iImm=k}  -> k == 8       -- tag radix
  NDivI{iImm=k}  -> k == 8       -- tag radix
  NEqual{}        -> True         -- case/list dispatch
  NGThan{}        -> True         -- list length guard
  _               -> False

-- | Resolve a single edge: if both endpoints are kept, pass through.
-- If source is kept but dest is removed, BFS through removed nodes.
-- If source is removed, skip (handled by predecessor's BFS).
resolveEdge :: S.Set NodeId
            -> M.Map NodeId [(Text, NodeId, Text)]
            -> (NodeId, Text, NodeId, Text)
            -> [(NodeId, Text, NodeId, Text)]
resolveEdge kept fwdAdj (s, sp, d, dp)
  | not (S.member s kept) = []
  | S.member d kept       = [(s, sp, d, dp)]
  | otherwise             =
      [ (s, sp, target, tp)
      | (target, tp) <- bfsToKept kept fwdAdj d ]

-- | BFS from a removed node, following edges through removed nodes,
-- collecting all (kept node, dest port) endpoints reached.
bfsToKept :: S.Set NodeId
          -> M.Map NodeId [(Text, NodeId, Text)]
          -> NodeId
          -> [(NodeId, Text)]
bfsToKept kept fwdAdj start = go S.empty [start]
  where
    go _       []     = []
    go visited (n:ns)
      | S.member n visited = go visited ns
      | otherwise =
          let visited' = S.insert n visited
              succs    = M.findWithDefault [] n fwdAdj
              (found, more) = foldr classify ([], []) succs
          in found ++ go visited' (more ++ ns)
    classify (_sp, d, dp) (found, more)
      | S.member d kept = ((d, dp) : found, more)
      | otherwise       = (found, d : more)

-- | Render a node with shape based on its type.
ppCleanNode :: (NodeId, DNode) -> Text
ppCleanNode (nid, dn) =
  T.concat [ "  n", T.pack (show nid)
           , " [label=\"", opSymbol dn, "\""
           , ", ", nodeAttrs dn
           , "];" ]

-- | GraphViz attributes (shape, style) per node type.
nodeAttrs :: DNode -> Text
nodeAttrs = \case
  NSteer{}      -> "shape=invtriangle, style=filled, fillcolor=lightyellow"
  NConstI{}     -> "shape=box, style=rounded"
  NConstF{}     -> "shape=box, style=rounded"
  NConstD{}     -> "shape=box, style=rounded"
  NArg{}        -> "shape=box, style=\"rounded,bold\""
  NSuper{}      -> "shape=doubleoctagon, style=filled, fillcolor=lightblue"
  NCallGroup{}  -> "shape=box, style=rounded"
  NCallSnd{}    -> "shape=box, style=rounded"
  NRetSnd{}     -> "shape=box, style=rounded"
  NRet{}        -> "shape=box, style=rounded"
  _             -> "shape=oval"

-- | Render a clean edge — label only steer T/F outputs.
ppCleanEdge :: (NodeId, Text, NodeId, Text) -> Text
ppCleanEdge (s, sp, d, _dp) =
  let lbl | sp == "t" || sp == "T" = " [label=\"T\", fontsize=10]"
          | sp == "f" || sp == "F" = " [label=\"F\", fontsize=10]"
          | otherwise              = ""
  in T.concat [ "  n", T.pack (show s), " -> n", T.pack (show d), lbl, ";" ]

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

sortedNodes :: DGraph a -> [(NodeId, a)]
sortedNodes g = sortOn fst (M.toList (dgNodes g))

sortedEdges :: DGraph n -> [(NodeId, Text, NodeId, Text)]
sortedEdges g =
  let es = [ (s, T.pack sp, d, T.pack dp) | (s,sp,d,dp) <- dgEdges g ]
  in sortOn (\(s,_sp,d,dp) -> (d, toInt dp, s)) es

toInt :: Text -> Int
toInt t | T.all isDigit t && not (T.null t) = read (T.unpack t)
toInt _ = 0

ppNode :: DGraph DNode -> (NodeId, DNode) -> Text
ppNode _ (nid, dn) =
  T.concat [ "  n", T.pack (show nid), " [label=\"", opSymbol dn, "\"];" ]

-- | Mnemonic for each 'DNode' variant.
opSymbol :: DNode -> Text
opSymbol = \case
  NConstI{..}   -> T.pack ("const "  ++ show cInt)
  NConstF{..}   -> T.pack ("fconst " ++ show cFloat)
  NConstD{..}   -> T.pack ("dconst " ++ show cDouble)
  NAdd{}        -> "+"
  NSub{}        -> "-"
  NMul{}        -> "*"
  NDiv{}        -> "/"
  NFAdd{}       -> "f+"
  NFSub{}       -> "f-"
  NFMul{}       -> "f*"
  NFDiv{}       -> "f/"
  NDAdd{}       -> "d+"
  NBand{}       -> "band"
  NSteer{}      -> "steer"
  NLThan{}      -> "<"
  NGThan{}      -> ">"
  NEqual{}      -> "=="
  NLThanI{..}   -> T.pack ("<i "  ++ show iImm)
  NGThanI{..}   -> T.pack (">i "  ++ show iImm)
  NAddI{..}     -> T.pack ("+i "  ++ show iImm)
  NSubI{..}     -> T.pack ("-i "  ++ show iImm)
  NMulI{..}     -> T.pack ("*i "  ++ show iImm)
  NFMulI{..}    -> T.pack ("*fi " ++ show fImm)
  NDivI{..}     -> T.pack ("/i "  ++ show iImm)
  NCallGroup{..}-> T.pack ("callgroup " <> nName)
  NCallSnd{..}  -> T.pack ("callsnd "   <> nName)
  NRetSnd{..}   -> T.pack ("retsnd "    <> nName)
  NRet{..}      -> T.pack ("ret "       <> nName)
  NTagVal{}     -> "tagval"
  NValTag{}     -> "valtag"
  NIncTag{}     -> "IT"
  NIncTagI{..}  -> T.pack ("IT " ++ show iImm)
  NCpHToDev{}   -> "cphtodev"
  NCpDevToH{}   -> "cpdevtoh"
  NCommit{}     -> "commit"
  NStopSpec{}   -> "stopspec"
  NArg{..}      -> T.pack ("arg "       <> nName)
  NSuper{}      -> "super"

ppEdge :: (NodeId, Text, NodeId, Text) -> Text
ppEdge (s, sp, d, dp) =
  T.concat
    [ "  n", T.pack (show s), " -> n", T.pack (show d)
    , " [label=\"(", if T.null sp then "0" else T.toUpper sp, ",", dp, ")\", fontsize=10];"
    ]
