{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Synthesis.GraphViz
  ( toDot  -- :: DGraph DNode -> LazyText
  ) where

import qualified Data.Map       as M
import           Data.Char      (isDigit)
import           Data.List      (sortOn)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Data.Text      (Text)

import           Types          (DGraph(..), NodeId)
import           Node           (DNode(..), nodeName)

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
  T.concat
    [ "  ", nodeId nid, " [label=\""
    , opSymbol dn
    , "\"];"
    ]

nodeId :: NodeId -> Text
nodeId n = T.pack ("n" ++ show n)

opSymbol :: DNode -> Text
opSymbol = \case
  NConstI{}   -> "const"
  NConstF{}   -> "fconst"
  NConstD{}   -> "dconst"
  NAdd{}      -> "+"
  NSub{}      -> "-"
  NMul{}      -> "*"
  NDiv{}      -> "/"
  NFAdd{}     -> "f+"
  NDAdd{}     -> "d+"
  NBand{}     -> "band"
  NSteer{}    -> "steer"
  NLThan{}    -> "<"
  NGThan{}    -> ">"
  NEqual{}    -> "=="
  NLThanI{}   -> "<i"
  NGThanI{}   -> ">i"
  NAddI{}     -> "+i"
  NSubI{}     -> "-i"
  NMulI{}     -> "*i"
  NFMulI{}    -> "*fi"
  NDivI{}     -> "/i"
  NCallGroup{}-> "callgroup"
  NCallSnd{}  -> "callsnd"
  NRetSnd{}   -> "retsnd"
  NRet{}      -> "ret"
  NTagVal{}   -> "tagval"
  NValTag{}   -> "valtag"
  NCpHToDev{} -> "cphtodev"
  NCpDevToH{} -> "cpdevtoh"
  NCommit{}   -> "commit"
  NStopSpec{} -> "stopspec"
  NSuper{}    -> "super"

ppEdge :: (NodeId, Text, NodeId, Text) -> Text
ppEdge (s, sp, d, dp) =
  T.concat
    [ "  ", nodeId s, " -> ", nodeId d
    , " [label=\"(", tPort sp, ",", dp, ")\", fontsize=10];"
    ]

tPort :: Text -> Text
tPort p | T.null p  = "0"
        | otherwise = T.toUpper p
