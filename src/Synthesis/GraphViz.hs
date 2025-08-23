{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Synthesis.GraphViz (toDot) where

import qualified Data.Map       as M
import           Data.Char      (isDigit)
import           Data.List      (sortOn)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           Data.Text      (Text)

import           Types          (DGraph(..), NodeId)
import           Node           (DNode(..))

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
  T.concat [ "  n", T.pack (show nid), " [label=\"", opSymbol dn, "\"];" ]

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
  NDAdd{}       -> "d+"
  NBand{}       -> "band"
  NSteer{}      -> "steer"
  NLThan{}      -> "<"
  NGThan{}      -> ">"
  NEqual{}      -> "=="
  NLThanI{..}   -> T.pack ("<i " ++ show iImm)
  NGThanI{..}   -> T.pack (">i " ++ show iImm)
  NAddI{..}     -> T.pack ("+i " ++ show iImm)
  NSubI{..}     -> T.pack ("-i " ++ show iImm)
  NMulI{..}     -> T.pack ("*i " ++ show iImm)
  NFMulI{..}    -> T.pack ("*fi "++ show fImm)
  NDivI{..}     -> T.pack ("/i " ++ show iImm)
  NCallGroup{..}-> T.pack ("callgroup " <> nName)
  NCallSnd{..}  -> T.pack ("callsnd "   <> nName)
  NRetSnd{..}   -> T.pack ("retsnd "    <> nName)
  NRet{..}      -> T.pack ("ret "       <> nName)
  NTagVal{}     -> "tagval"
  NValTag{}     -> "valtag"
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
