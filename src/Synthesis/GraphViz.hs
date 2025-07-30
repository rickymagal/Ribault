{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Synthesis.GraphViz (render) where

import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as B

import           Synthesis.Instruction

-- ════════════════════════════════════════════════
-- Render
-- ════════════════════════════════════════════════
render :: [Inst] -> TL.Text
render allInsts =
  let called = S.fromList [ f | InstCallGrp{ cgFun = f } <- allInsts ]
      insts  = filter (keepReturn called) allInsts        -- remove “return res3”
      idx    = callGrpIndex insts
      edges  = S.fromList
                 [ e | i <- insts, e <- normEdges i ++ extraEdges idx i ]
  in B.toLazyText $
         header
      <> mconcat (map nodeLine insts)
      <> mconcat (map edgeLine $ S.toList edges)
      <> footer

keepReturn :: S.Set String -> Inst -> Bool
keepReturn funs InstReturn{ funName = f } = f `S.member` funs
keepReturn _    _                         = True

-- ═══════════════════════════════════════
-- DOT boilerplate
-- ═══════════════════════════════════════
header, footer :: B.Builder
header = "digraph G {\n  node [shape=box, style=rounded];\n"
footer = "}\n"

-- ═══════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════
nodeName :: NodeId -> String
nodeName (NodeId n) | n < 0     = 'd' : show (abs n)
                    | otherwise = 'n' : show n

escape :: TL.Text -> B.Builder
escape = B.fromLazyText . TL.replace "\"" "\\\""

type Edge = (NodeId, NodeId)

-- ═══════════════════════════════════════
-- Nodes
-- ═══════════════════════════════════════
nodeLine :: Inst -> B.Builder
nodeLine inst =
  case inst of
    InstSteer{} ->
      "  " <> nid <> " [shape=triangle, label=\"\"];\n"
    _ ->
      "  " <> nid <> " [label=\"" <> lbl <> "\"];\n"
  where
    nid = B.fromString (nodeName (nodeId inst))
    lbl = escape (labelFor inst)

labelFor :: Inst -> TL.Text
labelFor InstConst{..}
  | litType == "float" = "fconst #" <> TL.pack (show litVal)
  | otherwise          = "const #"  <> TL.pack (show litVal)
labelFor InstBinop {..}   = TL.pack binOp
labelFor InstBinopI{..}   = TL.pack binOp <> "i"
labelFor InstIncTag{}     = "inctag"
labelFor InstSuper{..}    = "super" <> TL.pack (show superNum)
labelFor InstPar{..}      = "par"   <> TL.pack (show parNum)
labelFor InstReturn{..}   = "return " <> TL.pack funName
labelFor InstCallGrp{..}  = "callgrp(" <> TL.pack cgFun <> ")"
labelFor InstCallSnd{}    = "callsnd"
labelFor InstRetSnd{}     = "retsnd"
labelFor _                = "?"

-- ═══════════════════════════════════════
-- Edges
-- ═══════════════════════════════════════
normEdges :: Inst -> [Edge]
normEdges inst = concatMap mk (inputs inst)
  where
    dst = nodeId inst
    ok  (NodeId k) = k >= 0
    mk SigInstPort {sigNode = src} | ok src = [(src,dst)]
    mk SigSteerPort{sigNode = src} | ok src = [(src,dst)]
    mk _ = []

inputs :: Inst -> [Signal]
inputs InstConst{}        = []
inputs InstBinop{..}      = leftSrc ++ rightSrc
inputs InstBinopI{..}     = uniSrc
inputs InstSteer{..}      = steerExpr ++ steerInp
inputs InstIncTag{..}     = tagInp
inputs InstSuper{..}      = concat superInp
inputs InstPar{..}        = concat parInp
inputs InstReturn{..}     = retExpr
inputs InstCallSnd{..}    = csOper
inputs InstRetSnd{..}     = rsOper
inputs _                  = []

-- arestas extra p/ ligações da chamada
extraEdges :: M.Map (String,String) NodeId -> Inst -> [Edge]
extraEdges idx InstCallSnd{ nodeId = s, csFun = f, csGroup = g } =
  maybe [] (\grp -> [(s,grp)]) (M.lookup (f,g) idx)
extraEdges idx InstRetSnd{ nodeId = r, rsFun = f, rsGroup = g } =
  maybe [] (\grp -> [(grp,r)]) (M.lookup (f,g) idx)
extraEdges _ _ = []

callGrpIndex :: [Inst] -> M.Map (String,String) NodeId
callGrpIndex = M.fromList
  . map (\InstCallGrp{cgFun = f, cgName = g, nodeId = n} -> ((f,g),n))
  . filter (\x -> case x of InstCallGrp{} -> True; _ -> False)

edgeLine :: Edge -> B.Builder
edgeLine (src,dst) =
  "  " <> B.fromString (nodeName src)
       <> " -> "
       <> B.fromString (nodeName dst)
       <> ";\n"
