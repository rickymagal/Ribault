{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Converte a lista linear de 'Inst' (IR) em texto DOT.
--   Robusto: nunca faz pattern-match incompleto nem usa 'error'.
--
--   Exemplos:
--
--   > let dot = Synthesis.GraphViz.render progIR
--   > Data.Text.Lazy.IO.writeFile "g.dot" dot
--   > dot -Tpng g.dot -o g.png
--
module Synthesis.GraphViz (render) where

import qualified Data.Set                 as S
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as B

import           Synthesis.Instruction

-- ════════════════════════════════════════════════════════════════════
-- API
-- ════════════════════════════════════════════════════════════════════

render :: [Inst] -> TL.Text
render insts =
    B.toLazyText $
         header
      <> mconcat (map nodeLine insts)
      <> mconcat (map edgeLine . S.toList $ allEdges)
      <> footer
  where
    allEdges = S.fromList (concatMap mkEdges insts)

-- ════════════════════════════════════════════════════════════════════
-- DOT boilerplate
-- ════════════════════════════════════════════════════════════════════

header, footer :: B.Builder
header = "digraph G {\n  node [shape=box, style=rounded];\n"
footer = "}\n"

-- ════════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════════

nodeName :: NodeId -> String
nodeName (NodeId n)
  | n < 0     = 'd' : show (abs n)   -- dummy (parâmetro formal)
  | otherwise = 'n' : show n

escape :: TL.Text -> B.Builder
escape = B.fromLazyText . TL.replace "\"" "\\\""

-- ════════════════════════════════════════════════════════════════════
-- Nodes
-- ════════════════════════════════════════════════════════════════════

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

-- ════════════════════════════════════════════════════════════════════
-- Edges
-- ════════════════════════════════════════════════════════════════════

type Edge = (NodeId, NodeId, Maybe String)   -- src dst label

mkEdges :: Inst -> [Edge]
mkEdges inst = concatMap toEdge (inputs inst)
  where
    dst = nodeId inst
    valid (NodeId k) = k >= 0

    toEdge :: Signal -> [Edge]
    toEdge SigInstPort{sigNode = n} | valid n = [(n, dst, Nothing)]
    toEdge SigSteerPort{sigNode = n, sigBranch = br}
                       | valid n = [(n, dst, Just (showBranch br))]
    toEdge _ = []   -- ReturnPort ou futuro signal → ignora

    showBranch T = "T"
    showBranch F = "F"

-- | Coleta todas as fontes de dados de uma instrução.
inputs :: Inst -> [Signal]
inputs InstConst{}        = []
inputs InstBinop{..}      = leftSrc  ++ rightSrc
inputs InstBinopI{..}     = uniSrc
inputs InstSteer{..}      = steerExpr ++ steerInp
inputs InstIncTag{..}     = tagInp
inputs InstSuper{..}      = concat superInp
inputs InstPar{..}        = concat parInp
inputs InstReturn{..}     = retExpr
inputs InstCallSnd{..}    = csOper
inputs InstRetSnd{..}     = rsOper
inputs _                  = []

edgeLine :: Edge -> B.Builder
edgeLine (src, dst, mLab) =
  "  " <> B.fromString (nodeName src)
  <> " -> " <> B.fromString (nodeName dst)
  <> case mLab of
       Nothing -> ""
       Just t  -> " [label=\"" <> B.fromString t <> "\"]"
  <> ";\n"
