{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Synthesis.GraphViz (render) where

import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as B

import           Synthesis.DFG
import qualified Synthesis.Instruction  as IR
import           Synthesis.Instruction  (Inst(..), NodeId(..), Signal(..))

-- ──────────────────────────────────  DOT boilerplate  ─────────────────────────
header, footer :: B.Builder
header = "digraph G {\n  node [shape=box, style=rounded];\n"
footer = "}\n"

-- ─────────────────────────────────────  helpers  ──────────────────────────────
nodeName :: NodeId -> String
nodeName (NodeId n) | n < 0     = 'd' : show (abs n)   -- dummy (arg)
                    | otherwise = 'n' : show n

type Edge = (NodeId, NodeId, Maybe String)             -- src, dst, rótulo

edgeEnds :: S.Set Edge -> S.Set NodeId
edgeEnds = S.foldr (\(a,b,_) s -> S.insert a (S.insert b s)) S.empty

escape :: TL.Text -> B.Builder
escape = B.fromLazyText . TL.replace "\"" "\\\""

-- ───────────────────────────────────────  API  ───────────────────────────────
render :: DFG -> TL.Text
render DFG{..} =
  let allNodes = M.elems dNodes

      -- remover returns “soltos” (não chamados)
      calledFuns = S.fromList [ cgFun | Node{nInst=InstCallGrp{..}} <- allNodes ]
      nodes      = filter (keepReturn calledFuns) allNodes

      edges      = S.fromList (concatMap instEdges nodes ++ callGrpEdges nodes)

      realIds    = S.fromList (map nId nodes)
      argIds     = S.filter (\nid@(NodeId n) -> n < 0 && nid `S.notMember` realIds)
                            (edgeEnds edges)
      argMap     = M.fromList (zip (S.toAscList argIds) [1..])

  in B.toLazyText
       $  header
       <> mconcat (map nodeLine nodes)
       <> mconcat (map (argNodeLine argMap) (S.toAscList argIds))
       <> mconcat (map edgeLine (S.toList edges))
       <> footer

keepReturn :: S.Set String -> Node -> Bool
keepReturn funs Node{nInst=InstReturn{..}} = funName `S.member` funs
keepReturn _     _                         = True

-- ───────────────────────────────────────  nós  ───────────────────────────────
nodeLine :: Node -> B.Builder
nodeLine Node{..} =
  case nInst of
    InstSteer{} ->
      "  " <> nid <> " [shape=triangle, label=\"T F\", fontsize=10];\n"
    _ ->
      "  " <> nid <> " [label=\"" <> lbl <> "\"];\n"
  where
    nid = B.fromString (nodeName nId)
    lbl = escape (labelFor nInst)

argNodeLine :: M.Map NodeId Int -> NodeId -> B.Builder
argNodeLine amap nid =
  case M.lookup nid amap of
    Nothing -> mempty
    Just k  -> "  " <> B.fromString (nodeName nid)
             <> " [label=\"arg" <> B.fromString (show k) <> "\"];\n"

-- ───────────────────────────────────  edges normais  ──────────────────────────
instEdges :: Node -> [Edge]
instEdges Node{nInst=inst} =
  let dst = IR.nodeId inst
      mk sig = case sig of
        SigInstPort  {sigNode=src}             -> Just (src,dst,Nothing)
        SigSteerPort {sigNode=src,sigBranch=b} -> Just (src,dst,Nothing)
        _                                      -> Nothing   -- ignora ReturnPort
  in catMaybes (map mk (IR.inputs inst))
  where
    catMaybes = foldr (\m acc -> maybe acc (:acc) m) []

-- ────────────────────────  edges extra (callgrp/callsnd/retsnd)  ─────────────
callGrpEdges :: [Node] -> [Edge]
callGrpEdges ns =
  let idx = M.fromList [ ((cgFun,cgName),nId)
                       | Node{nInst=InstCallGrp{..},..} <- ns ]

      fromS Node{nId=src,nInst=InstCallSnd{..}} =
        maybe [] (\dst -> [(src,dst,Nothing)])
                 (M.lookup (csFun,csGroup) idx)
      fromS _ = []

      fromR Node{nId=dst,nInst=InstRetSnd{..}} =
        maybe [] (\src -> [(src,dst,Nothing)])
                 (M.lookup (rsFun,rsGroup) idx)
      fromR _ = []

  in concatMap (\n -> fromS n ++ fromR n) ns

-- ────────────────────────────────  edge → DOT  ───────────────────────────────
edgeLine :: Edge -> B.Builder
edgeLine (src,dst,ml) =
  "  " <> B.fromString (nodeName src) <> " -> "
       <> B.fromString (nodeName dst)
       <> maybe ";\n"
                (\l -> " [label=\"" <> B.fromString l <> "\"];\n") ml

-- ─────────────────────────────  rótulo de nó  ────────────────────────────────
labelFor :: Inst -> TL.Text
labelFor InstConst{..}
  | litType=="float" = "fconst #" <> TL.pack (show litVal)
  | otherwise        = "const #"  <> TL.pack (show litVal)
labelFor InstBinop {..}        = TL.pack binOp
labelFor InstBinopI{..}        = TL.pack binOp <> "i"
labelFor InstMkTuple{..}       = "mkTuple" <> TL.pack (show tupArity)
labelFor InstTupleProj{..}     = "π" <> TL.pack (show fieldIx)
labelFor InstIncTag{}          = "inctag"
labelFor InstSuper{..}         = "super" <> TL.pack (show superNum)
labelFor InstPar{..}           = "par"   <> TL.pack (show parNum)
labelFor InstReturn{..}        = "return " <> TL.pack funName
labelFor InstCallGrp{..}       = "callgrp(" <> TL.pack cgFun <> ")"
labelFor InstCallSnd{}         = "callsnd"
labelFor InstRetSnd{}          = "retsnd"
labelFor _                     = "?"
