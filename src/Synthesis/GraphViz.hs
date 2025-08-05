{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

-- | Pretty-printer: DGraph → texto DOT/Graphviz
module Synthesis.GraphViz (toDot) where

import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (Builder)
import qualified Data.Text.Lazy.Builder    as TB
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as Map

import           Types
import           Node                      ( DNode(..), Literal(..)
                                           , BinOp(..), UnaryOp(..) )

----------------------------------------------------------------------
-- Entrada pública
----------------------------------------------------------------------
toDot :: DGraph DNode -> Text
toDot DGraph{..} =
  TB.toLazyText $
       "digraph G {\n"
    <> "  node [fontname=\"Courier\",shape=box];\n"
    <> mconcat (map showNode (Map.elems dgNodes))
    <> mconcat (map showEdge dgEdges)
    <> "}\n"

----------------------------------------------------------------------
-- Nó individual ------------------------------------------------------
----------------------------------------------------------------------
showNode :: DNode -> Builder
-- ▸ Steer: triângulo com “T  F”
showNode n@InstSteer{} =
  "  flowInst" <> int (nId n)
  <> " [shape=triangle,orientation=180,label=\"T  F\",fontsize=10];\n"

-- ▸ Oculta placeholders <fwd:…>
showNode n@InstSuper{name}
  | "<fwd:" `T.isPrefixOf` name = mempty

-- ▸ DEPRECADO: não mostrar InstPar (parâmetros explícitos)
showNode InstPar{} = mempty

-- ▸ Novos nós TALM
showNode InstCallGroup{nId, groupName} =
  "  flowInst" <> int nId <> " [label=\"callgroup(" <> TB.fromString (T.unpack groupName) <> ")\"];\n"
showNode InstCallSnd{nId, groupName} =
  "  flowInst" <> int nId <> " [label=\"callsnd(" <> TB.fromString (T.unpack groupName) <> ")\"];\n"
showNode InstRetSnd{nId, groupName} =
  "  flowInst" <> int nId <> " [label=\"retsnd(" <> TB.fromString (T.unpack groupName) <> ")\"];\n"
showNode InstRet{nId} =
  "  flowInst" <> int nId <> " [label=\"ret\"];\n"

-- ▸ Todos os demais
showNode n =
  "  flowInst" <> int (nId n)
  <> " [label=\"" <> TB.fromString (label n) <> "\"];\n"
 where
  label = \case
    InstConst{lit=LUnit}      -> "unit"
    InstConst{lit}            -> "const#" <> showLit lit
    InstBinop{op}             -> showBin op
    InstBinopI{opI,imm}       -> showBin opI <> "\\n#" <> show imm
    InstUnary{unop}           -> showUn unop
    InstIncTag{}              -> "inctag"
    InstTuple{fields}         -> "tuple(" <> show (length fields) <> ")"
    InstProj{idx}             -> "proj#" <> show idx
    InstSuper{name}           -> T.unpack name
    _                        -> "?"

----------------------------------------------------------------------
-- Arestas  -----------------------------------------------------------
showEdge :: Edge -> Builder
showEdge (s,_,d,_) =
  "  flowInst" <> int s <> " -> flowInst" <> int d <> ";\n"

----------------------------------------------------------------------
-- Auxiliares de texto -----------------------------------------------
showLit :: Literal -> String
showLit = \case
  LInt i    -> show i
  LFloat d  -> show d
  LBool b   -> show b
  LChar c   -> [c]
  LString s -> T.unpack s
  LUnit     -> "()"

showBin :: BinOp -> String
showBin = \case
  BAdd -> "+"   ; BSub -> "-"   ; BMul -> "*"   ; BDiv -> "/"
  BMod -> "%"   ; BAnd -> "&&"  ; BOr  -> "||"  ; BXor -> "^"
  BLt  -> "<"   ; BGt  -> ">"   ; BLe -> "<="  ; BGe  -> ">="
  BEq  -> "=="  ; BNe  -> "!="  ; BCons -> ":"

showUn :: UnaryOp -> String
showUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"

int :: Int -> Builder
int = TB.fromString . show
