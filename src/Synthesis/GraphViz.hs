{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Synthesis.GraphViz (toDot) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------
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
-- API
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
-- Nós
----------------------------------------------------------------------
showNode :: DNode -> Builder
showNode n@InstSuper{name}
  | "<fwd:" `T.isPrefixOf` name = mempty        -- ignora placeholders
showNode n =
  "  flowInst" <> int (nId n)
  <> " [label=\"" <> TB.fromString (centerLabel n) <> "\"];\n"
 where
  centerLabel = \case
    InstConst{lit=LUnit}      -> "unit"
    InstConst{lit}            -> "const#" <> showLit lit
    InstBinop{op}             -> showBin op
    InstBinopI{opI,imm}       -> showBin opI <> "\\n#" <> show imm
    InstUnary{unop}           -> showUn unop
    InstSteer{}               -> "steer"
    InstIncTag{}              -> "inctag"
    InstTuple{fields}         -> "tuple(" <> show (length fields) <> ")"
    InstProj{idx}             -> "proj#" <> show idx
    InstSuper{name}           -> T.unpack name
    InstPar{name}             -> T.unpack name

----------------------------------------------------------------------
-- Arestas  (sem ‘:porta’)
----------------------------------------------------------------------
showEdge :: Edge -> Builder
showEdge (s,_,d,_) =
  "  flowInst" <> int s <> " -> flowInst" <> int d <> ";\n"

----------------------------------------------------------------------
-- Auxiliares de texto
----------------------------------------------------------------------
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
  BLt  -> "<"   ; BGt  -> ">"   ; BLe  -> "<="  ; BGe  -> ">="
  BEq  -> "=="  ; BNe  -> "!="  ; BCons -> ":"

showUn :: UnaryOp -> String
showUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"

int :: Int -> Builder
int = TB.fromString . show
