{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

-- | Codegen: DFG → TALM code
module Synthesis.Codegen (generateTALM) where

import           Types                  (DGraph(..), Edge)
import           Node                   (DNode(..), nId, Literal(..), BinOp(..), UnaryOp(..))
import           Port                   (Port(..))
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy         as TL
import           Data.Text              (unpack)
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map

-- | Generate TALM assembly text from a dataflow graph
generateTALM :: DGraph DNode -> TL.Text
generateTALM DGraph{dgNodes, dgEdges} =
  TB.toLazyText $ mconcat (map genNode nodes ++ [genApply dgEdges])
  where
    nodes = sortOn nId (Map.elems dgNodes)

-- | Emit TALM instruction for each node
genNode :: DNode -> TB.Builder
genNode = \case
  InstConst nid lit ->
    TB.fromString $ "const c" ++ show nid ++ " " ++ genLit lit ++ "\n"

  InstPar nid name ins nout ->
    TB.fromString $ "param p" ++ show nid ++ " " ++ unpack name ++ "\n"

  InstSuper nid name ins nout ->
    let insText = concatMap (\nid -> " " ++ show nid ++ ":out0") ins
    in TB.fromString $
         "super " ++ unpack name
      ++ " s" ++ show nid
      ++ " "  ++ show nout
      ++ insText ++ "\n"

  InstBinop nid op lhs rhs ->
    TB.fromString $
      genOp op ++ " b" ++ show nid
      ++ " " ++ show lhs ++ ":out0"
      ++ " " ++ show rhs ++ ":out0\n"

  InstUnary nid u arg ->
    TB.fromString $
      genUn u ++ " u" ++ show nid
      ++ " " ++ show arg ++ ":out0\n"

  InstTuple nid fields ->
    TB.fromString $
      "tuple t" ++ show nid
      ++ concatMap (\nid -> " " ++ show nid ++ ":out0") fields
      ++ "\n"

  InstProj nid idx srcNode ->
    TB.fromString $
      "proj p" ++ show nid
      ++ " " ++ show idx
      ++ " "  ++ show srcNode ++ ":out0\n"

  InstSteer nid srcNode ->
    TB.fromString $
      "steer d" ++ show nid
      ++ " " ++ show srcNode ++ ":out0\n"

  InstIncTag nid srcNode ->
    TB.fromString $
      "inctag i" ++ show nid
      ++ " "    ++ show srcNode ++ ":out0\n"

-- | Emit apply instructions for each edge in the graph
genApply :: [Edge] -> TB.Builder
genApply edges = mconcat
  [ TB.fromString $
      "apply "
      ++ show srcId ++ ":" ++ srcP
      ++ " -> "
      ++ show dstId ++ ":" ++ dstP
      ++ "\n"
  | (srcId, srcP, dstId, dstP) <- edges
  ]

-- Helpers -------------------------------------------------------------

-- Convert a Port to "nodeId:portName" string
portStr :: Port -> String
portStr (InstPort n p) = show n ++ ":" ++ p

-- Convert Literal to its TALM string representation
genLit :: Literal -> String
genLit = \case
  LInt i    -> show i
  LFloat f  -> show f
  LBool b   -> if b then "true" else "false"
  LChar c   -> '\'' : c : "'"
  LString s -> '"' : unpack s ++ "'"
  LUnit     -> "()"

-- Convert BinOp to its TALM mnemonic
genOp :: BinOp -> String
genOp = \case
  BAdd  -> "add"; BSub -> "sub"; BMul -> "mul"; BDiv -> "div"; BMod -> "mod"
  BAnd  -> "and"; BOr  -> "or"; BCons-> "cons"
  BLt   -> "lt" ; BGt  -> "gt" ; BLe  -> "le" ; BGe -> "ge"
  BEq   -> "eq" ; BNe  -> "neq"; BXor-> "xor"

-- Convert UnaryOp to its TALM mnemonic
genUn :: UnaryOp -> String
genUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"
