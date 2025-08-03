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
import qualified Data.Text              as T
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map

-- | Entry point: recebe o grafo e produz o texto TALM
generateTALM :: DGraph DNode -> TL.Text
generateTALM DGraph{dgNodes, dgEdges} =
  TB.toLazyText $ mconcat (map genNode nodes ++ [genApply dgEdges])
  where
    -- ordena por ID para saída estável
    nodes = sortOn nId (Map.elems dgNodes)

-- | Detecta placeholders do tipo "<…>"
isAngle :: T.Text -> Bool
isAngle t = T.isPrefixOf "<" t && T.isSuffixOf ">" t

-- | Gera instrução TALM para um nó
genNode :: DNode -> TB.Builder
genNode = \case

  InstConst nid lit ->
    TB.fromString $ "const c" ++ show nid ++ " " ++ genLit lit ++ "\n"

  InstPar nid name _ins _nout ->
    TB.fromString $ "param p" ++ show nid ++ " " ++ T.unpack name ++ "\n"

  InstSuper nid name ins nout
    -- filtra forwarders e supers internas (nil, cons literal e isNil)
    | isAngle name
      || T.unpack name `elem` ["nil", ":", "isNil"]
      -> mempty
    | otherwise ->
      let insText = concatMap (\i -> " " ++ show i ++ ":out0") ins
      in TB.fromString $
           "super " ++ T.unpack name
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
      ++ concatMap (\i -> " " ++ show i ++ ":out0") fields
      ++ "\n"

  InstProj nid idx srcNode ->
    TB.fromString $
      "proj p" ++ show nid
      ++ " " ++ show idx
      ++ " " ++ show srcNode ++ ":out0\n"

  InstSteer nid srcNode ->
    TB.fromString $
      "steer d" ++ show nid
      ++ " " ++ show srcNode ++ ":out0\n"

  InstIncTag nid srcNode ->
    TB.fromString $
      "inctag i" ++ show nid
      ++ " "    ++ show srcNode ++ ":out0\n"

-- | Emite as linhas de apply para cada aresta
genApply :: [Edge] -> TB.Builder
genApply edges = mconcat
  [ TB.fromString $
      "apply "
      ++ show sId ++ ":" ++ sP
      ++ " -> "
      ++ show dId ++ ":" ++ dP
      ++ "\n"
  | (sId, sP, dId, dP) <- edges
  ]

-- ======================
-- Helpers de conversão
-- ======================

-- | Converte Literal em string TALM
genLit :: Literal -> String
genLit = \case
  LInt i    -> show i
  LFloat f  -> show f
  LBool b   -> if b then "true" else "false"
  LChar c   -> '\'' : c : "'"
  LString s -> '"' : T.unpack s ++ "'"
  LUnit     -> "()"

-- | Converte BinOp em mnemonic TALM
genOp :: BinOp -> String
genOp = \case
  BAdd  -> "add"
  BSub  -> "sub"
  BMul  -> "mul"
  BDiv  -> "div"
  BMod  -> "mod"
  BAnd  -> "and"
  BOr   -> "or"
  BCons -> "cons"
  BLt   -> "lt"
  BGt   -> "gt"
  BLe   -> "le"
  BGe   -> "ge"
  BEq   -> "eq"
  BNe   -> "neq"
  BXor  -> "xor"

-- | Converte UnaryOp em mnemonic TALM
genUn :: UnaryOp -> String
genUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"
