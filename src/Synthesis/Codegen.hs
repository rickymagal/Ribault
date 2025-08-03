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
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map

-- | Entry point: recebe o grafo (após SSA) e produz o texto TALM
generateTALM :: DGraph DNode -> TL.Text
generateTALM DGraph{dgNodes, dgEdges} =
  TB.toLazyText $
    -- primeiro todas as instruções de nó
    mconcat (map genNode nodes)
    -- depois todas as instruções de apply
 <> mconcat (map genApply dgEdges)
  where
    -- para estabilidade, ordena por NodeId
    nodes = sortOn nId (Map.elems dgNodes)

-- | Gera TALM para UM nó
genNode :: DNode -> TB.Builder
genNode = \case

  -- constantes
  InstConst nid lit ->
    TB.fromString $ "const c" ++ show nid ++ " " ++ genLit lit ++ "\n"

  -- parâmetros
  InstPar nid name _ins _nout ->
    TB.fromString $ "param p" ++ show nid ++ " " ++ T.unpack name ++ "\n"

  -- super-instruções: só as da nossa biblioteca
  InstSuper nid name ins nout
    | T.unpack name `elem` ["headList","tailList","isNil","cons"] ->
      TB.fromString $
        "super " ++ T.unpack name
     ++ " s" ++ show nid
     ++ " "  ++ show nout
     ++ concatMap (\i -> " " ++ show i ++ ":out0") ins
     ++ "\n"
    | otherwise ->
      -- tudo o mais (forwarders, funções do usuário, nil, <apply>, ...)
      -- não vira super, e sim será traduzido pelos outros casos ou
      -- simplesmente ignorado aqui (mas suas arestas virão em genApply)
      mempty

  -- operações binárias
  InstBinop nid op lhs rhs ->
    TB.fromString $
      genOp op ++ " b" ++ show nid
    ++ " " ++ show lhs ++ ":out0"
    ++ " " ++ show rhs ++ ":out0\n"

  -- operações unárias
  InstUnary nid u arg ->
    TB.fromString $
      genUn u ++ " u" ++ show nid
    ++ " " ++ show arg ++ ":out0\n"

  -- tupla
  InstTuple nid fields ->
    TB.fromString $
      "tuple t" ++ show nid
    ++ concatMap (\i -> " " ++ show i ++ ":out0") fields
    ++ "\n"

  -- projeção de tupla
  InstProj nid idx srcNode ->
    TB.fromString $
      "proj p" ++ show nid
    ++ " " ++ show idx
    ++ " " ++ show srcNode ++ ":out0\n"

  -- desvio (steer)
  InstSteer nid srcNode ->
    TB.fromString $
      "steer d" ++ show nid
    ++ " " ++ show srcNode ++ ":out0\n"

  -- incremento de tag (SSA)
  InstIncTag nid srcNode ->
    TB.fromString $
      "inctag i" ++ show nid
    ++ " "    ++ show srcNode ++ ":out0\n"

-- | Gera as instruções de apply para cada aresta do grafo
genApply :: Edge -> TB.Builder
genApply (sId,sP,dId,dP) =
  TB.fromString $
    "apply "
 <> show sId ++ ":" ++ sP
 <> " -> "
 <> show dId ++ ":" ++ dP
 <> "\n"

-- ======================
-- Helpers de conversão
-- ======================

genLit :: Literal -> String
genLit = \case
  LInt i    -> show i
  LFloat f  -> show f
  LBool b   -> if b then "true" else "false"
  LChar c   -> '\'' : c : "'"
  LString s -> '"' : T.unpack s ++ "'"
  LUnit     -> "()"

genOp :: BinOp -> String
genOp = \case
  BAdd  -> "add";  BSub -> "sub";  BMul -> "mul"
  BDiv  -> "div";  BMod -> "mod";  BAnd -> "and"
  BOr   -> "or";   BCons-> "cons"
  BLt   -> "lt";   BGt  -> "gt";   BLe  -> "le"
  BGe   -> "ge";   BEq  -> "eq";   BNe  -> "neq"
  BXor  -> "xor"

genUn :: UnaryOp -> String
genUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"
