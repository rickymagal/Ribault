{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Synthesis.Codegen (assemble) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as Map
import           Types
import           Node
import           Data.Text (Text)
import           Data.Graph (topSort, buildG)
import           Data.Maybe (fromMaybe)

assemble :: DGraph DNode -> T.Text
assemble g =
  let
    nodes = Map.toAscList (dgNodes g)
    idNodeMap = Map.fromList nodes
    edges :: [(NodeId, [NodeId])]
    edges = [ (nId, nodeDeps node) | (nId, node) <- nodes ]
    nodeIds = map fst nodes
    nodeIndexMap = Map.fromList $ zip nodeIds [0..]
    idxNodeMap = Map.fromList $ zip [0..] nodeIds
    getIdx nid = fromMaybe (error $ "Node id not found: " ++ show nid) (Map.lookup nid nodeIndexMap)
    getNid idx = fromMaybe (error $ "Node idx not found: " ++ show idx) (Map.lookup idx idxNodeMap)
    graphEdges = [ (getIdx n, map getIdx ds) | (n, ds) <- edges ]
    numNodes = length nodes
    gr = buildG (0, numNodes - 1) [ (i, d) | (i, ds) <- graphEdges, d <- ds ]
    order = map getNid $ reverse $ topSort gr
    orderedNodes = map (\nid -> (nid, idNodeMap Map.! nid)) order
  in TL.toStrict . TB.toLazyText $ mconcat (map (emitNode . snd) orderedNodes)

nodeDeps :: DNode -> [NodeId]
nodeDeps = \case
  InstConst{} -> []
  InstBinop{lhs, rhs} -> [lhs, rhs]
  InstBinopI{lhs} -> [lhs]
  InstUnary{arg} -> [arg]
  InstSteer{predN} -> [predN]
  InstIncTag{base} -> [base]
  InstTuple{fields} -> fields
  InstProj{tuple} -> [tuple]
  InstSuper{ins} -> ins
  InstPar{} -> []

emitNode :: DNode -> TB.Builder
emitNode n = case n of
  -- Constante: inteiro, float, char (ascii), bool (0/1), string (lista de ints ascii), unit
  InstConst{nId, lit} -> TB.fromText $ "const " <> cgNodeName nId <> ", " <> showLit lit <> "\n"

  -- Binários e unários
  InstBinop{nId, op, lhs, rhs} -> TB.fromText $
    showBin op <> " " <> cgNodeName nId <> ", " <> cgNodeName lhs <> ", " <> cgNodeName rhs <> "\n"
  InstBinopI{nId, opI, lhs, imm} -> TB.fromText $
    showBinI opI <> " " <> cgNodeName nId <> ", " <> cgNodeName lhs <> ", " <> T.pack (show imm) <> "\n"
  InstUnary{nId, unop, arg} -> TB.fromText $
    showUn unop <> " " <> cgNodeName nId <> ", " <> cgNodeName arg <> "\n"
  InstSteer{nId, predN} -> TB.fromText $
    "steer " <> cgNodeName nId <> ", " <> cgNodeName predN <> "\n"

  -- IGNORE os nós de lista, wrappers e infra-estrutura AST!
  InstSuper{nId, name, ins, outs}
    | name `elem`
        [ "intVal", "charVal", "floatVal", "falseBool", "trueBool"
        , "stringVal", "unit", "tupleVal", "listVal"
        , ":" -- cons
        , "cons", "isNil", "head", "tail"
        ]
    -> mempty
    | "λ#" `T.isPrefixOf` name || "<local:" `T.isPrefixOf` name || "<fwd:" `T.isPrefixOf` name
    -> mempty
    | name == "<apply>" || otherwise
    -> lowerCall n

  InstTuple{} -> mempty
  InstProj{} -> mempty
  InstIncTag{} -> mempty
  InstPar{} -> mempty

-- Nome do nó no código TALM
cgNodeName :: NodeId -> Text
cgNodeName i = "flowInst" <> T.pack (show i)

showLit :: Literal -> Text
showLit (LInt i)    = T.pack (show i)
showLit (LFloat d)  = T.pack (show d)
showLit (LBool b)   = if b then "1" else "0"
showLit (LChar c)   = T.pack (show $ fromEnum c)
showLit (LString s) = T.intercalate ", " (map (T.pack . show . fromEnum) $ T.unpack s)
showLit LUnit       = "()"

showBin :: BinOp -> Text
showBin = \case
  BAdd -> "add"
  BSub -> "sub"
  BMul -> "mul"
  BDiv -> "div"
  BMod -> "mod"
  BAnd -> "band"
  BOr  -> "bor"
  BXor -> "bxor"
  BLt  -> "lthan"
  BGt  -> "gthan"
  BLe  -> "leq"
  BGe  -> "geq"
  BEq  -> "equal"
  BNe  -> "nequal"
  BCons -> "cons"

showBinI :: BinOp -> Text
showBinI = \case
  BAdd -> "addi"
  BSub -> "subi"
  BMul -> "muli"
  BDiv -> "divi"
  BMod -> "modi"
  op   -> showBin op <> "I"

showUn :: UnaryOp -> Text
showUn = \case
  UNeg   -> "neg"
  UNot   -> "not"
  UIsNil -> "isNil"

-- Função para gerar callgroup/callsnd/retsnd (padrão geral)
lowerCall :: DNode -> TB.Builder
lowerCall InstSuper{nId, name, ins, outs} =
  let groupName = name <> T.pack (show nId)
      -- callgroup sempre gerado
      callgroupLine = TB.fromText $ "callgroup('" <> groupName <> "', '" <> name <> "')\n"
      -- callsnds: cada argumento vira callsnd name[i]
      callsnds = [ TB.fromText $ "callsnd " <> name <> "[" <> T.pack (show i) <> "], " <> cgNodeName aid <> ", " <> groupName <> "\n"
                 | (i, aid) <- zip [1..] ins
                 ]
      -- retsnd: sempre retorna para o primeiro argumento (padrão, pode ajustar)
      retsndLine = case ins of
        (x:_) -> TB.fromText $ "retsnd " <> name <> "[0], " <> cgNodeName x <> ", " <> groupName <> "\n"
        []    -> mempty
  in mconcat (callgroupLine : callsnds ++ [retsndLine])
