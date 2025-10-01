{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Synthesis.Codegen (assemble) where

import qualified Data.Map.Strict as M
import           Data.List       (sortOn)
import           Data.Char       (isDigit)
import qualified Data.Text       as T

import           Types           (DGraph(..), NodeId)
import           Node            (DNode(..))
import           Port            (Port(..))

----------------------------------------------------------------
-- Utilitários
----------------------------------------------------------------
showT :: Show a => a -> T.Text
showT = T.pack . show

dstN, dstS :: NodeId -> T.Text
dstN nid = "n" <> showT nid
dstS nid = "s" <> showT nid

-- tag   ----------------------------------------------------------------
-- Força sempre cg NN, com NN ∈ [10..99]; evita “08”
tagName :: NodeId -> T.Text
tagName nid = "cg" <> showT ((nid `mod` 90) + 10)

-- "fun#i" -> "fun[i]"
argAsFunSlot :: String -> T.Text
argAsFunSlot nm =
  case break (=='#') nm of
    (f,'#':ix) | all isDigit ix -> T.pack f <> "[" <> T.pack ix <> "]"
    _                           -> T.pack nm

-- nome do output para usar como fonte
outName :: DGraph DNode -> NodeId -> String -> T.Text
outName g s sp =
  case M.lookup s (dgNodes g) of
    -- steer t -> .w / f -> .g
    Just NSteer{} ->
      dstS s <> "." <> case sp of
                         "t" -> "w"
                         "f" -> "g"
                         _   -> T.pack sp
    Just n ->
      case n of
        NRetSnd{..} -> T.pack nName <> "[0]"
        NDiv{}      -> if sp=="0" then dstN s else dstN s <> ".1"
        NDivI{}     -> if sp=="0" then dstN s else dstN s <> ".1"
        NCommit{}   -> if sp=="0" then dstN s else dstN s <> ".1"
        NStopSpec{} -> if sp=="0" then dstN s else dstN s <> ".1"
        _           -> dstN s
    Nothing -> dstN s

fmtOp :: [T.Text] -> T.Text
fmtOp []  = "z0"
fmtOp [x] = x
fmtOp xs  = "[" <> T.intercalate ", " xs <> "]"

----------------------------------------------------------------
-- Construção do mapa de entradas
----------------------------------------------------------------
buildInputs :: DGraph DNode -> M.Map (NodeId,String) [T.Text]
buildInputs g =
  let step m (s,sp,d,dp) =
        let src = outName g s sp
        in M.insertWith (++) (d,dp) [src] m
  in foldl step M.empty (dgEdges g)

findCallgroupTag
  :: DGraph DNode
  -> M.Map (NodeId,String) [T.Text]
  -> NodeId
  -> Maybe T.Text
findCallgroupTag g _im nid =
  case [ s | (s,_sp,d,dp) <- dgEdges g, d==nid, dp=="1"
           , case M.lookup s (dgNodes g) of
               Just NCallGroup{} -> True
               _                 -> False ] of
    (cg:_) -> Just (tagName cg)
    _      -> Nothing

orderedPins :: M.Map (NodeId,String) [T.Text] -> NodeId -> [String]
orderedPins im k = [ p | (n,p) <- M.keys im, n==k ]

----------------------------------------------------------------
-- Emissão nó-a-nó
----------------------------------------------------------------
emitNode
  :: DGraph DNode
  -> M.Map (NodeId,String) [T.Text]
  -> (NodeId, DNode)
  -> [T.Text]
emitNode g im (nid, dn) =
  case dn of
    -------------------------------------------------- Consts
    NConstI{..} -> ["const "  <> dstN nid <> ", " <> showT cInt]
    NConstF{..} -> ["fconst " <> dstN nid <> ", " <> showT cFloat]
    NConstD{..} -> ["dconst " <> dstN nid <> ", " <> showT cDouble]

    -------------------------------------------------- Binárias
    NAdd{}  -> bin2 "add"
    NSub{}  -> bin2 "sub"
    NMul{}  -> bin2 "mul"
    NDiv{}  -> ["div "  <> dstN nid <> ", " <> fmtOp (gi "0") <> ", " <> fmtOp (gi "1")]
    NFAdd{} -> bin2 "fadd"
    NFSub{} -> bin2 "fsub"
    NFMul{} -> bin2 "fmult"
    NFDiv{} -> bin2 "fdiv"
    NDAdd{} -> bin2 "dadd"
    NBand{} -> bin2 "band"

    -------------------------------------------------- Imediatas
    NAddI{..}  -> bin1imm "addi"  (showT iImm)
    NSubI{..}  -> bin1imm "subi"  (showT iImm)
    NMulI{..}  -> bin1imm "muli"  (showT iImm)
    NFMulI{..} -> bin1imm "fmuli" (showT fImm)
    NDivI{..}  -> ["divi " <> dstN nid <> ", "
                          <> fmtOp (gi "0") <> ", " <> showT iImm]

    -------------------------------------------------- Comparações / steer
    NLThan{}    -> bin2 "lthan"
    NGThan{}    -> bin2 "gthan"
    NEqual{}    -> bin2 "equal"
    NLThanI{..} -> bin1imm "lthani" (showT iImm)
    NGThanI{..} -> bin1imm "gthani" (showT iImm)
    NSteer{}    -> ["steer " <> dstS nid <> ", "
                           <> fmtOp (gi "1") <> ", " <> fmtOp (gi "0")]

    -------------------------------------------------- TALM runtime
    NCallGroup{..} ->
      [ "callgroup(\"" <> tagName nid <> "\",\"" <> T.pack nName <> "\")" ]

    NCallSnd{..} ->
      let src0    = fmtOp (gi "0")
          tag     = maybe "0" id (findCallgroupTag g im nid)
      in  [ "callsnd " <> argAsFunSlot nName
            <> ", " <> src0 <> ", " <> tag ]

    NRetSnd{..} ->
      let tag = maybe "0" id (findCallgroupTag g im nid)
      in  [ "retsnd " <> T.pack nName <> "[0], z0, " <> tag ]

    NRet{..} ->
      let src0 = case gi "0" of
                   (h:_) -> h
                   []    -> "z0"
      in  [ "ret " <> T.pack nName <> ", " <> src0 <> ", z0" ]

    -------------------------------------------------- tag/val
    NTagVal{} -> one1 "tagval"
    NValTag{} -> bin2 "valtag"

    -------------------------------------------------- DMA / spec
    NCpHToDev{}  -> ["cphtodev " <> dstN nid <> ", 0"]
    NCpDevToH{}  -> ["cpdevtoh " <> dstN nid <> ", 0"]

    -------------------------------------------------- commit / stopspec
    NCommit{}   -> multi "commit"
    NStopSpec{} -> multi "stopspec"

    -------------------------------------------------- Formal arg
    NArg{} -> ["add " <> dstN nid <> ", " <> fmtOp (gi "0") <> ", z0"]

    -------------------------------------------------- Super-inst
    NSuper{..} ->
      let pins = orderedPins im nid
          srcs = map (\p -> fmtOp (gi p)) pins
          base = case (superSpec, superImm) of
                   (False, Nothing) -> "super"
                   (True , Nothing) -> "specsuper"
                   (False, Just _)  -> "superi"
                   (True , Just _)  -> "specsuperi"
          imm  = maybe [] (\t -> [showT t]) superImm
      in  [ T.intercalate ", " $
              (T.pack base <> " " <> dstN nid)
              : [ showT superNum
                , showT (max 1 superOuts)
                ]
              ++ srcs ++ imm
          ]
  where
    gi  p     = M.findWithDefault [] (nid,p) im
    bin2 mnem = [ T.pack mnem <> " " <> dstN nid <> ", "
                              <> fmtOp (gi "0") <> ", " <> fmtOp (gi "1") ]
    bin1imm m immTxt =
      [ T.pack m <> " " <> dstN nid <> ", " <> fmtOp (gi "0") <> ", " <> immTxt ]
    one1 mnem  = [ T.pack mnem <> " " <> dstN nid <> ", " <> fmtOp (gi "0") ]
    multi base =
      let pins = orderedPins im nid
          srcs = map (\p -> fmtOp (gi p)) pins
      in  [ T.pack base <> " " <> T.intercalate ", " (dstN nid : srcs) ]

----------------------------------------------------------------
-- assemble : entrada = grafo; saída = .fl text
----------------------------------------------------------------
assemble :: DGraph DNode -> T.Text
assemble g =
  T.unlines $ "const z0, 0" : concatMap (emitNode g inMap) nodes
  where
    nodes = sortOn fst (M.toList (dgNodes g))
    inMap = buildInputs g
