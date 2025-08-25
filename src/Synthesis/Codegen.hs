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

-- Saída principal
assemble :: DGraph DNode -> T.Text
assemble g =
  T.unlines $
    ["const z0, 0"] ++ concatMap (emitNode g inMap) nodes
  where
    nodes = sortOn fst (M.toList (dgNodes g))
    inMap = buildInputs g

-- Utilitários ---------------------------------------------------------------

showT :: Show a => a -> T.Text
showT = T.pack . show

dstN :: NodeId -> T.Text
dstN nid = "n" <> showT nid

dstS :: NodeId -> T.Text
dstS nid = "s" <> showT nid

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
    Just NSteer{} -> dstS s <> "." <> T.pack sp
    Just n ->
      case n of
        -- retorno de função deve ser referenciado como fun[0]
        NRetSnd{..} -> T.pack nName <> "[0]"
        -- Dois outputs: base = n<s>; saída 0 = n<s>, saída 1 = n<s>.1
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

-- Mapa de entradas: (dest,porta) -> [fontes]
buildInputs :: DGraph DNode -> M.Map (NodeId,String) [T.Text]
buildInputs g =
  let step m (s,sp,d,dp) =
        let src = outName g s sp
        in M.insertWith (++) (d,dp) [src] m
  in foldl step M.empty (dgEdges g)

-- acha o callgroup (porta 1) para callsnd/retsnd
findCallgroupTag :: DGraph DNode -> M.Map (NodeId,String) [T.Text] -> NodeId -> Maybe T.Text
findCallgroupTag g _im nid =
  case [ s | (s,_sp,d,dp) <- dgEdges g, d==nid, dp=="1"
           , case M.lookup s (dgNodes g) of
               Just NCallGroup{} -> True
               _                  -> False ] of
    (cg:_) -> Just ("c" <> showT cg)
    _      -> Nothing

orderedPins :: M.Map (NodeId,String) [T.Text] -> NodeId -> [String]
orderedPins im k = [ p | (n,p) <- M.keys im, n==k ]

-- Emissão -------------------------------------------------------------------

emitNode :: DGraph DNode -> M.Map (NodeId,String) [T.Text] -> (NodeId, DNode) -> [T.Text]
emitNode g im (nid, dn) =
  case dn of
    -- Consts
    NConstI{..} -> [ "const "  <> dstN nid <> ", " <> showT cInt ]
    NConstF{..} -> [ "fconst " <> dstN nid <> ", " <> showT cFloat ]
    NConstD{..} -> [ "dconst " <> dstN nid <> ", " <> showT cDouble ]

    -- Binárias
    NAdd{}  -> bin2 "add"
    NSub{}  -> bin2 "sub"
    NMul{}  -> bin2 "mul"
    NDiv{}  ->
      let a0 = fmtOp (getInputs "0")
          a1 = fmtOp (getInputs "1")
      in [ "div " <> dstN nid <> ", " <> a0 <> ", " <> a1 ]
    NFAdd{} -> bin2 "fadd"
    NDAdd{} -> bin2 "dadd"
    NBand{} -> bin2 "band"

    -- Imediatas
    NAddI{..}  -> bin1imm "addi"  (showT iImm)
    NSubI{..}  -> bin1imm "subi"  (showT iImm)
    NMulI{..}  -> bin1imm "muli"  (showT iImm)
    NFMulI{..} -> bin1imm "fmuli" (showT fImm)
    NDivI{..}  ->
      let a0 = fmtOp (getInputs "0")
      in [ "divi " <> dstN nid <> ", " <> a0 <> ", " <> showT iImm ]

    -- Comparações e steer
    NLThan{}    -> bin2 "lthan"
    NGThan{}    -> bin2 "gthan"
    NEqual{}    -> bin2 "equal"
    NLThanI{..} -> bin1imm "lthani" (showT iImm)
    NGThanI{..} -> bin1imm "gthani" (showT iImm)
    NSteer{}    ->
      let g0 = fmtOp (getInputs "1")
          v0 = fmtOp (getInputs "0")
      in [ "steer " <> dstS nid <> ", " <> g0 <> ", " <> v0 ]

    -- TALM: grupos/chamadas/retornos
    NCallGroup{..} ->
      [ "callgroup(\"c" <> showT nid <> "\",\"" <> T.pack nName <> "\")" ]

    NCallSnd{..} ->
      let src0    = fmtOp (getInputs "0")
          tag     = maybe "0" id (findCallgroupTag g im nid)
          slotTxt = argAsFunSlot nName
      in [ "callsnd " <> slotTxt <> ", " <> src0 <> ", " <> tag ]

    -- >>> correção: retsnd SEM payload; valor vem do 'ret <fun>, ...'
    NRetSnd{..} ->
      let tag  = maybe "0" id (findCallgroupTag g im nid)
          dstF = T.pack nName <> "[0]"
      in [ "retsnd " <> dstF <> ", z0, " <> tag ]

    -- ret da função
    NRet{..} ->
      let src0 = fmtOp (getInputs "0")
      in [ "ret " <> T.pack nName <> ", " <> src0 <> ", z0" ]

    -- tag/val
    NTagVal{} -> one1 "tagval"
    NValTag{} -> bin2 "valtag"

    -- DMA / spec
    NCpHToDev{}  -> [ "cphtodev " <> dstN nid <> ", 0" ]
    NCpDevToH{}  -> [ "cpdevtoh " <> dstN nid <> ", 0" ]

    -- commit / stopspec
    NCommit{} ->
      let pins = orderedPins im nid
          srcs = map (\p -> fmtOp (getInputs p)) pins
      in [ "commit " <> T.intercalate ", " (dstN nid : srcs) ]
    NStopSpec{} ->
      let pins = orderedPins im nid
          srcs = map (\p -> fmtOp (getInputs p)) pins
      in [ "stopspec " <> T.intercalate ", " (dstN nid : srcs) ]

    -- Formal
    NArg{} ->
      let a0 = fmtOp (getInputs "0")
      in [ "add " <> dstN nid <> ", " <> a0 <> ", z0" ]

    -- Super
    NSuper{..} ->
      let pins   = orderedPins im nid
          srcs   = map (\p -> fmtOp (getInputs p)) pins
          base   = case (superSpec, superImm) of
                     (False, Nothing) -> "super"
                     (True,  Nothing) -> "specsuper"
                     (False, Just _)  -> "superi"
                     (True,  Just _)  -> "specsuperi"
          imm    = maybe [] (\t -> [showT t]) superImm
          headOp = T.pack base <> " " <> dstN nid
          line   = T.intercalate ", "
                     ( headOp
                     : [ showT superNum
                       , showT (max 1 superOuts)
                       ] ++ srcs ++ imm )
      in [ line ]
  where
    getInputs pin = M.findWithDefault [] (nid, pin) im
    bin2 mnem =
      let a0 = fmtOp (getInputs "0")
          a1 = fmtOp (getInputs "1")
      in [ T.pack mnem <> " " <> dstN nid <> ", " <> a0 <> ", " <> a1 ]
    bin1imm mnem immTxt =
      let a0 = fmtOp (getInputs "0")
      in [ T.pack mnem <> " " <> dstN nid <> ", " <> a0 <> ", " <> immTxt ]
    one1 mnem =
      let a0 = fmtOp (getInputs "0")
      in [ T.pack mnem <> " " <> dstN nid <> ", " <> a0 ]
