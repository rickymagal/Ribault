{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Synthesis.GraphViz
  ( toDot  -- :: DGraph DNode -> Data.Text.Lazy.Text
  ) where

----------------------------------------------------------------------
-- Imports do projeto
----------------------------------------------------------------------
import           Types                 (DGraph(..))
import           Node                  (DNode(..), nodeName)
import qualified Data.Map              as M

----------------------------------------------------------------------
-- Imports padrão
----------------------------------------------------------------------
import           Data.Char             (isDigit, toUpper)
import qualified Data.Text.Lazy        as TL
import           Data.Text.Lazy        (Text)

----------------------------------------------------------------------
-- API
----------------------------------------------------------------------
toDot :: DGraph DNode -> Text
toDot g =
  TL.unlines $
    [ "digraph G {" ] ++
    concatMap renderNode (M.toList (dgNodes g)) ++
    concatMap (renderEdge nameOf) (reverse (dgEdges g)) ++
    [ "}" ]
  where
    nameOf nid = case M.lookup nid (dgNodes g) of
                   Just n  -> nodeName n
                   Nothing -> "unknown_" ++ show nid

----------------------------------------------------------------------
-- Renderização de nós (idêntico ao script do assembler)
----------------------------------------------------------------------
renderNode :: (Int, DNode) -> [Text]
renderNode (_nid, n) =
  [ "node [shape=box, style=rounded, fontsize=12];"
  , TL.pack $
      "node [label=\"" ++ opcode n ++ " " ++ nodeName n
      ++ "\"] " ++ safeId (nodeName n) ++ ";"
  ]

----------------------------------------------------------------------
-- Renderização de arestas (duas linhas por aresta)
-- <SRC> -> <DST>
-- [label = "(PORT,IDX)", fontsize=10]
----------------------------------------------------------------------
renderEdge :: (Int -> String) -> (Int, String, Int, String) -> [Text]
renderEdge nameOf (srcId, srcPort, dstId, dstPort) =
  [ TL.pack (safeId (nameOf srcId) ++ " -> " ++ safeId (nameOf dstId))
  , TL.pack ("[label = \"(" ++ portLabel ++ "," ++ idxLabel ++ ")\", fontsize=10]")
  ]
  where
    portLabel = map toUpper (if null srcPort then "0" else srcPort)
    idxLabel  = if all isDigit dstPort && not (null dstPort) then dstPort else "0"

----------------------------------------------------------------------
-- Mnemônicos (iguais aos do assembler)
----------------------------------------------------------------------
opcode :: DNode -> String
opcode = \case
  -- Constantes
  NConstI{}    -> "const"
  NConstF{}    -> "fconst"
  NConstD{}    -> "dconst"

  -- ALU binárias 1 saída
  NAdd{}       -> "add"
  NSub{}       -> "sub"
  NMul{}       -> "mul"
  NFAdd{}      -> "fadd"
  NDAdd{}      -> "dadd"
  NBand{}      -> "band"

  -- ALU binária 2 saídas
  NDiv{}       -> "div"

  -- ALU com imediato
  NAddI{}      -> "addi"
  NSubI{}      -> "subi"
  NMulI{}      -> "muli"
  NFMulI{}     -> "fmuli"
  NDivI{}      -> "divi"

  -- Comparações e steer
  NLThan{}     -> "lthan"
  NGThan{}     -> "gthan"
  NEqual{}     -> "equal"
  NLThanI{}    -> "lthani"
  NGThanI{}    -> "gthani"
  NSteer{}     -> "steer"

  -- Controle/tag/call/ret
  NIncTag{}    -> "inctag"
  NIncTagI{}   -> "inctagi"
  NCallSnd{}   -> "callsnd"
  NRetSnd{}    -> "retsnd"
  NRet{}       -> "ret"

  -- Converters
  NTagVal{}    -> "tagval"
  NValTag{}    -> "valtag"

  -- GPU copies
  NCpHToDev{}  -> "cphtodev"
  NCpDevToH{}  -> "cpdevtoh"

  -- Commit/especulação (2 saídas)
  NCommit{}    -> "commit"
  NStopSpec{}  -> "stopspec"

  -- Super-instruções
  NSuper{ superSpec = True,  superImm = Just{} } -> "specsuperi"
  NSuper{ superSpec = True,  superImm = Nothing }-> "specsuper"
  NSuper{ superSpec = False, superImm = Just{} } -> "superi"
  NSuper{ superSpec = False, superImm = Nothing }-> "super"

----------------------------------------------------------------------
-- IDs seguros para DOT
----------------------------------------------------------------------
safeId :: String -> String
safeId s =
  if null s then "_"
  else map (\c -> if c `elem` bad then '_' else c) s
  where
    bad :: String
    bad = " \t\r\n\"'`:$"
