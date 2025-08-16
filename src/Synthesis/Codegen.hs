{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Gera **TALM assembly (texto)** a partir do DFG.
--   Mantém o formato esperado pelo FlowASM (nome, ordem de operandos e
--   sufixos de porta ".t" / ".f" / ".1", etc).
module Synthesis.Codegen
  ( assemble  -- :: DGraph DNode -> Text (estrito)
  ) where

import           Data.List            (sortBy, intersperse)
import           Data.Char            (isDigit)
import qualified Data.Map            as M
import qualified Data.Text           as T
import           Data.Text            (Text)
import           Data.Ord             (comparing)

import           Types                (DGraph(..), NodeId)
import           Node                 (DNode(..), nodeName)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

assemble :: DGraph DNode -> T.Text
assemble g =
  T.unlines $
    concatMap (emitNode g) (sortedNodes g)

--------------------------------------------------------------------------------
-- Ordem estável de nós / arestas
--------------------------------------------------------------------------------

sortedNodes :: DGraph a -> [(NodeId, a)]
sortedNodes DGraph{..} = sortBy (comparing fst) (M.toList dgNodes)

incomingOf :: DGraph n -> NodeId -> [(NodeId, T.Text, T.Text)]
incomingOf DGraph{..} dst =
  -- (srcId, srcPort, dstPort) apenas dos edges que chegam ao 'dst'
  [ (s, T.pack sp, T.pack dp)
  | (s, sp, d, dp) <- dgEdges, d == dst
  ]

-- Ordena entradas primeiro pela porta de destino (numérica quando possível),
-- depois por id do nó de origem — dá estabilidade e previsibilidade.
orderInputs :: [(NodeId, T.Text, T.Text)] -> [(NodeId, T.Text, T.Text)]
orderInputs =
  let keyDst :: (NodeId, T.Text, T.Text) -> (Int, T.Text)
      keyDst (_, _sp, dp) = (parseInt dp, dp)
      keySrc :: (NodeId, T.Text, T.Text) -> NodeId
      keySrc (s, _sp, _dp) = s
  in sortBy (comparing keyDst <> comparing keySrc)

parseInt :: T.Text -> Int
parseInt t | T.all isDigit t && not (T.null t) = read (T.unpack t)
parseInt _ = 0

--------------------------------------------------------------------------------
-- Impressão de um nó como uma linha de assembly
--------------------------------------------------------------------------------

emitNode :: DGraph DNode -> (NodeId, DNode) -> [T.Text]
emitNode g (nid, n) =
  case n of
    -- Constantes
    NConstI{..}   -> [ row "const"   [ name, T.pack (show cInt) ] ]
    NConstF{..}   -> [ row "fconst"  [ name, prettyFloat  cFloat ] ]
    NConstD{..}   -> [ row "dconst"  [ name, prettyDouble cDouble] ]

    -- ALU 2-op, 1 saída
    NAdd{..}      -> [ row2 "add"    ]
    NSub{..}      -> [ row2 "sub"    ]
    NMul{..}      -> [ row2 "mul"    ]
    NFAdd{..}     -> [ row2 "fadd"   ]
    NDAdd{..}     -> [ row2 "dadd"   ]
    NBand{..}     -> [ row2 "band"   ]

    -- ALU 2-op, 2 saídas (div produz quociente e resto)
    NDiv{..}      -> [ row2 "div"    ]

    -- ALU com imediato (1 fonte + immed)
    NAddI{..}     -> [ rowImm1  "addi"   iImm   ]
    NSubI{..}     -> [ rowImm1  "subi"   iImm   ]
    NMulI{..}     -> [ rowImm1  "muli"   iImm   ]
    NFMulI{..}    -> [ rowImm1F "fmuli"  fImm   ]
    NDivI{..}     -> [ rowImm1  "divi"   iImm   ]

    -- Comparações e steer
    NLThan{..}    -> [ row2 "lthan"  ]
    NGThan{..}    -> [ row2 "gthan"  ]
    NEqual{..}    -> [ row2 "equal"  ]
    NLThanI{..}   -> [ rowImm1 "lthani" iImm ]
    NGThanI{..}   -> [ rowImm1 "gthani" iImm ]
    NSteer{..}    -> [ row2 "steer"  ]

    -- Controle/tag/call/ret
    NIncTag{..}   -> [ row1 "inctag" ]
    NIncTagI{..}  -> [ rowImm1 "inctagi" iImm ]
    NCallSnd{..}  -> [ rowImm1 "callsnd" taskId ]
    NRetSnd{..}   -> [ rowImm1 "retsnd"  taskId ]
    NRet{..}      -> [ row2 "ret"    ]  -- ret usa 2 fontes (valor e tag) no FlowASM da base

    -- Conversões de tag/valor
    NTagVal{..}   -> [ row1 "tagval" ]
    NValTag{..}   -> [ row2 "valtag" ]

    -- Cópias GPU (tamanho, destino, origem)
    NCpHToDev{..} -> [ rowN "cphtodev"  3 ]
    NCpDevToH{..} -> [ rowN "cpdevtoh"  3 ]

    -- Commit/especulação (var-arg)
    NCommit{..}   -> [ rowVar "commit" ]
    NStopSpec{..} -> [ rowVar "stopspec" ]

    -- Super-instruções
    NSuper{..}  ->
      let op | superSpec && superImm /= Nothing = "specsuperi"
             | superSpec                        = "specsuper"
             | superImm /= Nothing              = "superi"
             | otherwise                        = "super"
          base = [ name
                 , T.pack (show (0 :: Int))          -- <-- substitua por superNumber se existir
                 , T.pack (show superOuts)
                 ]
          srcs = incomingOps nid
          immT = maybe [] (\k -> [T.pack (show k)]) superImm
      in [ row op (base ++ srcs ++ immT) ]

  where
    name   = T.pack (nodeName n)
    incom  = incomingOf g nid
    incom' = orderInputs incom

    -- gera operandos (como "inst", "inst.t", "inst.1")
    toOp :: (NodeId, T.Text, T.Text) -> T.Text
    toOp (srcId, srcPort, _dstPort) =
      let srcName = case M.lookup srcId (dgNodes g) of
                      Just nn -> T.pack (nodeName nn)
                      Nothing -> T.pack ("instruction_" ++ show srcId)
          portSuffix
            | srcPort == "0" || T.null srcPort = ""
            | otherwise                        = T.cons '.' srcPort
      in srcName <> portSuffix

    incomingOps :: NodeId -> [T.Text]
    incomingOps _ = map toOp incom'

    -- pega 1/2/N/var-arg
    takeN :: Maybe Int -> [T.Text] -> [T.Text]
    takeN m xs = case m of
      Just n  -> take n xs
      Nothing -> xs

    row :: Text -> [Text] -> Text
    row op xs = T.concat [ op, " ", name, ", ", commaSep xs ]

    row1 :: Text -> Text
    row1 op =
      let ops = takeN (Just 1) (incomingOps nid)
          single = case ops of
                     (a:_) -> a
                     []    -> "0"
      in T.concat [ op, " ", name, ", ", single ]

    row2 :: Text -> Text
    row2 op =
      let ops = takeN (Just 2) (incomingOps nid)
      in T.concat [ op, " ", name, ", ", commaSep ops ]

    rowN :: Text -> Int -> Text
    rowN op n =
      let ops = takeN (Just n) (incomingOps nid)
      in T.concat [ op, " ", name, ", ", commaSep ops ]

    rowVar :: Text -> Text
    rowVar op = T.concat [ op, " ", name, ", ", commaSep (incomingOps nid) ]

    rowImm1 :: Text -> Int -> Text
    rowImm1 op imm =
      let ops = takeN (Just 1) (incomingOps nid)
      in T.concat [ op, " ", name, ", ", commaSep ops, ", ", T.pack (show imm) ]

    rowImm1F :: Text -> Float -> Text
    rowImm1F op imm =
      let ops = takeN (Just 1) (incomingOps nid)
      in T.concat [ op, " ", name, ", ", commaSep ops, ", ", prettyFloat imm ]

--------------------------------------------------------------------------------
-- Utilitários de texto/numéricos
--------------------------------------------------------------------------------

commaSep :: [Text] -> Text
commaSep = T.concat . intersperse ", "

prettyFloat :: Float -> Text
prettyFloat x =
  let s = show x
  in if '.' `elem` s then T.pack s else T.pack (s ++ ".0")

prettyDouble :: Double -> Text
prettyDouble x =
  let s = show x
  in if '.' `elem` s then T.pack s else T.pack (s ++ ".0")
