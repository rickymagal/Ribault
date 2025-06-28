{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Inst → TALM assembly (texto).
--
-- Cobertura:
--   • InstConst  / fconst
--   • InstBinop  (+ – * / % …)
--   • InstBinopI
--   • InstSteer
--   • InstIncTag
--   • InstSuper
--   • InstCallGrp / CallSnd / RetSnd / Return
--
--  Faltando: InstPar, flags de paralelismo avançadas etc.

module Synthesis.Codegen
  ( assemble        -- :: [Inst] -> Text
  , saveAsm         -- :: FilePath -> [Inst] -> IO ()
  ) where

import           Synthesis.Instruction
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.List            (intercalate)

------------------------------------------------------------------------
-- API helpers
------------------------------------------------------------------------

assemble :: [Inst] -> T.Text
assemble = T.unlines . concatMap emit

saveAsm :: FilePath -> [Inst] -> IO ()
saveAsm fp = TIO.writeFile fp . assemble

------------------------------------------------------------------------
-- Core emitter
------------------------------------------------------------------------

emit :: Inst -> [T.Text]
emit = \case
  --------------------------------------------------------------------
  -- Constantes
  --------------------------------------------------------------------
  InstConst{..}
    | litType == "int"   ->
        [instr "const"  [nid nodeId, T.pack (show litVal)]]
    | litType == "float" ->
        [instr "fconst" [nid nodeId
                        , T.pack (show (fromIntegral litVal / 100 :: Double))]]
    | otherwise          -> []

  --------------------------------------------------------------------
  -- Binários
  --------------------------------------------------------------------
  InstBinop{..} ->
        [instr (tyPref typStr <> asmOp binOp)
               [nid nodeId, srcs leftSrc, srcs rightSrc]]

  InstBinopI{..} ->
        [instr (tyPref typStr <> asmOp binOp <> "i")
               [nid nodeId, srcs uniSrc, T.pack (show immedVal)]]

  --------------------------------------------------------------------
  -- Steer
  --------------------------------------------------------------------
  InstSteer{..} ->
        [instr "steer" [nid nodeId, srcs steerExpr, srcs steerInp]]

  --------------------------------------------------------------------
  -- IncTag
  --------------------------------------------------------------------
  InstIncTag{..} ->
        [instr "inctag" [nid nodeId, srcs tagInp]]

  --------------------------------------------------------------------
  -- Super-instruction
  --------------------------------------------------------------------
  InstSuper{..} ->
        let header = [ nid nodeId
                     , T.pack (show superNum)
                     , T.pack (show superOutN) ]
            inputs = map srcs superInp
        in  [instr "super" (header ++ inputs)]

  --------------------------------------------------------------------
  -- Chamadas
  --------------------------------------------------------------------
  InstCallGrp{..} ->
        [T.concat ["callgroup(\"", T.pack cgName
                  , "\", \"", T.pack cgFun, "\")"]]

  InstCallSnd{..} ->
        [instr "callsnd"
               [T.concat [T.pack csFun, "[", T.pack (show csIdx), "]"]
               , srcs csOper
               , T.pack csGroup]]

  InstRetSnd{..} ->
        [instr "retsnd"
               [T.concat [T.pack rsFun, "[0]"]
               , srcs rsOper
               , T.pack rsGroup]]

  InstReturn{..} ->
        [instr "ret" [nid nodeId, srcs retExpr, srcs retSend]]

  -- Ainda não tratados
  --------------------------------------------------------------------
  InstPar{} -> ["# TODO InstPar"]

------------------------------------------------------------------------
-- Pretty helpers
------------------------------------------------------------------------

asmOp :: String -> T.Text
asmOp = \case
  "+"  -> "add" ; "-"  -> "sub"
  "*"  -> "mul" ; "/"  -> "div"
  "%"  -> "mod"
  "&&" -> "and"
  "<"  -> "lthan" ; ">" -> "gthan"
  "==" -> "eq"    ; "!="-> "neq"
  other -> T.pack other

tyPref :: String -> T.Text
tyPref "int"    = ""
tyPref "float"  = "f"
tyPref "double" = "d"
tyPref other    = T.pack other <> "_"

instr :: T.Text -> [T.Text] -> T.Text
instr m fs = T.intercalate " " (m : fs)

nid :: NodeId -> T.Text
nid (NodeId i) = T.pack ("n" <> show i)

------------------------------------------------------------------------
-- Signals pretty-print
------------------------------------------------------------------------

srcs :: [Signal] -> T.Text
srcs xs = case xs of
  [s] -> sig s
  _   -> T.concat ["[", T.intercalate ", " (map sig xs), "]"]


sig :: Signal -> T.Text
sig = \case
  SigInstPort nId port _ ->
      nid nId <> "." <> T.pack (show port)          -- ← usa helper nid
  SigSteerPort nId br    ->
      nid nId <> "." <> (if br == T then "t" else "f")
  SigReturnPort f g      ->
      T.pack f <> "ret." <> T.pack g
