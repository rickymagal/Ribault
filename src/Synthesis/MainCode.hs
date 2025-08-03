{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Entry point para gerar TALM assembly (via SSA) a partir da mesma pipeline usada no MainGraph.
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents, hPutStrLn, stderr)
import System.Exit        (exitFailure)
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO

-- Front-end
import Lexer    (alexScanTokens)
import Parser   (parse)
import Syntax   (Program)
import Semantic (checkAll)

-- Back-end
import qualified Synthesis.Builder as DF
import qualified Synthesis.SSA     as SSA
import qualified Synthesis.Codegen as CG

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
    [file] -> readFile file
    []     -> getContents
    _      -> hPutStrLn stderr "Usage: lambdaflow-asm [file]" >> exitFailure

  let ast = parse (alexScanTokens src)
  case checkAll ast of
    [] -> do
      -- 1) monta DFG
      let dfg0 = DF.buildProgram ast
      -- 2) aplica transformação SSA (insere InstIncTag onde for preciso)
          dfg1 = SSA.ssaTransform dfg0
      -- 3) gera TALM
          asm  = CG.generateTALM dfg1
      TLIO.putStr asm
    errs ->
      mapM_ (hPutStrLn stderr . show) errs >> exitFailure
