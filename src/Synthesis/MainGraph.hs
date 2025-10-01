{-# LANGUAGE OverloadedStrings #-}
module Main where

----------------------------------------------------------------------
-- Bibliotecas padrão
----------------------------------------------------------------------
import Prelude            hiding (readFile, getContents)
import System.Environment (getArgs)
import System.IO          (readFile, getContents, hPutStrLn, stderr)
import System.Exit        (exitFailure)

import qualified Data.Text.Lazy.IO as TLIO

----------------------------------------------------------------------
-- Front-end
----------------------------------------------------------------------
import Lexer    (alexScanTokens)
import Parser   (parse)
import Semantic (checkAll)
import Syntax   (Program)

----------------------------------------------------------------------
-- Back-end: Builder → DOT
----------------------------------------------------------------------
import qualified Synthesis.Builder  as DF  -- buildProgram :: Program -> DFG
import qualified Synthesis.GraphViz as GV  -- toDot        :: DFG     -> Text

----------------------------------------------------------------------
main :: IO ()
main = do
  src <- getInput
  let ast :: Program
      ast = parse (alexScanTokens src)

  case checkAll ast of
    []   -> TLIO.putStr . GV.toDot $ DF.buildProgram ast
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure

----------------------------------------------------------------------
-- Util: arquivo ou stdin ------------------------------------------------
----------------------------------------------------------------------
getInput :: IO String
getInput = do
  args <- getArgs
  case args of
    [file] -> readFile file
    []     -> getContents
    _      -> hPutStrLn stderr "Usage: lambdaflow-df [file]" >> exitFailure
