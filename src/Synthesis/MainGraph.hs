{-# LANGUAGE OverloadedStrings #-}

-- | Executável que:
--     1. faz front-end (lexer → parser → checagem semântica);
--     2. gera o **DFG** com o Builder;
--     3. imprime o grafo em formato DOT/GraphViz.
--
--   Compile:
--     ghc -isrc -o lambdaflow-df src/Synthesis/MainGraph.hs
--
--   Uso:
--     lambdaflow-df [arquivo.hsk]
--     cat prog.hsk | lambdaflow-df
----------------------------------------------------------------------
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents, hPutStrLn, stderr)
import System.Exit        (exitFailure)
import Prelude            hiding (putStr)

import qualified Data.Text.Lazy.IO as TLIO

-- Front-end -------------------------------------------------------------
import Lexer    (alexScanTokens)
import Parser   (parse)
import Syntax   (Program)
import Semantic (checkAll)

-- Back-end: Builder → GraphViz -----------------------------------------
import qualified Synthesis.Builder  as DF  -- buildProgram :: Program -> DFG
import qualified Synthesis.GraphViz as GV  -- render       :: DFG     -> Text

----------------------------------------------------------------------
main :: IO ()
main = do
  input <- getInput
  let ast :: Program
      ast = parse (alexScanTokens input)

  case checkAll ast of
    []   -> TLIO.putStr . GV.render $ DF.buildProgram ast
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure

-- | Lê arquivo passado na linha de comando ou stdin se nenhum argumento.
getInput :: IO String
getInput = do
  args <- getArgs
  case args of
    [file] -> readFile file
    []     -> getContents
    _      -> hPutStrLn stderr "Usage: lambdaflow-df [file]" >> exitFailure
