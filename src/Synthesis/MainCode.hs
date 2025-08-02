{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Entry point para gerar **TALM assembly** a partir da mesma pipeline usada no MainGraph.
--   Compile, por exemplo:
--     ghc -O2 -isrc -o lambdaflow-asm src/Synthesis/MainCode.hs
--
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
import qualified Synthesis.Builder  as DF
import qualified Synthesis.Codegen  as CG

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
      let dfg      = DF.buildProgram ast
          asmText  = CG.generateTALM dfg
      TLIO.putStr asmText
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure
