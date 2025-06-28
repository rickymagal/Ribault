{-# LANGUAGE LambdaCase #-}   
{-# LANGUAGE OverloadedStrings #-}
-- | Entry point para gerar **TALM assembly** a partir da mesma pipeline
--   usada no MainGraph.  Ele:
--
--   1. Lê fonte Haskell-subset (arquivo ou stdin)
--   2. Faz lexing, parsing e verificação semântica
--   3. Constrói o grafo data-flow (Inst)
--   4. Converte o grafo para assembly TALM e imprime em stdout
--
--   Compile, por exemplo:
--     ghc -O2 -isrc -o lambdaflow-asm src/Synthesis/MainASM.hs
--
--   Uso:
--     lambdaflow-asm programa.hsk
--     cat prog.hsk | lambdaflow-asm
-----------------------------------------------------------------------------
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents, hPutStrLn, stderr)
import System.Exit        (exitFailure)
import qualified Data.Text            as TS
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO as TLIO

-- Front-end ---------------------------------------------------------------
import Lexer    (alexScanTokens)
import Parser   (parse)
import Syntax   (Program)
import Semantic (checkAll)

-- Back-end ----------------------------------------------------------------

import qualified Synthesis.Builder  as DF   -- AST → [Inst]
import qualified Synthesis.Codegen  as CG   -- [Inst] → TALM assembly (lazy Text)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  src <- getArgs >>= \case
           [file] -> readFile file
           []     -> getContents
           _      -> hPutStrLn stderr "Usage: lambdaflow-asm [file]" >> exitFailure

  let ast = parse (alexScanTokens src)

  case checkAll ast of
    [] -> do
      let df      = DF.buildProgram ast
          asmText = TL.fromStrict (CG.assemble df)   -- ← aqui
      TLIO.putStr asmText
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure
