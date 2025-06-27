{-# LANGUAGE OverloadedStrings #-}
-- | Entry point for the **data‑flow graph** generation tool.
--   Esta Main NÃO substitui `Analysis/MainAST.hs`; apenas acrescenta um
--   executável separado que faz **um passo extra**: depois da AST pronta e
--   verificada ele constrói o grafo de instruções e o imprime em DOT.
--
--   Compile algo como:
--
--   @
--   ghc -isrc -o lambdaflow-df src/Synthesis/MainGraph.hs
--   @
--
--   Uso:
--
--   > lambdaflow-df [arquivo.hsk]
--   > cat prog.hsk | lambdaflow-df
----------------------------------------------------------------------
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents, hPutStrLn, stderr)
import System.Exit        (exitFailure)
import Prelude hiding (putStr)
import qualified Data.Text.Lazy.IO as TLIO

-- Fases de front‑end -----------------------------------------------------
import Lexer    (alexScanTokens)   -- ^ Scanner
import Parser   (parse)            -- ^ Parser
import Syntax   (Program)          -- ^ AST datatypes (for type sigs)
import Semantic (checkAll)         -- ^ Verificação semântica

-- Geração do grafo Data‑flow --------------------------------------------
import qualified Synthesis.Builder  as DF  -- buildProgram :: Program -> [Inst]
import qualified Synthesis.GraphViz as GV  -- render       :: [Inst]  -> Text

----------------------------------------------------------------------
-- | Main entry: igual ao MainAST, mas com o passo DF extra.
main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    [file] -> readFile file
    []     -> getContents
    _      -> do hPutStrLn stderr "Usage: lambdaflow-df [file]"; exitFailure

  -- 1. Lexing & Parsing --------------------------------------------------
  let ast :: Program
      ast = parse (alexScanTokens input)

  -- 2. Semantic check ----------------------------------------------------
  case checkAll ast of
    []   -> do
      -- 3. AST → Data‑flow → DOT ----------------------------------------
      let df  = DF.buildProgram ast
          dot = GV.render df
      TLIO.putStr dot
    errs -> do
      mapM_ (hPutStrLn stderr . show) errs
      exitFailure
