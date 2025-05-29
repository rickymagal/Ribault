{-# LANGUAGE OverloadedStrings #-}

-- | Entry point for the Dataflow graph generation tool.
--
-- This executable reads a Haskell-subset program from a file or standard input,
-- performs lexical scanning, parsing, and semantic analysis, and then emits
-- the dataflow graph in Graphviz DOT format if no errors are found.
module Main where

import System.Environment (getArgs)      -- ^ Access command-line arguments
import System.IO          (readFile, getContents) -- ^ Read from file or stdin
import System.Exit        (exitFailure)  -- ^ Exit with failure code on error
import Prelude hiding (putStr)           -- ^ Hide default putStr
import qualified Data.Text.Lazy.IO as TLIO -- ^ Efficient lazy I/O for Text

import Lexer       (alexScanTokens)      -- ^ Tokenize input source
import Parser      (parse)               -- ^ Parse tokens into an AST
import Syntax      (Program)            -- ^ AST data types
import Semantic    (checkAll)            -- ^ Semantic and type checking
import GraphGen    (programToDataflowDot) -- ^ Render AST as DOT dataflow graph

-- | Main entry point.
--
-- Reads source code from a file (if a single filename argument is provided)
-- or from standard input otherwise.  Performs semantic validation, and on
-- success writes the dataflow graph in DOT format to stdout.  If any semantic
-- or type errors are detected, they are printed to stderr and the program
-- exits with a failure code.
main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    [file] -> readFile file
    []     -> getContents
    _      -> putStrLn "Usage: lambdaflow [file]" >> exitFailure

  let ast = parse (alexScanTokens input)

  case checkAll ast of
    []   -> TLIO.putStr $ programToDataflowDot ast
    errs -> mapM_ print errs >> exitFailure
