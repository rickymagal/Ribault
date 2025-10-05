{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : CLI entry point that parses, semantically checks, and renders the AST to Graphviz DOT.
Copyright   :
License     :
Maintainer  : ricardo@example.com
Stability   : experimental
Portability : portable

## Overview

This executable reads a program in the project’s Haskell-like subset,
runs **lexing** ('Lexer.alexScanTokens'), **parsing** ('Parser.parse'),
optionally **renames super-instructions** ('Semantic.assignSuperNames'),
and performs **semantic + typing checks** ('Semantic.checkAll').
If no errors are found, the resulting 'Syntax.Program' is rendered as a
Graphviz DOT graph via 'ASTGen.programToDot' and written to @stdout@.

## Usage

- From a file:
@
lambdaflow-ast path/to/source.hsk > ast.dot
@

- From standard input:
@
cat source.hsk | lambdaflow-ast > ast.dot
@

If any errors are detected, they are printed to @stderr@ and the process exits
with a non-zero status.

## Exit codes

- @0@: success; DOT printed to @stdout@
- non-@0@: failure; errors printed to @stderr@

-}
-- | Entry point for the AST graph generation tool.
-- 
-- This executable reads a Haskell subset program from a file or standard input,
-- performs lexical scanning, parsing, and semantic analysis, and then emits
-- the abstract syntax tree (AST) in Graphviz DOT format if no errors are found.
module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents)
import System.Exit        (exitFailure)
import Prelude hiding (putStr)
import qualified Data.Text.Lazy.IO as TLIO

import Lexer    (alexScanTokens)  -- ^ Generate tokens from input text
import Parser   (parse)           -- ^ Build the AST from the token stream
import Syntax   (Program)         -- ^ AST data types
import Semantic (checkAll, assignSuperNames)        -- ^ Perform semantic and type checks
import ASTGen   (programToDot)    -- ^ Render Program AST as DOT graph

-- | Main entry point.
--
-- Reads program source from either a file (if a filename is provided as
-- the only argument) or from standard input otherwise. Performs semantic
-- validation, and on success writes the AST in DOT format to stdout.
-- If any semantic or type errors are detected, they are printed to stderr
-- and the program exits with a failure code.
main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    [file] -> readFile file
    []     -> getContents
    _      -> putStrLn "Usage: lambdaflow-ast [file]" >> exitFailure

  let ast0 = parse (alexScanTokens input)
  let ast  = assignSuperNames ast0
  case checkAll ast of
    []   -> TLIO.putStr $ programToDot ast
    errs -> mapM_ print errs >> exitFailure esse agora
