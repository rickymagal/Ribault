{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Read a .hsk file, parse + check, rename Supers to s#, collect and emit Supers.hs (or nothing).
-- Maintainer  : ricardofilhoschool@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Usage:
--   supersgen [file]
-- If no file is provided, reads source from STDIN. On success, prints the
-- generated Supers module to STDOUT. If there are no supers, prints an empty
-- string so the Makefile can skip building the shared library.

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath (takeBaseName)

import Lexer   (alexScanTokens)
import Parser  (parse)
import Semantic (checkAll, assignSuperNames)
import Syntax

import Synthesis.SuperExtract (collectSupers)
import Synthesis.SupersEmit   (emitSupersModule)

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [file] -> readFile file
            []     -> getContents
            _      -> hPutStrLn stderr "Usage: supersgen [file]" >> exitFailure

  let ast0 = parse (alexScanTokens src)

  case checkAll ast0 of
    [] -> do
      let ast  = assignSuperNames ast0
          base = case args of { [file] -> takeBaseName file; _ -> "stdin" }
          out  = emitSupersModule base (collectSupers ast)
      putStr out   -- empty ⇒ Makefile detects and skips .so generation
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure
