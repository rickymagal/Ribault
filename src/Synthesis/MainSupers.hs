{-# LANGUAGE OverloadedStrings #-}

-- Lê .hsk, faz parse+check, renomeia Supers para s#, coleta e imprime Supers.hs (ou nada).

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
      putStr out   -- vazio ⇒ Makefile detecta e não gera .so
    errs -> mapM_ (hPutStrLn stderr . show) errs >> exitFailure
