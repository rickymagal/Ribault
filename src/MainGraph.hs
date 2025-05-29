-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.IO          (readFile, getContents)
import System.Exit        (exitFailure)
import Prelude hiding (putStr)
import qualified Data.Text.Lazy.IO as TLIO

import Lexer       (alexScanTokens)
import Parser      (parse)
import Syntax      (Program)
import Semantic    (checkAll)
import GraphGen    (programToDataflowDot)

main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    [file] -> readFile file
    []     -> getContents
    _      -> putStrLn "Uso: lambdaflow [arquivo]" >> exitFailure

  let ast = parse (alexScanTokens input)

  case checkAll ast of
    []   -> TLIO.putStr $ programToDataflowDot ast
    errs -> mapM_ print errs >> exitFailure
