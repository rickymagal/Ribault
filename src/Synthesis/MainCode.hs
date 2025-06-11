{-# LANGUAGE OverloadedStrings #-}

-- | MainCode.hs
-- Entry point for the DOT→TALM code generator
module Main where

import           System.IO            (getContents)
import qualified Data.Text.Lazy.IO    as TLIO
import           Data.Text.Lazy       (Text)
import           Codegen              (parseNodes, parseEdges, generateInstructions)

-- | Program entry: read DOT from stdin, write TALM assembly to stdout\ nmain :: IO ()
main :: IO ()
main = do
  -- Read the entire DOT graph from standard input
  dot  <- TLIO.getContents
  -- Parse nodes and edges
  let nodes  = parseNodes dot
      edges  = parseEdges dot
      -- Generate TALM instruction lines
      instrs = generateInstructions nodes edges
  -- Emit each instruction on its own line
  mapM_ TLIO.putStrLn instrs
