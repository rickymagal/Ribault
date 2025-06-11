{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Codegen – convert a DOT data-flow graph (from GraphGen) into
--   TALM FlowASM text.
module Codegen
  ( parseNodes
  , parseEdges
  , generateInstructions
  ) where

import           Data.Char      (isDigit)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

----------------------------------------------------------------------
-- Simple DOT parsing ------------------------------------------------
----------------------------------------------------------------------

stripQuotes :: Text -> Text
stripQuotes t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' =
      T.init (T.tail t)
  | otherwise = t

parseNodes :: Text -> [(Text, Text)]
parseNodes input =
  [ ( stripQuotes $ T.strip (head parts)
    , label )
  | line <- T.lines input
  , "[label=" `T.isInfixOf` line
  , let parts   = T.splitOn "[" line
        lblPart = T.dropWhile (/= '\"') (parts !! 1)
        label   = T.takeWhile (/= '\"') (T.drop 1 lblPart)
  ]

parseEdges :: Text -> [(Text, Text)]
parseEdges input =
  [ ( stripQuotes (T.strip l)
    , stripQuotes (T.strip (T.takeWhile (/= ';') r')) )
  | line <- T.lines input
  , "->" `T.isInfixOf` line
  , let (l,r) = T.breakOn "->" line
        r'    = T.drop 2 r
  ]

----------------------------------------------------------------------
-- Assembly generation ----------------------------------------------
----------------------------------------------------------------------

generateInstructions
  :: [(Text, Text)]  -- ^ nodes
  -> [(Text, Text)]  -- ^ edges
  -> [Text]
generateInstructions nodes edges = map (genNode edges) nodes

genNode :: [(Text, Text)] -> (Text, Text) -> Text
genNode edges (name,label) =
  let ins      = [ s | (s,d) <- edges, d == name ]
      outs     = [ d | (s,d) <- edges, s == name ]
      (op,imm) = case T.splitOn ":" label of
                   [x,y] -> (x,Just y)
                   [x]   -> (x,Nothing)
                   _     -> error "malformed label"

      opcode   = mapOpcode op
      nRes     = calcResults opcode outs

      -- destination handling
      dstTxt   | opcode == "const" = name
               | otherwise         = T.intercalate ", " (take nRes outs)

      -- immediate handling
      immRaw   = maybe T.empty id imm
      immTxt   | opcode == "const" && needsQuotes immRaw
               = "\"" <> immRaw <> "\""
               | otherwise = immRaw

      srcTxt   = T.intercalate ", " ins
      operands = filter (not . T.null) [dstTxt, srcTxt, immTxt]
  in if null operands
        then opcode
        else opcode <> " " <> T.intercalate ", " operands

-- | Heuristic: quote immediates that are non-numeric strings
needsQuotes :: Text -> Bool
needsQuotes t =
  let s = T.unpack t
  in  length s > 1
      && (not (all isDigit s))
      && s /= "true" && s /= "false"

----------------------------------------------------------------------
-- Opcode map & arities ---------------------------------------------
----------------------------------------------------------------------

mapOpcode :: Text -> Text
mapOpcode op
  | op == "var" || op == "in" = "split"
  | op `elem` allowed         = op
  | otherwise                 = error ("unknown opcode " ++ T.unpack op)
  where
    allowed =
      [ "const","add","sub","mul","div","mod"
      , "andi","ori","xori","and","or","xor"
      , "not","eq","neq","lt","leq","gt","geq"
      , "addi","subi","muli","divi"
      , "steer","merge","split"
      , "callgroup","callsnd","retsnd","ret"
      , "inctag","tagop"
      , "super","specsuper","superinstmacro"
      ]

calcResults :: Text -> [Text] -> Int
calcResults "steer" _    = 2
calcResults "split" outs = length outs
calcResults "inctag" _   = 1
calcResults op _
  | op `elem` [ "callgroup","callsnd","retsnd","ret" ] = 0
  | otherwise = 1
