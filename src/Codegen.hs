{-# LANGUAGE OverloadedStrings #-}

-- | Codegen.hs
-- Translator from TALM dataflow graph DOT format into TALM assembly.
module Codegen
  ( parseNodes
  , parseEdges
  , generateInstructions
  ) where

import Data.Char       (isSpace)
import Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy as T

-- | Parse node definitions in DOT: lines containing "[label=".
parseNodes :: Text -> [(Text, Text)]
parseNodes input =
  [ (name, label)
  | line <- T.lines input
  , "[label=" `T.isInfixOf` line
  , let parts   = T.splitOn "[" line
  , let name    = T.strip (head parts)
  , let lblPart = T.dropWhile (/= '"') (parts !! 1)
  , let label   = T.takeWhile (/= '"') (T.drop 1 lblPart)
  ]

-- | Parse edges in DOT: lines containing "->".
parseEdges :: Text -> [(Text, Text)]
parseEdges input =
  [ (src, dst)
  | line <- T.lines input
  , "->" `T.isInfixOf` line
  , let (l, r) = T.breakOn "->" line
  , let src    = T.strip l
  , let r'     = T.drop 2 r
  , let dst    = T.strip (T.takeWhile (/= ';') r')
  ]

-- | Generate TALM instructions for all nodes (order preserved)
generateInstructions :: [(Text, Text)] -> [(Text, Text)] -> [Text]
generateInstructions nodes edges = map (genNode edges) nodes

-- | Generate a single TALM instruction from a node and its edges
genNode :: [(Text, Text)] -> (Text, Text) -> Text
genNode edges (name, label) =
  let ins       = [ s | (s,d) <- edges, d == name ]
      outs      = [ d | (s,d) <- edges, s == name ]
      (op,immo) = case T.splitOn ":" label of
                     [x,y] -> (x, Just y)
                     [x]   -> (x, Nothing)
                     xs    -> (head xs, Just (last xs))
      opcode    = mapOpcode op
      immTxt    = maybe T.empty (T.cons ' ') immo
      nRes      = calcResults op outs
      nSrc      = length ins
      dsts      = T.intercalate "," (take nRes outs)
      srcs      = T.intercalate "," ins
      arrowTxt  = if nSrc > 0 then " <- " <> srcs else ""
  in T.concat [ opcode
              , " " , T.pack (show nRes)
              , " " , T.pack (show nSrc)
              , immTxt
              , " : " , dsts
              , arrowTxt
              ]

-- | Map operation name to TALM opcode
mapOpcode :: Text -> Text
mapOpcode "var" = "split"
mapOpcode "in"  = "split"
mapOpcode op
  | op `elem` [ "const","add","sub","mul","div"
              ,"andi","ori","xori","and","or","xor"
              ,"not","eq","neq","lt","leq","gt","geq"
              ,"addi","subi","muli","divi"
              ,"steer","merge","split"
              ,"callgroup","callsnd","retsnd","ret"
              ,"inctag","tagop"
              ,"super","specsuper","superinstmacro" ] = op
  | otherwise = error $ "Unknown label opcode: " ++ T.unpack op

-- | Determine number of results for an op
typeResults :: [(Text,Text)] -> Text -> [Text] -> Int
typeResults _ = calcResults

-- | Calculate results count by op and outgoing edges
calcResults :: Text -> [Text] -> Int
calcResults "steer" _    = 2
calcResults "split" outs = length outs
calcResults op _
  | op `elem` [ "callgroup","callsnd","retsnd","ret","inctag" ] = 0
  | otherwise = 1
