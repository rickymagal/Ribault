{-# LANGUAGE OverloadedStrings #-}

-- | Codegen.hs
-- A dependency-free translator from a TALM dataflow graph in DOT format into TALM assembly.
module Codegen
  ( parseNodes
  , parseEdges
  , generateInstructions
  ) where

import           Data.Char            (isSpace)
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T
import           Data.List            (partition)

-- | Parse node definitions in DOT: lines containing "[label=".
--   Example: f0_e0_out [label="const:1"];
parseNodes :: Text -> [ (Text, Text) ]
parseNodes input =
  [ (name, label)
  | line <- T.lines input
  , "[label=" `T.isInfixOf` line
  , let name = T.takeWhile (not . isSpace) line
  , let rest = T.dropWhile (/='[') line
  , let (_, after) = T.breakOn "label=\"" rest
  , let labelPart = T.drop (T.length "label=\"") after
  , let label = T.takeWhile (/='\"') labelPart
  ]

-- | Parse edges in DOT: lines containing "->".
--   Example: f0_e0_out -> f0_out;
parseEdges :: Text -> [ (Text, Text) ]
parseEdges input =
  [ (T.strip src, T.strip dst)
  | line <- T.lines input
  , "->" `T.isInfixOf` line
  , let (srcPart, arrowAndRest) = T.breakOn "->" line
  , let src = T.strip srcPart
  , let rest = T.drop 2 arrowAndRest
  , let dstRaw = T.takeWhile (/=';') rest
  , let dst = T.strip dstRaw
  ]

-- | Generate TALM instructions for all nodes (order preserved)
generateInstructions :: [ (Text,Text) ] -> [ (Text,Text) ] -> [Text]
generateInstructions nodes edges =
  map (genNode edges) nodes

-- | Generate a single TALM instruction from a node and its edges
genNode :: [ (Text,Text) ] -> (Text,Text) -> Text
genNode edges (name,label) =
  let ins    = [ src | (src,dst) <- edges, dst == name ]
      outs   = [ dst | (src,dst) <- edges, src == name ]
      opcode = labelToOpcode label
      imm    = labelToImmediate label
      nRes   = labelToResults label outs
      nSrc   = length ins
      dsts   = T.intercalate "," (take nRes outs)
      srcs   = T.intercalate "," ins
      immTxt = maybe "" (" " <>) imm
      arrow  = if nSrc>0 then " <- " <> srcs else ""
  in T.concat [ opcode
              , " " , T.pack (show nRes)
              , " " , T.pack (show nSrc)
              , immTxt
              , " : " , dsts
              , arrow
              ]

-- | Map label to TALM opcode (covers all TALM instructions)
labelToOpcode :: Text -> Text
labelToOpcode lab
  | Just _ <- T.stripPrefix "const:" lab        = "const"
  | Just _ <- T.stripPrefix "addi:" lab        = "addi"
  | Just _ <- T.stripPrefix "subi:" lab        = "subi"
  | Just _ <- T.stripPrefix "muli:" lab        = "muli"
  | Just _ <- T.stripPrefix "divi:" lab        = "divi"
  | lab `elem` ["add","sub","mul","div"]  = lab
  | lab `elem` ["and","or","xor"]          = lab
  | lab == "not"                             = "not"
  | lab `elem` ["eq","neq","lt","leq","gt","geq"] = lab
  | lab `elem` ["steer","merge"]            = lab
  | lab == "split"                            = "split"
  | lab `elem` ["callgroup","callsnd","retsnd","ret","inctag","tagop"] = lab
  | lab `elem` ["super","specsuper","superinstmacro"]  = lab
  | "var:" `T.isPrefixOf` lab                 = "split"
  | "in:"  `T.isPrefixOf` lab                 = "split"
  | otherwise = error $ "Unknown label: " ++ T.unpack lab

-- | Extract immediate operand from label, if any
labelToImmediate :: Text -> Maybe Text
labelToImmediate lab
  | ":" `T.isInfixOf` lab = Just (T.drop 1 $ T.dropWhile (/=':') lab)
  | otherwise              = Nothing

-- | Determine number of results based on label prefix or outgoing edges
labelToResults :: Text -> [Text] -> Int
labelToResults lab outs
  | "const:" `T.isPrefixOf` lab = 1
  | "addi:" `T.isPrefixOf` lab = 1
  | "subi:" `T.isPrefixOf` lab = 1
  | "muli:" `T.isPrefixOf` lab = 1
  | "divi:" `T.isPrefixOf` lab = 1
  | lab == "steer" = 2
  | lab == "split" = length outs
  | "var:" `T.isPrefixOf` lab = length outs   -- variables forwarded
  | "in:" `T.isPrefixOf` lab = length outs    -- input parameters
  | lab `elem` [ "merge"
               , "add","sub","mul","div"
               , "and","or","xor","not"
               , "eq","neq","lt","leq","gt","geq"
               , "super","specsuper","superinstmacro"
               ] = 1
  | lab `elem` [ "callgroup","callsnd","retsnd","ret","inctag","tagop" ] = 0
  | otherwise = error $ "Cannot determine results for label: " ++ T.unpack lab
