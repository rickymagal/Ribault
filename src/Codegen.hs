{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | *Codegen* ‒ converts a DOT data‑flow graph (as produced by
--   "GraphGen") into TALM assembly accepted by Trebuchet.
--
--   Instruction format produced here follows the official syntax:
--
-- @
--   opcode dst1[, dst2] , src1[, src2] [, imm]
-- @
--
--   * No result/source counters are printed (they are implicit).
--   * Nodes labelled "var" or "in" are mapped to the @split@ opcode, which
--     replicates an input token.
--   * A handful of opcodes (e.g. @inctag@) have zero results.
--
--   The generator is deliberately minimal: it relies on the topology of the
--   graph to decide result arity rather than keeping a separate symbol table.
--   This allows the same code to work for plain data‑flow kernels and for
--   compound "super‑instructions".
module Codegen
  ( parseNodes   -- ^ Extract node names and labels from a DOT file.
  , parseEdges   -- ^ Extract directed edges (source,destination).
  , generateInstructions -- ^ Produce the text for each TALM instruction.
  ) where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

--------------------------------------------------------------------------------
-- DOT parsing helpers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove a single layer of double‑quotes, if present.
stripQuotes :: Text -> Text
stripQuotes t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' =
      T.init (T.tail t)
  | otherwise = t

-- | Return all nodes that contain a @label@ attribute.
--   Each result is a pair @(name,labelText)@.
parseNodes :: Text -> [(Text, Text)]
parseNodes input =
  [ ( stripQuotes $ T.strip (head parts)    -- node name
    , label )                              -- label content "op[:imm]"
  | line <- T.lines input
  , "[label=" `T.isInfixOf` line
  , let parts   = T.splitOn "[" line
        lblPart = T.dropWhile (/= '"') (parts !! 1)
        label   = T.takeWhile (/= '"') (T.drop 1 lblPart)
  ]

-- | Return all directed edges @src -> dst@ (semicolon terminated).
parseEdges :: Text -> [(Text, Text)]
parseEdges input =
  [ ( stripQuotes $ T.strip l
    , stripQuotes $ T.strip $ T.takeWhile (/= ';') r')
  | line <- T.lines input
  , "->" `T.isInfixOf` line
  , let (l, r) = T.breakOn "->" line
        r'     = T.drop 2 r
  ]

--------------------------------------------------------------------------------
-- Instruction generation ------------------------------------------------------
--------------------------------------------------------------------------------

-- | Generate a list of TALM instructions preserving the original node order.
--   The input must be the output of 'parseNodes' and 'parseEdges'.
generateInstructions :: [(Text, Text)]  -- ^ Nodes (name,label)
                     -> [(Text, Text)]  -- ^ Edges (src,dst)
                     -> [Text]
generateInstructions nodes edges = map (genNode edges) nodes

-- | Emit one assembly line for a single node.
--   The implementation keeps only minimal state: *ins* and *outs* lists are
--   obtained by scanning the global edge list.
--
--   The label syntax is either @op@ or @op:imm@.  The latter is used for
--   constants and immediate operations.
--
--   Examples produced:
--
-- @
--   const  a, 42
--   add    sum, x, y
--   steer  tBranch, fBranch, selector
--   inctag                       -- no operands
-- @
--
--   When a node has zero destinations (e.g. @callgroup@) or zero sources
--   (e.g. @const@) the corresponding operand list is omitted.
--   Empty operand lists are not printed.
--
--   Result arity is inferred by 'calcResults'.
-------------------------------------------------------------------------------

genNode :: [(Text, Text)] -> (Text, Text) -> Text
genNode edges (name, label) =
  let ins      = [ s | (s,d) <- edges, d == name ]          -- incoming tokens
      outs     = [ d | (s,d) <- edges, s == name ]          -- produced tokens
      (op, mi) = case T.splitOn ":" label of
                   [x,y] -> (x, Just y)
                   [x]   -> (x, Nothing)
                   _     -> error $ "Malformed label: " ++ T.unpack label
      opcode   = mapOpcode op
      nRes     = calcResults opcode outs
      dstTxt   = T.intercalate ", " (take nRes outs)
      srcTxt   = T.intercalate ", " ins
      immTxt   = maybe T.empty id mi
  in case opcode of
       -- Special pretty‑printing for INCTAG:  dst, [src1, src2, ...]
       "inctag" ->
         let bracketed = if T.null srcTxt then "[]" else "[" <> srcTxt <> "]"
             body      = filter (not . T.null) [dstTxt, bracketed]
         in  opcode <> " " <> T.intercalate ", " body
       -- Default printing – operands separated by comma+space ----------------
       _ ->
         let operands = filter (not . T.null) [dstTxt, srcTxt, immTxt]
             opersTxt = T.intercalate ", " operands
         in  if T.null opersTxt then opcode else opcode <> " " <> opersTxt

--------------------------------------------------------------------------------
-- Opcode mapping --------------------------------------------------------------
--------------------------------------------------------------------------------

mapOpcode :: Text -> Text
mapOpcode op
  | op == "var" || op == "in" = "split"  -- replicate incoming token
  | op `elem` [ "const", "add", "sub", "mul", "div", "mod"
              , "andi", "ori", "xori", "and", "or", "xor"
              , "not", "eq", "neq", "lt", "leq", "gt", "geq"
              , "addi", "subi", "muli", "divi"
              , "steer", "merge", "split"
              , "callgroup", "callsnd", "retsnd", "ret"
              , "inctag", "tagop"
              , "super", "specsuper", "superinstmacro" ] = op
  | otherwise = error $ "Unknown opcode: " ++ T.unpack op

--------------------------------------------------------------------------------
-- Result arity ----------------------------------------------------------------
--------------------------------------------------------------------------------

calcResults :: Text   -- ^ opcode (already mapped)
            -> [Text] -- ^ outgoing edge list
            -> Int
calcResults "steer" _    = 2                     -- true / false arms
calcResults "split" outs = length outs           -- dynamic fan‑out
calcResults "inctag" _   = 1                     -- one retagged token
calcResults op _
  | op `elem` [ "callgroup", "callsnd", "retsnd", "ret" ] = 0
  | otherwise = 1

