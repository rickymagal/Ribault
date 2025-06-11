{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Codegen (parseNodes,parseEdges,generateInstructions) where

import           Data.Char        (isDigit)
import           Data.Text.Lazy   (Text)
import qualified Data.Text.Lazy   as T

-------------------------------------------------------------------------------
-- DOT helpers
-------------------------------------------------------------------------------
stripQuotes :: Text -> Text
stripQuotes t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' = T.init (T.tail t)
  | otherwise                                             = t

parseNodes :: Text -> [(Text,Text)]
parseNodes txt =
  [ ( stripQuotes (T.strip (head p))
    , T.takeWhile (/='"') (T.drop 1 (T.dropWhile (/='"') (p !! 1))) )
  | line <- T.lines txt
  , "[label=" `T.isInfixOf` line
  , let p = T.splitOn "[" line
  ]

parseEdges :: Text -> [(Text,Text)]
parseEdges txt =
  [ ( stripQuotes (T.strip l)
    , stripQuotes (T.strip (T.takeWhile (/=';') r')) )
  | line <- T.lines txt
  , "->" `T.isInfixOf` line
  , let (l,r) = T.breakOn "->" line; r' = T.drop 2 r
  ]

-------------------------------------------------------------------------------
-- geração de instruções
-------------------------------------------------------------------------------
generateInstructions :: [(Text,Text)] -> [(Text,Text)] -> [Text]
generateInstructions ns es = map (emit es) ns

emit :: [(Text,Text)] -> (Text,Text) -> Text
emit es (name,label) =
  let srcs      = [s | (s,d) <- es, d == name]
      outs      = [d | (s,d) <- es, s == name]

      (rawOp,immM) = case T.splitOn ":" label of
                       [x,y] -> (x,Just y)
                       [x]   -> (x,Nothing)
                       _     -> error "label mal-formado"

      opcode0  = mapOp rawOp
      immTxt0  = maybe "0" id immM

      isBool   = immTxt0 == "true" || immTxt0 == "false"
      isFloat  = T.any (=='.') immTxt0 && T.all (\c -> isDigit c || c == '.') immTxt0
      isNumeric = T.all isDigit immTxt0

      -- escolha de opcode para constantes
      opcode
        | opcode0 == "const" && isFloat   = "fconst"
        | otherwise                        = opcode0

      -- format imm: aspas só para strings
      immTxt
        | isBool || isNumeric || isFloat = immTxt0
        | otherwise                       = "\"" <> immTxt0 <> "\""

      nSrc   = length srcs
      srcTxt = T.intercalate ", " srcs
      dstTxt = T.intercalate ", " (take (arity opcode outs) outs)
      comma  t = if T.null t then "" else ", " <> t

  in case opcode of
       -- super instName, imm, nSrc, src0[…]
       "super"  ->
         T.concat
           [ "super "
           , name, ", "
           , immTxt, ", "
           , T.pack (show nSrc)
           , comma srcTxt
           ]

       -- retsnd dst, src
       "retsnd" -> "retsnd " <> name <> comma srcTxt

       -- ret dst
       "ret"    -> "ret "    <> name

       -- const / fconst dst, imm
       "const"  -> "const "  <> name <> ", " <> immTxt
       "fconst" -> "fconst " <> name <> ", " <> immTxt

       -- demais
       _        -> opcode <> " " <> dstTxt <> comma srcTxt

-------------------------------------------------------------------------------
-- opcode map & aridade
-------------------------------------------------------------------------------
mapOp :: Text -> Text
mapOp t
  | t `elem` ["var","in"]       = "split"
  | "super" `T.isPrefixOf` t    = "super"
  | t `elem` valid              = t
  | otherwise                   = error ("opcode desconhecido: " ++ T.unpack t)
  where
    valid =
      [ "const","add","sub","mul","div","mod"
      , "andi","ori","xori","and","or","xor","not"
      , "eq","neq","lt","leq","gt","geq"
      , "addi","subi","muli","divi"
      , "steer","merge","split"
      , "callgroup","callsnd","retsnd","ret"
      , "inctag","tagop","specsuper","superinstmacro"
      ]

arity :: Text -> [Text] -> Int
arity "steer" _   = 2
arity "split" xs  = length xs
arity "inctag" _  = 1
arity "retsnd" _  = 2
arity _ _         = 1
