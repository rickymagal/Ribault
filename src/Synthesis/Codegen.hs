{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Codegen (parseNodes,parseEdges,generateInstructions) where

import           Data.Char      (isAlphaNum, isDigit)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-------------------------------------------------------------------------------
-- DOT  →  listas de nós / arestas
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
-- Geração de assembly TALM
-------------------------------------------------------------------------------
generateInstructions :: [(Text,Text)] -> [(Text,Text)] -> [Text]
generateInstructions ns es = [ asm | n <- ns, let asm = emit es n ]

emit :: [(Text,Text)] -> (Text,Text) -> Text
emit es (name,label) =
  let srcs        = [s | (s,d) <- es, d == name]

      (rawOp,immM) = case T.splitOn ":" label of
                       [x,y] -> (x,Just y)
                       [x]   -> (x,Nothing)
                       _     -> error "label mal-formado"

      ------------------------------------------------------------
      -- var: / in:  →  super-split (ID 3)
      ------------------------------------------------------------
      isSplit  = rawOp == "var" || rawOp == "in"
      opcode0  = if isSplit then "super" else mapOp rawOp
      immTxt0  = if isSplit then "3"      else maybe "0" id immM

      ------------------------------------------------------------
      -- ajuste do imediato
      ------------------------------------------------------------
      isBool    = immTxt0 `elem` ["true","false"]
      isFloat   = T.any (=='.') immTxt0
                  && T.all (\c -> isDigit c || c=='.') immTxt0
      isNumeric = T.all isDigit immTxt0
      isIdent   = T.all (\c -> isAlphaNum c || c=='_') immTxt0

      opcode | opcode0=="const" && isFloat = "fconst"
             | otherwise                   = opcode0

      immTxt | isBool || isNumeric || isFloat || isIdent = immTxt0
              | otherwise                                 = "\"" <> immTxt0 <> "\""

      srcTxt = T.intercalate ", " srcs
      comma  = if T.null srcTxt then "" else ", " <> srcTxt
      nSrc   = T.pack (show (length srcs))
  in case opcode of
       "super"  -> "super "  <> name <> ", " <> immTxt <> ", " <> nSrc <> comma
       "retsnd" -> "retsnd " <> name <> comma
       "ret"    -> "ret "    <> name
       "const"  -> "const "  <> name <> ", " <> immTxt
       "fconst" -> "fconst " <> name <> ", " <> immTxt
       _        -> opcode <> " " <> name <> comma

-------------------------------------------------------------------------------
-- Mapeamento de mnemónicos simples
-------------------------------------------------------------------------------
mapOp :: Text -> Text
mapOp t
  | t == "tagop"             = "inctag"
  | "super" `T.isPrefixOf` t = "super"
  | t `elem` valid           = t
  | otherwise = error ("opcode desconhecido: " ++ T.unpack t)
  where
    valid =
      [ "const","add","sub","mul","div","mod"
      , "andi","ori","xori","and","or","xor","not"
      , "eq","neq","lt","leq","gt","geq"
      , "addi","subi","muli","divi"
      , "steer","merge"
      , "callgroup","callsnd","retsnd","ret"
      , "inctag","valtotag","tagtoval","itag","itagi"
      , "specsuper","superinstmacro"
      ]

arity :: Text -> [Text] -> Int
arity "steer"  _  = 2
arity "inctag" _  = 1
arity "retsnd" _  = 2
arity _        xs = max 1 (length xs)
