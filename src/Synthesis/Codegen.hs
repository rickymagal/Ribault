{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Codegen (parseNodes,parseEdges,generateInstructions) where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

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
generateInstructions :: [(Text,Text)] -> [(Text,Text)] -> [Text]
generateInstructions ns es = map (emit es) ns

emit :: [(Text,Text)] -> (Text,Text) -> Text
emit es (name,label) =
  let srcs = [s | (s,d) <- es, d == name]

      (rawOp,immM) = case T.splitOn ":" label of
                       [x,y] -> (x,Just y)
                       [x]   -> (x,Nothing)
                       _     -> error "label mal-formado"

      opcode = mapOp rawOp
      immTxt = maybe "0" id immM
      nDst   = superDst immM
      nSrc   = length srcs
      srcTxt = T.intercalate ", " srcs
      comma  t = if T.null t then "" else ", " <> t

      line
        -- super instName, imm, nDst, nSrc, src0[,src1…]
        | opcode == "super" =
            T.concat
              [ "super "
              , name, ", "
              , immTxt, ", "
              , T.pack (show nDst), ", "
              , T.pack (show nSrc)
              , comma srcTxt
              ]

        | opcode == "retsnd" = "retsnd " <> name <> comma srcTxt
        | opcode == "ret"    = "ret "    <> name
        | opcode == "const"  = "const "  <> name <> ", " <> immTxt
        | otherwise          = opcode <> " " <> name <> comma srcTxt
  in line

-------------------------------------------------------------------------------
superDst :: Maybe Text -> Int
superDst (Just "3") = 2   -- split2
superDst _          = 1   -- builder / phi

mapOp :: Text -> Text
mapOp t | t `elem` ["var","in"]        = "split"
        | "super" `T.isPrefixOf` t     = "super"
        | t `elem`
          [ "const","add","sub","mul","div","mod","andi","ori","xori"
          , "and","or","xor","not","eq","neq","lt","leq","gt","geq"
          , "addi","subi","muli","divi"
          , "steer","merge","split"
          , "callgroup","callsnd","retsnd","ret"
          , "inctag","tagop" ]         = t
        | otherwise = error ("opcode desconhecido: " ++ T.unpack t)
