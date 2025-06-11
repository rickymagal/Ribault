{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Codegen — converts DOT (from GraphGen) into FlowASM text.
module Codegen (parseNodes,parseEdges,generateInstructions) where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-------------------------------------------------------------------------------
stripQuotes :: Text -> Text
stripQuotes t | T.length t>=2 && T.head t=='"' && T.last t=='"' = T.init (T.tail t)
              | otherwise = t

parseNodes :: Text -> [(Text,Text)]
parseNodes txt =
  [ ( stripQuotes (T.strip (head parts))
    , T.takeWhile (/='"') (T.drop 1 (T.dropWhile (/='"') (parts!!1))) )
  | line <- T.lines txt, "[label=" `T.isInfixOf` line
  , let parts = T.splitOn "[" line ]

parseEdges :: Text -> [(Text,Text)]
parseEdges txt =
  [ ( stripQuotes (T.strip l)
    , stripQuotes (T.strip (T.takeWhile (/=';') r')) )
  | line <- T.lines txt, "->" `T.isInfixOf` line
  , let (l,r) = T.breakOn "->" line; r' = T.drop 2 r ]

-------------------------------------------------------------------------------
generateInstructions :: [(Text,Text)] -> [(Text,Text)] -> [Text]
generateInstructions ns es = map (gen es) ns

gen :: [(Text,Text)] -> (Text,Text) -> Text
gen es (name,label) =
  let ins  = [s | (s,d)<-es, d==name]
      outs = [d | (s,d)<-es, s==name]
      (op,imm) = case T.splitOn ":" label of
                   [x,y] -> (x,Just y); [x] -> (x,Nothing); _ -> error "lbl"
      opcode = mapOp op

      dst | opcode=="const" = name
          | opcode=="retsnd" = name                      -- dst part
          | opcode=="merge" = name
          | otherwise       = T.intercalate ", " (take (arity opcode outs) outs)

      src = T.intercalate ", " ins
      immTxt = maybe T.empty id imm
      ops = filter (not . T.null) [dst,src,immTxt]

      line | opcode=="ret"    = opcode <> " " <> dst
           | opcode=="retsnd" = opcode <> " " <> dst <> ", " <> src
           | null ops         = opcode
           | otherwise        = opcode <> " " <> T.intercalate ", " ops
  in line

-------------------------------------------------------------------------------
mapOp :: Text -> Text
mapOp t | t `elem` ["var","in"] = "split"
        | t `elem` valid        = t
        | otherwise             = error ("unknown opcode " ++ T.unpack t)
  where
    valid =
      [ "const","add","sub","mul","div","mod","andi","ori","xori"
      , "and","or","xor","not","eq","neq","lt","leq","gt","geq"
      , "addi","subi","muli","divi","steer","merge","split"
      , "callgroup","callsnd","retsnd","ret"
      , "inctag","tagop","super","specsuper","superinstmacro" ]

arity :: Text -> [Text] -> Int
arity "steer" _ = 2
arity "split" xs = length xs
arity "inctag" _ = 1
arity "retsnd" _ = 2  -- dst , src
arity "merge"  _ = 1
arity _ _ = 1
