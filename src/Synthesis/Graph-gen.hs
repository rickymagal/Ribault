{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphGen reduzido: só literais, listas e tuplas.
module GraphGen (programToDataflowDot) where

import           Data.Char      (isAlphaNum, ord)
import           Data.Char      (ord)
import           Data.List      ((\\))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Syntax          (Program(..), Decl(..), Expr(..), Literal(..))

-- | Ponto de entrada
programToDataflowDot :: Program -> Text
programToDataflowDot (Program ds) =
  let header = "digraph Dataflow {\n  node [shape=record,fontname=\"Courier\"];\n"
      body   = T.concat $ zipWith declToDFDot [0::Int ..] ds
      footer = "}\n"
  in header <> body <> footer

-- | Gera subgrafo apenas para FunDecls sem parâmetros e corpo Literal/List/Tuple
declToDFDot :: Int -> Decl -> Text
declToDFDot i (FunDecl _ [] body) =
  let p       = "f" <> T.pack (show i)
      dotBody = exprToDFDot p body
      ret     = node (p <> "_retsnd") "retsnd"
      linkOut = edge (p <> "_out") (p <> "_retsnd")
  in dotBody <> ret <> linkOut
declToDFDot _ _ = ""  -- ignora todo o resto

-- | Só trata literais, listas e tuplas
exprToDFDot :: Text -> Expr -> Text
exprToDFDot p = \case
  Lit lit -> case lit of
    LBool b   -> node (p <> "_out") ("const:" <> if b then "1" else "0")
    LChar c   -> node (p <> "_out") ("const:" <> T.pack (show (ord c)))
    LInt n    -> node (p <> "_out") ("const:" <> T.pack (show n))
    LFloat f  -> node (p <> "_out") ("const:" <> T.pack (show f))
    LString s -> buildList p [Lit (LChar c) | c <- s]

  List xs  -> buildList p xs
  Tuple xs -> buildList p xs

  _ -> ""  -- ignora todo o resto

-- | Usa ListBuild em vez de super:1
buildList :: Text -> [Expr] -> Text
buildList pref xs =
  let els    = [ pref <> "_el" <> T.pack (show i) | i <- [0..length xs-1] ]
      bodies = mconcat [ exprToDFDot el x | (el,x) <- zip els xs ]
      build  = node pref "ListBuild"
      feeds  = mconcat [ edge (el <> "_out") pref | el <- els ]
      out    = edge pref (pref <> "_out")
  in bodies <> build <> feeds <> out

-- ───── Helpers DOT ─────────────────────────────────────────────────────────

qid :: Text -> Text
qid t | T.all isAlphaNum t = t
      | otherwise          = "\"" <> t <> "\""

esc :: Text -> Text
esc = T.concatMap (\c -> case c of
    '"'  -> "\\\"" ; '\\' -> "\\\\" ; x -> T.singleton x)

node :: Text -> Text -> Text
node a l = "  " <> qid a <> " [label=\"" <> esc l <> "\"];\n"

edge :: Text -> Text -> Text
edge a b = "  " <> qid a <> " -> " <> qid b <> ";\n"
