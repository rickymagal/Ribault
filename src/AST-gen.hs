-- AST-gen.hs
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

-- | Módulo com função para geração de grafo AST em DOT.
module ASTGen
  ( programToDot
  ) where

import Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy    as T
import Syntax (Program(..), Decl(..), Expr(..), Pattern(..))

-- | Gera um grafo AST em formato DOT a partir de um AST de Programa.
programToDot :: Program -> Text
programToDot (Program decls) =
     "digraph AST {\n"
  <> "  node [shape=box, fontname=\"Courier\"];\n"
  <> T.concat (zipWith (\i d -> declToDot ("decl" <> T.pack (show i)) d) [0..] decls)
  <> "}\n"

-- | Renderiza uma declaração como subgrafo DOT.
declToDot :: Text -> Decl -> Text
declToDot name (FunDecl f ps b) =
    let lbl   = "FunDecl\n" <> T.pack f <> "(" <> T.intercalate "," (map T.pack ps) <> ")"
        bodyN = name <> "_body"
    in  node name lbl
     <> node bodyN (exprLabel b)
     <> edge name bodyN
     <> exprToDot bodyN b

-- | Rótulo legível para cada expressão.
exprLabel :: Expr -> Text
exprLabel = \case
  Var x        -> "Var\n"   <> T.pack x
  Lit _        -> "Lit"
  Lambda ps _  -> "Lambda(" <> T.intercalate "," (map T.pack ps) <> ")"
  If{}         -> "If"
  Case{}       -> "Case"
  Let{}        -> "Let"
  App{}        -> "App"
  BinOp op _ _ -> "BinOp\n" <> T.pack (show op)
  UnOp op _    -> "UnOp\n" <> T.pack (show op)
  List{}       -> "List"
  Tuple{}      -> "Tuple"

-- | Desenha recursivamente crianças de uma expressão.
exprToDot :: Text -> Expr -> Text
exprToDot prefix expr =
  case expr of
    Var{}    -> ""
    Lit{}    -> ""
    Lambda _ b -> child prefix "body" b

    If c t e -> children prefix ["cond","then","else"] [c,t,e]

    Case s alts ->
         child prefix "scrut" s
      <> T.concat [ altToDot prefix i pat bd
                  | (i,(pat,bd)) <- zip [0..] alts ]

    Let ds e ->
         T.concat [ declToDot (prefix <> "_let" <> T.pack (show i)) d
                  | (i,d) <- zip [0..] ds ]
      <> child prefix "in" e

    App f x -> children prefix ["fun","arg"] [f,x]

    BinOp _ l r -> children prefix ["l","r"] [l,r]

    UnOp _ x -> child prefix "arg" x

    List xs -> T.concat [ child prefix (T.pack $ "e"++show i) x
                         | (i,x) <- zip [0..] xs ]

    Tuple xs -> T.concat [ child prefix (T.pack $ "e"++show i) x
                          | (i,x) <- zip [0..] xs ]
  where
    child p role e =
      let n = p <> "_" <> role
      in  node n (exprLabel e)
       <> edge p n
       <> exprToDot n e

    children p rs es = T.concat $ zipWith (\r e -> child p r e) rs es

-- | Renderiza alternativa de case.
altToDot :: Text -> Int -> Pattern -> Expr -> Text
altToDot prefix i pat bd =
  let pn = prefix <> "_pat" <> T.pack (show i)
      bn = prefix <> "_bd"  <> T.pack (show i)
  in  node pn (patternLabel pat)
   <> edge prefix pn
   <> node bn "AltBody"
   <> edge pn bn
   <> exprToDot bn bd

-- | Rótulo de padrão.
patternLabel :: Pattern -> Text
patternLabel = \case
  PWildcard   -> "_"
  PVar x      -> T.pack x
  PLit _      -> "LitPat"
  PList _     -> "ListPat"
  PTuple _    -> "TuplePat"

-- | Define um nó DOT.
node :: Text -> Text -> Text
node name label =
  "  " <> name <> " [label=\"" <> label <> "\"];\n"

-- | Define uma aresta DOT.
edge :: Text -> Text -> Text
edge from to =
  "  " <> from <> " -> " <> to <> ";\n"
