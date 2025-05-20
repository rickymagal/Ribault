{-# LANGUAGE OverloadedStrings #-}

module GraphGen
  ( programToDataflowDot
  ) where

import Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy    as T
import Syntax

-- | Emite o dot completo do grafo dataflow TALM
programToDataflowDot :: Program -> Text
programToDataflowDot (Program decls) =
     "digraph Dataflow {\n"
  <> "  node [shape=record, fontname=\"Courier\"];\n"
  <> T.concat (zipWith (\i d -> declToDFDot ("f" <> T.pack (show i)) d) [0..] decls)
  <> "}\n"

-- | Cada função vira um subgrafo com entrada, corpo e retorno
declToDFDot :: Text -> Decl -> Text
declToDFDot name (FunDecl _ params body) =
     -- entrada de tag
     node (name <> "_inctag") "inctag"
  <> -- entradas formais
     T.concat
       [ node inN ("in:" <> T.pack p)
       | p <- params
       , let inN = name <> "_in_" <> T.pack p
       ]
  <> -- corpo
     exprToDFDot name body
  <> -- nó final de retorno
     let out = name <> "_out" in
     node (name <> "_retsnd") "retsnd"
  <> edge out (name <> "_retsnd")
  <> node (name <> "_ret") "ret"
  <> edge (name <> "_retsnd") (name <> "_ret")

-- | Gera nós e arestas para cada Expr
exprToDFDot :: Text -> Expr -> Text
exprToDFDot prefix expr =
  case expr of

    Var x ->
      let out = prefix <> "_out" in
      node out ("var:" <> T.pack x)
      <> edge (prefix <> "_in_" <> T.pack x) out

    Lit lit ->
      let out = prefix <> "_out" in
      node out ("const:" <> litToText lit)
      <> edge prefix out

    Lambda _ps e ->
      node prefix "super"
      <> node (prefix <> "_tagop") "tagop"
      <> edge prefix (prefix <> "_tagop")
      <> exprToDFDot (prefix <> "_body") e
      <> edge (prefix <> "_body_out") prefix

    If c t e ->
      let cp = prefix <> "_c"
          tp = prefix <> "_t"
          ep = prefix <> "_e"
          st = prefix <> "_steer"
          mg = prefix <> "_merge"
      in  exprToDFDot cp c
       <> node st "steer"
       <> edge (cp <> "_out") st

       <> exprToDFDot tp t
       <> edge (tp <> "_out") mg

       <> exprToDFDot ep e
       <> edge (ep <> "_out") mg

       <> node mg "merge"
       <> edge st mg

    App f x ->
      let tag = prefix <> "_inctag"
          cg  = prefix <> "_cg"
          fp  = prefix <> "_f"
          xp  = prefix <> "_x"
      in  node tag "inctag"
       <> exprToDFDot fp f
       <> exprToDFDot xp x
       <> node cg "callgroup"
       <> edge tag cg
       <> node (cg <> "_snd1") "callsnd"
       <> edge (fp <> "_out") (cg <> "_snd1")
       <> edge (cg <> "_snd1") cg
       <> node (cg <> "_snd2") "callsnd"
       <> edge (xp <> "_out") (cg <> "_snd2")
       <> edge (cg <> "_snd2") cg
       <> node (cg <> "_retsnd") "retsnd"
       <> edge cg (cg <> "_retsnd")
       <> node (prefix <> "_out") "tagop"
       <> edge (cg <> "_retsnd") (prefix <> "_out")

    BinOp op l r ->
      let lp  = prefix <> "_l"
          rp  = prefix <> "_r"
          out = prefix <> "_out"
      in  exprToDFDot lp l
       <> exprToDFDot rp r
       <> node out (binOpToInstr op)
       <> edge (lp <> "_out") out
       <> edge (rp <> "_out") out

    UnOp op v ->
      let vp  = prefix <> "_v"
          out = prefix <> "_out"
      in  exprToDFDot vp v
       <> node out (unOpToInstr op)
       <> edge (vp <> "_out") out

    Let ds e ->
      let lets = T.concat
            [ declToDFDot (prefix <> "_let" <> T.pack (show i)) d
            | (i,d) <- zip [0::Int ..] ds
            ]
          inN = prefix <> "_in"
      in  lets
       <> exprToDFDot inN e
       <> edge (inN <> "_out") prefix

    List xs ->
      let names = [ prefix <> "_e" <> T.pack (show i) | i <- [0..length xs-1] ]
          out   = prefix <> "_out"
      in  T.concat (zipWith exprToDFDot names xs)
       <> node out "split"
       <> T.concat [ edge (n <> "_out") out | n <- names ]

    Tuple xs ->
      let names = [ prefix <> "_e" <> T.pack (show i) | i <- [0..length xs-1] ]
          out   = prefix <> "_out"
      in  T.concat (zipWith exprToDFDot names xs)
       <> node out "split"
       <> T.concat [ edge (n <> "_out") out | n <- names ]

    Case s alts ->
      let sp = prefix <> "_scrut"
          scrut = exprToDFDot sp s
          brs = T.concat
            [ let bd  = prefix <> "_bd"  <> T.pack (show i)
                  st  = prefix <> "_steer" <> T.pack (show i)
              in  exprToDFDot bd bdExpr
               <> node st "steer"
               <> edge (sp <> "_out") st
               <> edge (bd <> "_out") st
            | (i,(_, bdExpr)) <- zip [0..] alts
            ]
      in  scrut <> brs

  where
    outName = prefix <> "_out"

-- | Converte Literal em texto de imediato
litToText :: Literal -> Text
litToText lit =
  case lit of
    LInt n    -> T.pack (show n)
    LFloat f  -> T.pack (show f)
    LChar  c  -> T.singleton c
    LString s -> "\"" <> T.pack s <> "\""
    LBool  b  -> if b then "true" else "false"

-- | Mapeia BinOperator em instrução TALM
binOpToInstr :: BinOperator -> Text
binOpToInstr op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    Div -> "div"
    Eq  -> "eq"
    Neq -> "neq"
    Lt  -> "lt"
    Le  -> "leq"
    Gt  -> "gt"
    Ge  -> "geq"
    And -> "and"
    Or  -> "or"

-- | Mapeia UnOperator em instrução TALM
unOpToInstr :: UnOperator -> Text
unOpToInstr op =
  case op of
    Neg -> "subi"
    Not -> "not"

-- | Escapa barras e aspas em texto para DOT
escape :: Text -> Text
escape = T.concatMap $ \c ->
  case c of
    '"'  -> "\\\""
    '\\' -> "\\\\"
    x    -> T.singleton x

-- | Helpers para gerar nós e arestas DOT, aplicando escape ao label
node :: Text -> Text -> Text
node n l = "  " <> n <> " [label=\"" <> escape l <> "\"];\n"

edge :: Text -> Text -> Text
edge a b = "  " <> a <> " -> " <> b <> ";\n"
