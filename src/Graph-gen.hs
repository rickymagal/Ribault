{-# LANGUAGE OverloadedStrings #-}

-- | GraphGen provides a renderer from the internal AST to a DOT-formatted
-- dataflow graph following the TALM model.
--
-- The generated graph can be visualized with Graphviz or further processed
-- by a code generator to emit TALM assembly.
module GraphGen
  ( programToDataflowDot
  ) where

import Data.Text.Lazy     (Text)
import qualified Data.Text.Lazy    as T
import Syntax

-- | Render an entire program as a DOT graph in the TALM dataflow style.
--
-- Each top-level function declaration is numbered and converted into a
-- subgraph containing nodes for parameter inputs, internal computations,
-- and return outputs.  The result is a fully connected dataflow graph
-- in Graphviz DOT format.
programToDataflowDot :: Program -> Text
programToDataflowDot (Program decls) =
     "digraph Dataflow {\n"
  <> "  node [shape=record, fontname=\"Courier\"];\n"
  <> T.concat (zipWith (\i d -> declToDFDot ("f" <> T.pack (show i)) d) [0..] decls)
  <> "}\n"

-- | Convert a single function declaration into its DOT subgraph.
--
-- The naming convention is:
--
--   * \<name\>_inctag  : instance tag increment node  
--   * \<name\>_in_p    : input node for parameter p  
--   * ...             : computation nodes for the function body  
--   * \<name\>_retsnd : return send node  
--   * \<name\>_ret    : return completion node  
declToDFDot :: Text -> Decl -> Text
declToDFDot name (FunDecl _ params body) =
     -- instance tag increment
     node (name <> "_inctag") "inctag"
  <> -- formal parameter inputs
     T.concat
       [ node inN ("in:" <> T.pack p)
       | p <- params
       , let inN = name <> "_in_" <> T.pack p
       ]
  <> -- generate body graph
     exprToDFDot name body
  <> -- return sequence
     let out = name <> "_out" in
     node (name <> "_retsnd") "retsnd"
  <> edge out (name <> "_retsnd")
  <> node (name <> "_ret") "ret"
  <> edge (name <> "_retsnd") (name <> "_ret")

-- | Recursively render expressions into dataflow DOT nodes and edges.
--
-- Uses a naming scheme based on the given prefix for each sub-expression.
exprToDFDot :: Text -> Expr -> Text
exprToDFDot prefix expr =
  case expr of

    -- variable usage: connect from its input node
    Var x ->
      let out = prefix <> "_out" in
      node out ("var:" <> T.pack x)
      <> edge (prefix <> "_in_" <> T.pack x) out

    -- literal constant: generate const node
    Lit lit ->
      let out = prefix <> "_out" in
      node out ("const:" <> litToText lit)
      <> edge prefix out

    -- lambda abstraction: model as a super-instruction with tag ops
    Lambda _ps e ->
      node prefix "super"
      <> node (prefix <> "_tagop") "tagop"
      <> edge prefix (prefix <> "_tagop")
      <> exprToDFDot (prefix <> "_body") e
      <> edge (prefix <> "_body_out") prefix

    -- conditional: steer and merge pattern
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

    -- function application: callgroup with send/return semantics
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

    -- binary operation: two inputs → one output
    BinOp op l r ->
      let lp  = prefix <> "_l"
          rp  = prefix <> "_r"
          out = prefix <> "_out"
      in  exprToDFDot lp l
       <> exprToDFDot rp r
       <> node out (binOpToInstr op)
       <> edge (lp <> "_out") out
       <> edge (rp <> "_out") out

    -- unary operation: one input → one output
    UnOp op v ->
      let vp  = prefix <> "_v"
          out = prefix <> "_out"
      in  exprToDFDot vp v
       <> node out (unOpToInstr op)
       <> edge (vp <> "_out") out

    -- let-binding: flatten declarations then connect to body
    Let ds e ->
      let lets = T.concat
            [ declToDFDot (prefix <> "_let" <> T.pack (show i)) d
            | (i,d) <- zip [0::Int ..] ds
            ]
          inN = prefix <> "_in"
      in  lets
       <> exprToDFDot inN e
       <> edge (inN <> "_out") prefix

    -- list literal: split all elements to a single output
    List xs ->
      let names = [ prefix <> "_e" <> T.pack (show i) | i <- [0..length xs-1] ]
          out   = prefix <> "_out"
      in  T.concat (zipWith exprToDFDot names xs)
       <> node out "split"
       <> T.concat [ edge (n <> "_out") out | n <- names ]

    -- tuple literal: same as list but different tag
    Tuple xs ->
      let names = [ prefix <> "_e" <> T.pack (show i) | i <- [0..length xs-1] ]
          out   = prefix <> "_out"
      in  T.concat (zipWith exprToDFDot names xs)
       <> node out "split"
       <> T.concat [ edge (n <> "_out") out | n <- names ]

    -- pattern match: generate steer nodes for each branch
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

-- | Convert a literal into its text representation for 'const' nodes.
litToText :: Literal -> Text
litToText lit =
  case lit of
    LInt n    -> T.pack (show n)
    LFloat f  -> T.pack (show f)
    LChar  c  -> T.singleton c
    LString s -> "\"" <> T.pack s <> "\""
    LBool  b  -> if b then "true" else "false"

-- | Map a binary operator to its TALM instruction mnemonic.
binOpToInstr :: BinOperator -> Text
binOpToInstr op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    Mod -> "mod"
    Div -> "div"
    Eq  -> "eq"
    Neq -> "neq"
    Lt  -> "lt"
    Le  -> "leq"
    Gt  -> "gt"
    Ge  -> "geq"
    And -> "and"
    Or  -> "or"

-- | Map a unary operator to its TALM instruction mnemonic.
unOpToInstr :: UnOperator -> Text
unOpToInstr op =
  case op of
    Neg -> "subi"
    Not -> "not"

-- | Escape quotes and backslashes in labels for valid DOT syntax.
escape :: Text -> Text
escape = T.concatMap $ \c ->
  case c of
    '"'  -> "\\\""
    '\\' -> "\\\\"
    x    -> T.singleton x

-- | Render a DOT node with a given name and label.
node :: Text -> Text -> Text
node n l = "  " <> n <> " [label=\"" <> escape l <> "\"];\n"

-- | Render a DOT edge from node A to node B.
edge :: Text -> Text -> Text
edge a b = "  " <> a <> " -> " <> b <> ";\n"
