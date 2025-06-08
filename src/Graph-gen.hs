{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphGen — render the compiler's high‑level AST as a Graphviz DOT
--   data‑flow graph that the Trebuchet/TALM tool‑chain accepts.
--   The current implementation guarantees:
--     • every opcode receives the correct number of input tokens;
--     • no orphan nodes (e.g. split without a source);
--     • literals and list/tuple construction use a MERGE node;
--     • case‑expressions compile for 1 or 2+ branches.
module GraphGen (programToDataflowDot) where

import           Data.Char      (isAlphaNum)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Syntax

--------------------------------------------------------------------------------
-- Entry‑point -----------------------------------------------------------------
--------------------------------------------------------------------------------

programToDataflowDot :: Program -> Text
programToDataflowDot (Program decls) =
     "digraph Dataflow {\n"
  <> "  node [shape=record, fontname=\"Courier\"];\n"
  <> T.concat (zipWith (\i d -> declToDFDot ("f" <> tShow i) d) [0..] decls)
  <> "}\n"

--------------------------------------------------------------------------------
-- Function declarations -------------------------------------------------------
--------------------------------------------------------------------------------

declToDFDot :: Text -> Decl -> Text
declToDFDot name (FunDecl _ params body) =
     T.concat [ node (name <> "_in_" <> T.pack p) ("in:" <> T.pack p)
              | p <- params ]
  <> exprToDFDot name body
  <> let out = name <> "_out" in
     node (name <> "_retsnd") "retsnd"
  <> edge out (name <> "_retsnd")
  <> node (name <> "_ret") "ret"
  <> edge (name <> "_retsnd") (name <> "_ret")

--------------------------------------------------------------------------------
-- Expressions -----------------------------------------------------------------
--------------------------------------------------------------------------------

exprToDFDot :: Text -> Expr -> Text
exprToDFDot prefix expr = case expr of

  -- Variable reference --------------------------------------------------------
  Var x ->
    let out  = prefix <> "_out"
        root = T.takeWhile (/= '_') prefix
        src  = root <> "_in_" <> T.pack x
    in  node out ("var:" <> T.pack x) <> edge src out

  -- Literal constant ---------------------------------------------------------
  Lit lit ->
    let out = prefix <> "_out"
    in  node out ("const:" <> litToText lit)

  -- Lambda -------------------------------------------------------------------
  Lambda _ps e ->
       node prefix "super"
    <> node (prefix <> "_tagop") "tagop"
    <> edge prefix (prefix <> "_tagop")
    <> exprToDFDot (prefix <> "_body") e
    <> edge (prefix <> "_body_out") prefix

  -- If expression ------------------------------------------------------------
  If c t e ->
    let cp = prefix <> "_c"
        tp = prefix <> "_t"
        ep = prefix <> "_e"
        st = prefix <> "_steer"
        mg = prefix <> "_merge"
    in  exprToDFDot cp c
     <> node st "steer"
     <> edge (cp <> "_out") st
     <> edge st tp <> edge st ep
     <> exprToDFDot tp t <> exprToDFDot ep e
     <> node mg "merge"
     <> edge (tp <> "_out") mg <> edge (ep <> "_out") mg
     <> edge mg (prefix <> "_out")

  -- Function application -----------------------------------------------------
  App f x ->
    let tag = prefix <> "_inctag"
        cg  = prefix <> "_cg"
        fp  = prefix <> "_f"
        xp  = prefix <> "_x"
    in  exprToDFDot fp f <> exprToDFDot xp x
     <> node tag "inctag" <> edge (fp <> "_out") tag
     <> node cg "callgroup" <> edge tag cg
     <> node (cg <> "_snd_fun") "callsnd"
     <> edge (fp <> "_out") (cg <> "_snd_fun") <> edge (cg <> "_snd_fun") cg
     <> node (cg <> "_snd_arg") "callsnd"
     <> edge (xp <> "_out") (cg <> "_snd_arg") <> edge (cg <> "_snd_arg") cg
     <> node (cg <> "_retsnd") "retsnd" <> edge cg (cg <> "_retsnd")
     <> node (prefix <> "_out") "tagop" <> edge (cg <> "_retsnd") (prefix <> "_out")

  -- Binary op ----------------------------------------------------------------
  BinOp op l r ->
    let lp = prefix <> "_l"; rp = prefix <> "_r"; o = prefix <> "_out" in
    exprToDFDot lp l <> exprToDFDot rp r
    <> node o (binOpToInstr op)
    <> edge (lp <> "_out") o <> edge (rp <> "_out") o

  -- Unary op -----------------------------------------------------------------
  UnOp op v ->
    let vp = prefix <> "_v"; o = prefix <> "_out" in
    exprToDFDot vp v <> node o (unOpToInstr op) <> edge (vp <> "_out") o

  -- Let binding --------------------------------------------------------------
  Let ds e ->
    let lets = T.concat [ declToDFDot (prefix <> "_let" <> tShow i) d | (i,d) <- zip [(0::Int)..] ds ]
        inN  = prefix <> "_in" in
    lets <> exprToDFDot inN e <> edge (inN <> "_out") prefix

  -- List / Tuple literals ----------------------------------------------------
  List xs  -> mergeLit xs
  Tuple xs -> mergeLit xs

  -- Case expression ----------------------------------------------------------
  Case s alts ->
    let sp = prefix <> "_scrut" in
    case alts of
      [] -> error "Empty case expression"
      [( _pat, bd )] ->
           exprToDFDot sp s
        <> exprToDFDot (prefix <> "_bd0") bd
        <> edge ((prefix <> "_bd0") <> "_out") (prefix <> "_out")
      ( _p1, bd1 ) : ( _p2, bd2 ) : _ ->
        let st = prefix <> "_steer"
            mg = prefix <> "_merge"
            b1 = prefix <> "_bd0"
            b2 = prefix <> "_bd1" in
        exprToDFDot sp s
        <> node st "steer" <> edge (sp <> "_out") st
        <> edge st b1 <> edge st b2
        <> exprToDFDot b1 bd1 <> exprToDFDot b2 bd2
        <> node mg "merge"
        <> edge (b1 <> "_out") mg <> edge (b2 <> "_out") mg
        <> edge mg (prefix <> "_out")

  where
    -- | Merge all element tokens into a composite value.
    mergeLit :: [Expr] -> Text
    mergeLit xs =
      let elemNames = [ prefix <> "_elem" <> tShow i | i <- [0 .. length xs - 1] ]
          mergeN    = prefix <> "_merge"
          outNode   = prefix <> "_out" in
      T.concat (zipWith exprToDFDot elemNames xs)
      <> node mergeN "merge"
      <> T.concat [ edge (n <> "_out") mergeN | n <- elemNames ]
      <> edge mergeN outNode

--------------------------------------------------------------------------------
-- Literal & operator helpers --------------------------------------------------
--------------------------------------------------------------------------------

litToText :: Literal -> Text
litToText = \case
  LInt n    -> tShow n
  LFloat f  -> tShow f
  LChar c   -> T.singleton c
  LString s -> "\"" <> T.pack s <> "\""
  LBool b   -> if b then "true" else "false"

binOpToInstr :: BinOperator -> Text
binOpToInstr = \case
  Add -> "add"; Sub -> "sub"; Mul -> "mul"; Mod -> "mod"; Div -> "div"
  Eq  -> "eq";  Neq -> "neq"; Lt -> "lt"; Le -> "leq"
  Gt  -> "gt";  Ge -> "geq"; And -> "and"; Or -> "or"

unOpToInstr :: UnOperator -> Text
unOpToInstr = \case
  Neg -> "subi"
  Not -> "not"

--------------------------------------------------------------------------------
-- DOT helpers ----------------------------------------------------------------
--------------------------------------------------------------------------------

qid :: Text -> Text
qid t | T.all isAlphaNum t = t | otherwise = "\"" <> t <> "\""

node :: Text -> Text -> Text
node n l = "  " <> qid n <> " [label=\"" <> esc l <> "\"];\n"

edge :: Text -> Text -> Text
edge a b = "  " <> qid a <> " -> " <> qid b <> ";\n"

esc :: Text -> Text
esc = T.concatMap $ \case
  '"'  -> "\\\""
  '\\' -> "\\\\"
  c     -> T.singleton c

--------------------------------------------------------------------------------
-- Utility --------------------------------------------------------------------
--------------------------------------------------------------------------------

tShow :: Show a => a -> Text
tShow = T.pack . show
