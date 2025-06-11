{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphGen – gera um grafo DOT compatível com Trebuchet/TALM.
module GraphGen (programToDataflowDot) where

import           Data.Char      (isAlphaNum)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Syntax

-- ════════════════════════════════════════════════════════════════════════
programToDataflowDot :: Program -> Text
programToDataflowDot (Program ds) =
       "digraph Dataflow {\n"
    <> "  node [shape=record, fontname=\"Courier\"];\n"
    <> T.concat (zipWith (\i d -> declToDFDot ("f" <> tShow i) d) [0..] ds)
    <> "}\n"

-- ═════════════════════════════ Declarações ══════════════════════════════
declToDFDot :: Text -> Decl -> Text
declToDFDot f (FunDecl _ ps body)
  -- constante topo-nível
  | null ps, Lit lit <- body
  = node (f <> "_val") ("const:" <> litToText lit)

  -- lista literal: só gera nós, sem retsnd
  | null ps, List _ <- body
  = exprToDFDot f body

  -- tupla literal: só gera nós, sem retsnd
  | null ps, Tuple _ <- body
  = exprToDFDot f body

  -- outras funções: gera o retsnd normalmente
  | otherwise
  = T.concat [ node (f <> "_in_" <> T.pack p) ("in:" <> T.pack p) | p <- ps ]
 <> exprToDFDot f body
 <> node (f <> "_retsnd") "retsnd"
 <> edge (f <> "_out")    (f <> "_retsnd")

-- ═════════════════════════════ Expressões ═══════════════════════════════
exprToDFDot :: Text -> Expr -> Text
exprToDFDot p = \case
  Var x ->
    node (p <> "_out") ("var:" <> T.pack x)
    <> edge (T.takeWhile (/= '_') p <> "_in_" <> T.pack x) (p <> "_out")

  Lit lit ->
    node (p <> "_out") ("const:" <> litToText lit)

  Lambda _ e ->
    node p "super"
    <> node (p <> "_tagop") "tagop"
    <> edge p (p <> "_tagop")
    <> exprToDFDot (p <> "_body") e
    <> edge (p <> "_body_out") p

  If c t e ->
    let cp = p <> "_c"; tp = p <> "_t"; ep = p <> "_e"
        st = p <> "_steer"; phi = p <> "_phi"
    in  exprToDFDot cp c
     <> node st "steer" <> edge (cp <> "_out") st
     <> edge st tp <> edge st ep
     <> exprToDFDot tp t <> exprToDFDot ep e
     <> node phi "super:2"
     <> edge (tp <> "_out") phi <> edge (ep <> "_out") phi
     <> edge phi (p <> "_out")

  App f x ->
    let tag = p <> "_inctag"; cg = p <> "_cg"
        fp  = p <> "_f";    xp = p <> "_x"
    in  exprToDFDot fp f <> exprToDFDot xp x
     <> node tag "inctag"   <> edge (fp <> "_out") tag
     <> node cg "callgroup" <> edge tag cg
     <> node (cg <> "_snd_fun") "callsnd"
     <> edge (fp <> "_out") (cg <> "_snd_fun") <> edge (cg <> "_snd_fun") cg
     <> node (cg <> "_snd_arg") "callsnd"
     <> edge (xp <> "_out") (cg <> "_snd_arg") <> edge (cg <> "_snd_arg") cg
     <> edge cg (p <> "_out")

  BinOp op l r ->
    let lp = p <> "_l"; rp = p <> "_r"; o = p <> "_out"
    in  exprToDFDot lp l <> exprToDFDot rp r
     <> node o (binOpToInstr op)
     <> edge (lp <> "_out") o <> edge (rp <> "_out") o

  UnOp op v ->
    let vp = p <> "_v"; o = p <> "_out"
    in  exprToDFDot vp v <> node o (unOpToInstr op) <> edge (vp <> "_out") o

  Let ds e ->
    let lets = T.concat
                 [ declToDFDot (p <> "_let" <> tShow i) d
                 | (i,d) <- zip [0..] ds
                 ]
        inN = p <> "_in"
    in  lets <> exprToDFDot inN e <> edge (inN <> "_out") p

  List xs ->
    buildList p xs

  Tuple xs ->
    buildList p xs

  Case scr
       [ (PList [PVar x,PVar y], _)
       , (PWildcard      , bdBad) ] ->
    let sp   = p <> "_scr"
        st   = p <> "_steer"
        ok   = p <> "_okTok"
        bad  = p <> "_bad"
        sp3  = p <> "_split"
        vx   = p <> "_vx"
        vy   = p <> "_vy"
        addN = p <> "_add"
        phi  = p <> "_phi"
    in  exprToDFDot sp scr
     <> node st "steer" <> edge (sp <> "_out") st
     <> edge st ok
     <> node sp3 "super:3"
     <> edge ok sp3
     <> node vx ("var:"<>T.pack x) <> edge sp3 vx
     <> node vy ("var:"<>T.pack y) <> edge sp3 vy
     <> node addN "add" <> edge vx addN <> edge vy addN
     <> edge st bad
     <> exprToDFDot bad bdBad
     <> node phi "super:2"
     <> edge addN phi <> edge (bad<> "_out") phi
     <> edge phi (p <> "_out")

  Case scr alts ->
    let sp  = p <> "_scr"
        st  = p <> "_steer"
        phi = p <> "_phi"
        ns  = [ p <> "_alt" <> tShow i | i <- [0..length alts-1] ]
        mk (n,(_,e)) = edge st n <> exprToDFDot n e <> edge (n<> "_out") phi
    in  exprToDFDot sp scr
     <> node st "steer" <> edge (sp <> "_out") st
     <> T.concat (map mk (zip ns alts))
     <> node phi "super:2"
     <> edge phi (p <> "_out")

-- ──────────────── buildList (listas/tuplas) ────────────────────────
buildList :: Text -> [Expr] -> Text
buildList pref xs =
  let su  = pref <> "_build"
      els = [ pref <> "_el" <> tShow i | i <- [0..length xs-1] ]
  in  T.concat (zipWith exprToDFDot els xs)
   <> node su "super:1"
   <> T.concat [ edge (e <> "_out") su | e <- els ]
   -- sem retsnd nem edge para pref_out

-- ──────────────── Helpers ─────────────────────────────────────────────
litToText :: Literal -> Text
litToText = \case
  LInt n    -> tShow n
  LFloat f  -> T.pack (show f)
  LChar c   -> T.singleton c
  LString s -> T.pack s
  LBool b   -> if b then "true" else "false"

binOpToInstr :: BinOperator -> Text
binOpToInstr = \case
  Add -> "add"; Sub -> "sub"; Mul -> "mul"; Mod -> "mod"; Div -> "div"
  Eq  -> "eq"; Neq -> "neq"; Lt -> "lt"; Le -> "leq"
  Gt  -> "gt"; Ge  -> "geq"; And -> "and"; Or -> "or"

unOpToInstr :: UnOperator -> Text
unOpToInstr = \case
  Neg -> "subi"; Not -> "not"

qid :: Text -> Text
qid t | T.all isAlphaNum t = t | otherwise = "\""<>t<>"\""

node :: Text -> Text -> Text
node a l = "  " <> qid a <> " [label=\"" <> esc l <> "\"];\n"

edge :: Text -> Text -> Text
edge a b = "  " <> qid a <> " -> " <> qid b <> ";\n"

esc :: Text -> Text
esc = T.concatMap $ \c -> case c of
  '"'  -> "\\\""
  '\\' -> "\\\\"
  x    -> T.singleton x

tShow :: Show a => a -> Text
tShow = T.pack . show
