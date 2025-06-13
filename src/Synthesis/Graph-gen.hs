{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphGen – gera um grafo DOT compatível com Trebuchet/TALM.
module GraphGen (programToDataflowDot) where

import           Data.Char      (isAlphaNum, ord)
import           Data.List      (nub, (\\))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Syntax          (Program(..), Decl(..), Expr(..)
                                 , BinOperator(..), UnOperator(..)
                                 , Literal(..), Pattern(..), Ident)

-- ─── Free-vars helper ──────────────────────────────────────────────────────
freeVars :: Expr -> [Ident]
freeVars = nub . go where
  go = \case
    Var v         -> [v]
    Lit _         -> []
    Lambda xs e   -> go e \\ xs
    App f x       -> go f ++ go x
    BinOp _ l r   -> go l ++ go r
    UnOp _ v      -> go v
    If c t e      -> go c ++ go t ++ go e
    Let ds e      -> concatMap (\(FunDecl _ ps b) -> go b \\ ps) ds ++ go e
    List es       -> concatMap go es
    Tuple es      -> concatMap go es
    Case s alts   -> go s ++ concatMap (go . snd) alts

-- ─── Entry point: inverter ds antes de gerar ───────────────────────────────
programToDataflowDot :: Program -> Text
programToDataflowDot (Program ds0) =
  let ds       = reverse ds0
      topNames = [ T.pack n | FunDecl n _ _ <- ds ]
      header   = "digraph Dataflow {\n  node [shape=record,fontname=\"Courier\"];\n"
      body     = T.concat $ zipWith (\i d -> declToDFDot topNames ("f" <> tShow i) d)
                                    [0..] ds
      footer   = "}\n"
  in header <> body <> footer

-- ─── Declarações top-level ─────────────────────────────────────────────────
declToDFDot :: [Text] -> Text -> Decl -> Text
declToDFDot globals f (FunDecl _ ps body) = case body of

  -- 1) literal/list/tuple no topo → só expr
  Lit _   -> exprToDFDot globals f body
  List _  -> exprToDFDot globals f body
  Tuple _ -> exprToDFDot globals f body

  -- 2) top-level λ → portas só para vars usadas + retsnd
  Lambda xs expr ->
       T.concat
         [ node (f <> "_in_" <> T.pack v) ("in:" <> T.pack v)
         | v <- filter (`elem` freeVars expr) xs ]
    <> exprToDFDot globals f expr
    <> node (f <> "_retsnd") "retsnd"
    <> edge (f <> "_out") (f <> "_retsnd")

  -- 3) função normal → portas in:ps + retsnd
  _ ->
       T.concat
         [ node (f <> "_in_" <> T.pack p) ("in:" <> T.pack p)
         | p <- ps ]
    <> exprToDFDot globals f body
    <> node (f <> "_retsnd") "retsnd"
    <> edge (f <> "_out") (f <> "_retsnd")

-- ─── Expressões internas ───────────────────────────────────────────────────
exprToDFDot :: [Text] -> Text -> Expr -> Text
exprToDFDot globals p = \case

  -- variável local x global
  Var x ->
    let out     = p <> "_out"
        lbl     = "var:" <> T.pack x
        srcPort = currentFun p <> "_in_" <> T.pack x
    in if T.pack x `elem` globals
         then node out lbl
         else node out lbl <> edge srcPort out

  -- literais
  Lit lit -> case lit of
    LBool b   -> node (p <> "_out") ("const:" <> if b then "1" else "0")
    LChar c   -> node (p <> "_out") ("const:" <> T.pack (show (ord c)))
    LString s -> buildList globals p [ Lit (LChar c) | c <- s ]
    LInt n    -> node (p <> "_out") ("const:" <> T.pack (show n))
    LFloat f  -> node (p <> "_out") ("const:" <> T.pack (show f))

  -- lambda aninhada vira sub-função + valtotag
  Lambda xs e ->
    let lam   = sanitizeName p <> "Lam"
        used  = filter (`elem` freeVars e) xs
        ports = T.concat
          [ node (lam <> "_in_" <> T.pack v) ("in:" <> T.pack v)
          | v <- used ]
    in  ports
     <> exprToDFDot globals lam e
     <> node (lam <> "_retsnd") "retsnd"
     <> edge (lam <> "_out") (lam <> "_retsnd")
     <> node p "valtotag"
     <> edge (lam <> "_out") p
     <> edge p (p <> "_out")

  -- if-then-else
  If c t e ->
    let cp = p <> "_c"; tp = p <> "_t"; ep = p <> "_e"
        st = p <> "_steer"; phi = p <> "_phi"
    in  exprToDFDot globals cp c
     <> node st "steer" <> edge (cp <> "_out") st
     <> edge st tp <> edge st ep
     <> exprToDFDot globals tp t <> exprToDFDot globals ep e
     <> node phi "super:2"
     <> edge (tp <> "_out") phi <> edge (ep <> "_out") phi
     <> edge phi (p <> "_out")

  -- aplicação
  App f x ->
    let tag = p <> "_inctag"; cg = p <> "_cg"
        fp  = p <> "_f";    xp = p <> "_x"
    in  exprToDFDot globals fp f <> exprToDFDot globals xp x
     <> node tag "inctag" <> edge (fp <> "_out") tag
     <> node cg  "callgroup" <> edge tag cg
     <> node (cg <> "_snd_fun") "callsnd"
     <> edge (fp <> "_out") (cg <> "_snd_fun") <> edge (cg <> "_snd_fun") cg
     <> node (cg <> "_snd_arg") "callsnd"
     <> edge (xp <> "_out") (cg <> "_snd_arg") <> edge (cg <> "_snd_arg") cg
     <> edge cg (p <> "_out")

  -- binop / unop
  BinOp op l r ->
    let lp = p <> "_l"; rp = p <> "_r"; o = p <> "_out"
    in  exprToDFDot globals lp l
     <> exprToDFDot globals rp r
     <> node o (binOpToInstr op)
     <> edge (lp <> "_out") o <> edge (rp <> "_out") o

  UnOp op v ->
    let vp = p <> "_v"; o = p <> "_out"
    in  exprToDFDot globals vp v
     <> node o (unOpToInstr op)
     <> edge (vp <> "_out") o

  -- let-in
  Let ds e ->
    let lets = T.concat
          [ declToDFDot globals (p <> "_let" <> tShow i) d
          | (i,d) <- zip [0..] ds ]
        inN = p <> "_in"
    in  lets <> exprToDFDot globals inN e <> edge (inN <> "_out") p

  -- listas/tuplas
  List xs  -> buildList globals p xs
  Tuple xs -> buildList globals p xs

  -- case especial
  Case scr [ (PList [PVar x,PVar y],_), (PWildcard,bd) ] ->
    let sp  = p <> "_scr"; st  = p <> "_steer"; ok = p <> "_ok"; bad = p <> "_bad"
        sp3 = p <> "_split"; vx  = p <> "_vx"; vy  = p <> "_vy"
        aN  = p <> "_add";   phi = p <> "_phi"
    in  exprToDFDot globals sp scr
     <> node st "steer" <> edge (sp <> "_out") st
     <> edge st ok
     <> node sp3 "super:3" <> edge ok sp3
     <> node vx ("var:"<>T.pack x) <> edge sp3 vx
     <> node vy ("var:"<>T.pack y) <> edge sp3 vy
     <> node aN "add" <> edge vx aN <> edge vy aN
     <> edge st bad
     <> exprToDFDot globals bad bd
     <> node phi "super:2"
     <> edge aN phi <> edge (bad<> "_out") phi
     <> edge phi (p<> "_out")

  -- case genérico
  Case scr alts ->
    let sp  = p <> "_scr"; st = p <> "_steer"; phi = p <> "_phi"
        ns  = [ p <> "_alt" <> tShow i | i <- [0..length alts-1] ]
        mk (n,(_,e)) = edge st n <> exprToDFDot globals n e <> edge (n<> "_out") phi
    in  exprToDFDot globals sp scr
     <> node st "steer" <> edge (sp<> "_out") st
     <> T.concat (map mk (zip ns alts))
     <> node phi "super:2"
     <> edge phi (p<> "_out")

-- ─── buildList + dot / opcodes ──────────────────────────────────────────────
buildList :: [Text] -> Text -> [Expr] -> Text
buildList globals pref xs =
  let su  = pref <> "_build"
      els = [ pref <> "_el" <> tShow i | i <- [0..length xs-1] ]
  in  T.concat (zipWith (exprToDFDot globals) els xs)
   <> node su "super:1"
   <> T.concat [ edge (e<> "_out") su | e<-els ]
   <> edge su (pref<> "_out")

currentFun :: Text -> Text
currentFun = T.takeWhile (/= '_')

sanitizeName :: Text -> Text
sanitizeName = T.filter (/= '_')

tShow :: Show a => a -> Text
tShow = T.pack . show

qid :: Text -> Text
qid t | T.all isAlphaNum t = t | otherwise = "\"" <> t <> "\""

node :: Text -> Text -> Text
node a l = "  " <> qid a <> " [label=\"" <> esc l <> "\"];\n"

edge :: Text -> Text -> Text
edge a b = "  " <> qid a <> " -> " <> qid b <> ";\n"

esc :: Text -> Text
esc = T.concatMap (\c -> case c of '"' -> "\\\"" ; '\\' -> "\\\\" ; x -> T.singleton x)

binOpToInstr :: BinOperator -> Text
binOpToInstr = \case
  Add -> "add"; Sub -> "sub"; Mul -> "mul"; Div -> "div"; Mod -> "mod"
  Eq  -> "eq"; Neq -> "neq"; Lt -> "lt"; Le -> "leq"
  Gt  -> "gt"; Ge -> "geq"; And -> "and"; Or -> "or"

unOpToInstr :: UnOperator -> Text
unOpToInstr = \case
  Neg -> "subi"; Not -> "not"
