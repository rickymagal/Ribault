{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphGen – gera um grafo DOT compatível com Trebuchet/TALM.
module GraphGen (programToDataflowDot) where

import           Data.Char      (isAlphaNum, ord)
import           Data.List      (nub, (\\))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Syntax          (Program(..), Decl(..), Expr(..), BinOperator(..), UnOperator(..), Literal(..), Pattern(..))

-- ═══════════════════ Free-variables helper ════════════════════════════════════
freeVars :: Expr -> [String]
freeVars = nub . go
  where
    go = \case
      Var v           -> [v]
      Lit _           -> []
      Lambda xs e     -> go e \\ xs
      App f x         -> go f ++ go x
      BinOp _ l r     -> go l ++ go r
      UnOp _ v        -> go v
      If c t e        -> go c ++ go t ++ go e
      Let ds e        -> concatMap (\(FunDecl _ ps b) -> go b \\ ps) ds ++ go e
      List es         -> concatMap go es
      Tuple es        -> concatMap go es
      Case s alts     -> go s ++ concatMap (go . snd) alts
      _               -> []

-- ═════════════════════ programa completo → DOT ══════════════════════════
programToDataflowDot :: Program -> Text
programToDataflowDot (Program ds) =
  let topNames = [ T.pack n | FunDecl n _ _ <- ds ]   -- nomes globais
  in  "digraph Dataflow {\n"
   <> "  node [shape=record, fontname=\"Courier\"];\n"
   <> T.concat (zipWith (\i d -> declToDFDot topNames ("f" <> tShow i) d) [0..] ds)
   <> "}\n"

-- ═════════════════════ Declarações (funções) ════════════════════════════
declToDFDot :: [Text] -> Text -> Decl -> Text
declToDFDot globals f (FunDecl _ ps body) = case body of

  ----------------------------------------------------------------------
  -- 1. Literais / lista / tupla → sem retsnd
  ----------------------------------------------------------------------
  Lit _    -> exprToDFDot globals f body
  List _   -> exprToDFDot globals f body
  Tuple _  -> exprToDFDot globals f body

  ----------------------------------------------------------------------
  -- 2. Top-level λ → portas só p/ vars usadas + retsnd
  ----------------------------------------------------------------------
  Lambda xs expr ->
       T.concat
         [ node (f <> "_in_" <> T.pack v) ("in:" <> T.pack v)
         | v <- filter (`elem` freeVars expr) xs
         ]
    <> exprToDFDot globals f expr
    <> node (f <> "_retsnd") "retsnd"
    <> edge (f <> "_out") (f <> "_retsnd")

  ----------------------------------------------------------------------
  -- 3. Função normal (parâmetros explícitos) → in:ps + retsnd
  ----------------------------------------------------------------------
  _ ->
       T.concat
         [ node (f <> "_in_" <> T.pack p) ("in:" <> T.pack p)
         | p <- ps
         ]
    <> exprToDFDot globals f body
    <> node (f <> "_retsnd") "retsnd"
    <> edge (f <> "_out") (f <> "_retsnd")

-- ═════════════════════ Expressões internas ══════════════════════════════
exprToDFDot :: [Text] -> Text -> Expr -> Text
exprToDFDot globals p = \case

  ----------------------------------------------------------------------
  -- Variável
  ----------------------------------------------------------------------
  Var x ->
    let out     = p <> "_out"
        lbl     = "var:" <> T.pack x
        srcPort = currentFun p <> "_in_" <> T.pack x
    in if T.pack x `elem` globals
         then node out lbl                     -- global, sem aresta
         else node out lbl <> edge srcPort out -- local, com aresta

  ----------------------------------------------------------------------
  -- Literais
  ----------------------------------------------------------------------
  Lit lit -> case lit of
    LBool b   -> node (p <> "_out") ("const:" <> if b then "1" else "0")
    LChar c   -> node (p <> "_out") ("const:" <> T.pack (show (ord c)))
    LString s -> buildList globals p [ Lit (LChar c) | c <- s ]
    LInt n    -> node (p <> "_out") ("const:" <> T.pack (show n))
    LFloat f  -> node (p <> "_out") ("const:" <> T.pack (show f))

  ----------------------------------------------------------------------
  -- Lambda aninhada
  ----------------------------------------------------------------------
  Lambda xs e ->
    let lam   = sanitizeName p <> "Lam"
        used  = filter (`elem` freeVars e) xs
        ports = T.concat
          [ node (lam <> "_in_" <> T.pack v) ("in:" <> T.pack v)
          | v <- used
          ]
    in  ports
     <> exprToDFDot globals lam e
     <> node (lam <> "_retsnd") "retsnd"
     <> edge (lam <> "_out") (lam <> "_retsnd")
     <> node p "valtotag"
     <> edge (lam <> "_out") p
     <> edge p (p <> "_out")

  ----------------------------------------------------------------------
  -- If-then-else
  ----------------------------------------------------------------------
  If c t e ->
    let cp  = p <> "_c"; tp = p <> "_t"; ep = p <> "_e"
        st  = p <> "_steer"; phi = p <> "_phi"
    in  exprToDFDot globals cp c
     <> node st "steer" <> edge (cp <> "_out") st
     <> edge st tp <> edge st ep
     <> exprToDFDot globals tp t <> exprToDFDot globals ep e
     <> node phi "super:2"
     <> edge (tp <> "_out") phi <> edge (ep <> "_out") phi
     <> edge phi (p <> "_out")

  ----------------------------------------------------------------------
  -- Aplicação
  ----------------------------------------------------------------------
  App f x ->
    let tag = p <> "_inctag"
        cg  = p <> "_cg"
        fp  = p <> "_f"
        xp  = p <> "_x"
    in  exprToDFDot globals fp f <> exprToDFDot globals xp x
     <> node tag "inctag"   <> edge (fp <> "_out") tag
     <> node cg  "callgroup"<> edge tag cg
     <> node (cg <> "_snd_fun") "callsnd"
     <> edge (fp <> "_out") (cg <> "_snd_fun") <> edge (cg <> "_snd_fun") cg
     <> node (cg <> "_snd_arg") "callsnd"
     <> edge (xp <> "_out") (cg <> "_snd_arg") <> edge (cg <> "_snd_arg") cg
     <> edge cg (p <> "_out")

  ----------------------------------------------------------------------
  -- Binário
  ----------------------------------------------------------------------
  BinOp op l r ->
    let lp = p <> "_l"; rp = p <> "_r"; o = p <> "_out"
    in  exprToDFDot globals lp l <> exprToDFDot globals rp r
     <> node o (binOpToInstr op)
     <> edge (lp <> "_out") o <> edge (rp <> "_out") o

  ----------------------------------------------------------------------
  -- Unário
  ----------------------------------------------------------------------
  UnOp op v ->
    let vp = p <> "_v"; o = p <> "_out"
    in  exprToDFDot globals vp v <> node o (unOpToInstr op) <> edge (vp <> "_out") o

  ----------------------------------------------------------------------
  -- let-in
  ----------------------------------------------------------------------
  Let ds e ->
    let lets = T.concat
          [ declToDFDot globals (p <> "_let" <> tShow i) d
          | (i,d) <- zip [0..] ds
          ]
        inN = p <> "_in"
    in  lets <> exprToDFDot globals inN e <> edge (inN <> "_out") p

  ----------------------------------------------------------------------
  -- Listas / tuplas
  ----------------------------------------------------------------------
  List xs   -> buildList globals p xs
  Tuple xs  -> buildList globals p xs

  ----------------------------------------------------------------------
  -- Pattern-matching especial
  ----------------------------------------------------------------------
  Case scr [ (PList [PVar x,PVar y], _), (PWildcard, bdBad) ] ->
    let sp   = p <> "_scr"; st   = p <> "_steer"
        ok   = p <> "_ok";  bad  = p <> "_bad"
        sp3  = p <> "_split"; vx   = p <> "_vx"
        vy   = p <> "_vy"; addN = p <> "_add"; phi = p <> "_phi"
    in  exprToDFDot globals sp scr
     <> node st "steer" <> edge (sp <> "_out") st
     <> edge st ok
     <> node sp3 "super:3" <> edge ok sp3
     <> node vx ("var:" <> T.pack x) <> edge sp3 vx
     <> node vy ("var:" <> T.pack y) <> edge sp3 vy
     <> node addN "add" <> edge vx addN <> edge vy addN
     <> edge st bad
     <> exprToDFDot globals bad bdBad
     <> node phi "super:2"
     <> edge addN phi <> edge (bad <> "_out") phi
     <> edge phi (p <> "_out")

  ----------------------------------------------------------------------
  -- Case genérico
  ----------------------------------------------------------------------
  Case scr alts ->
    let sp = p <> "_scr"; st = p <> "_steer"; phi = p <> "_phi"
        ns = [ p <> "_alt" <> tShow i | i <- [0..length alts - 1] ]
        mk (n,(_,e)) = edge st n <> exprToDFDot globals n e <> edge (n <> "_out") phi
    in  exprToDFDot globals sp scr
     <> node st "steer" <> edge (sp <> "_out") st
     <> T.concat (map mk (zip ns alts))
     <> node phi "super:2"
     <> edge phi (p <> "_out")

-- ═══════════════════ buildList, helpers DOT & opcodes ════════════════════
buildList :: [Text] -> Text -> [Expr] -> Text
buildList globals pref xs =
  let su  = pref <> "_build"
      els = [ pref <> "_el" <> tShow i | i <- [0..length xs - 1] ]
  in  T.concat (zipWith (exprToDFDot globals) els xs)
   <> node su "super:1"
   <> T.concat [ edge (e <> "_out") su | e <- els ]
   <> edge su (pref <> "_out")

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
