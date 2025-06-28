{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

-- | Percorre a AST e gera a lista linear de Inst.
--   Qualquer ‘case’ que contenha PList delega o pattern-matching a uma
--   super-instrução genérica (executada em runtime pelo GHC).

module Synthesis.Builder (buildProgram) where

import           Syntax
import qualified Synthesis.Instruction as IR

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (fromMaybe)
import           Data.Foldable               (for_)

-- ════════════════════════════════════════════════════════
-- Estado
-- ════════════════════════════════════════════════════════
type Build = State BS
data BS = BS { env   :: Map Ident [IR.Signal]
             , acc   :: [IR.Inst]     -- ^ instruções em ordem reversa
             , nextN :: Int }

empty :: BS
empty = BS M.empty [] 0

freshId :: Build IR.NodeId
freshId = do s@BS{..} <- get; put s{nextN = nextN + 1}; pure (IR.NodeId nextN)

emit :: IR.Inst -> Build ()
emit i = modify' $ \st -> st{acc = i : acc st}

remember :: Ident -> [IR.Signal] -> Build ()
remember v s = modify' $ \st -> st{env = M.insert v s (env st)}

lookupSig :: Ident -> Build [IR.Signal]
lookupSig v = gets (fromMaybe (error ("unbound var " <> v)) . M.lookup v . env)

-- boolean literal helper -------------------------------------------------
constBool :: Bool -> Build [IR.Signal]
constBool b = do nid <- freshId
                 emit (IR.InstConst nid (if b then 1 else 0) "int")
                 pure [IR.SigInstPort nid 0 Nothing]

-- ════════════════════════════════════════════════════════
-- Entrada pública
-- ════════════════════════════════════════════════════════
buildProgram :: Program -> [IR.Inst]
buildProgram (Program ds) = reverse . acc $ execState (mapM_ topDecl ds) empty

-- ═════════════ Declarações topo-nível ═══════════════════
topDecl :: Decl -> Build ()
topDecl (FunDecl f ps (Lambda ls body)) =
  topDecl (FunDecl f (ps ++ ls) body)

topDecl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing
  modify' $ \st -> st{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \st -> st{env = M.empty}

-- ═════════════ Expressões ═══════════════════════════════
expr :: Expr -> Build [IR.Signal]
expr = \case
  Var v      -> lookupSig v
  Lit l      -> lit l
  BinOp o a b-> binop o a b
  UnOp  o e  -> unop  o e
  If c t e   -> ifExpr c t e
  Let ds e   -> letExpr ds e
  App f x    -> app f x
  Lambda{}   -> err "lambda expression not supported"
  Case s as  -> caseExpr s as
  List es    -> listLit es
  Tuple es   -> tupleLit es
  Cons x xs   -> consExpr x xs
-- ═════════════ Literais ════════════════════════════════
lit :: Literal -> Build [IR.Signal]
lit = \case
  LFloat d  -> scalar (floor (d*100)) "float"
  LString s -> fmap concat (mapM (lit . LChar) s)
  LInt n    -> scalar (toInteger n) "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LChar c   -> scalar (toInteger (fromEnum c)) "int"
 where
  scalar v ty = do nid <- freshId
                   emit (IR.InstConst nid v ty)
                   pure [IR.SigInstPort nid 0 Nothing]

listLit :: [Expr] -> Build [IR.Signal]
listLit es = do
  sigsPerElem <- mapM expr es
  let allSigs = concat sigsPerElem
  nid <- freshId
  emit (IR.InstSuper nid 1 [allSigs] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

tupleLit :: [Expr] -> Build [IR.Signal]
tupleLit es = do
  allSigs <- concat <$> mapM expr es
  nid     <- freshId
  emit (IR.InstSuper nid 1 [allSigs] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

makeCount :: Int -> Build [IR.Signal]
makeCount n = do nid <- freshId
                 emit (IR.InstConst nid (toInteger n) "int")
                 pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════ Unário / Binário ═════════════════════════
unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do v <- expr e; nid <- freshId
               let (o,i) = case op of { Neg -> ("sub",0); Not -> ("eq",0) }
               emit (IR.InstBinopI nid o "int" i v)
               pure [IR.SigInstPort nid 0 Nothing]

binop :: BinOperator -> Expr -> Expr -> Build [IR.Signal]
binop op l r = do la <- expr l; rb <- expr r; nid <- freshId
                  let o = case op of
                            Add->"+"; Sub->"-"; Mul->"*"; Div->"/"; Mod->"%"
                            Eq->"=="; Neq->"!="; Lt->"<"; Le->"<="; Gt->">"; Ge->">="
                            And->"&&"; Or->"||"
                  emit (IR.InstBinop nid o "int" la rb)
                  pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════ If (Steer) ═══════════════════════════════
ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t e = do cond <- expr c; sid <- freshId
                  emit (IR.InstSteer sid cond [])
                  (++) <$> expr t <*> expr e

-- ═════════════ Let ═════════════════════════════════════
letExpr :: [Decl] -> Expr -> Build [IR.Signal]
letExpr binds body = do
  env0 <- gets env; mapM_ bindLocal binds
  res <- expr body
  modify' $ \st -> st{env = env0}; pure res
 where
  bindLocal (FunDecl v [] rhs) = expr rhs >>= remember v
  bindLocal d@(FunDecl{})      = topDecl d

-- ═════════════ Case =====================================================

caseExpr :: Expr -> [(Pattern, Expr)] -> Build [IR.Signal]
caseExpr scr alts
  | any (needsSuper . fst) alts = superMatch scr           -- ★ envia p/ super
  | otherwise                   = expr scr >>= chain alts  --         steer
  where
    -- Padrões considerados “estruturais” (delegamos ao runtime/GHC)
    needsSuper :: Pattern -> Bool
    needsSuper = \case
        PList  _ -> True   -- lista  [x, y, …]
        PTuple _ -> True   -- tupla  (a, b, …)
        PCons _ _ -> True
        -- acrescente outros conforme necessário
        _        -> False

    -----------------------------------------------------------------
    -- Caminho antigo: cadeia de Steers para literais ou variáveis
    -----------------------------------------------------------------
    chain :: [(Pattern,Expr)] -> [IR.Signal] -> Build [IR.Signal]
    chain [] _ = err "non-exhaustive patterns"
    chain ((p,r):rest) scrSig = do
      cond <- patCond p scrSig
      sid  <- freshId
      emit (IR.InstSteer sid cond [])
      env0 <- gets env
      bindPat p scrSig
      tSig <- expr r
      modify' $ \st -> st{env = env0}
      eSig <- if null rest then constBool False else chain rest scrSig
      pure (tSig ++ eSig)

    -----------------------------------------------------------------
    -- Caminho novo: gera uma InstSuper genérica
    -----------------------------------------------------------------
    superMatch :: Expr -> Build [IR.Signal]
    superMatch source = do
      inSig <- expr source
      nid   <- freshId
      -- superNum=1, 1 entrada, 1 saída, sem propriedades paralelas
      emit (IR.InstSuper nid 1 [inSig] 1 [[]])
      pure [IR.SigInstPort nid 0 Nothing]


consExpr :: Expr -> Expr -> Build [IR.Signal]
consExpr hd tl = do
  hdSig <- expr hd
  tlSig <- expr tl
  nid   <- freshId
  -- Super-instrução genérica de cons: agora passa hdSig e tlSig COMO entrada
  emit (IR.InstSuper nid 1 [hdSig, tlSig] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

-- ---- Super para pattern-matching de listas -----------------------------
listCaseViaSuper :: Expr -> Build [IR.Signal]
listCaseViaSuper scr = do
  inSig <- expr scr
  nid   <- freshId
  emit (IR.InstSuper nid 1 [inSig] 1 [[]])   -- uma entrada, uma saída
  pure [IR.SigInstPort nid 0 Nothing]

-- helpers ----------------------------------------------------------------
patCond :: Pattern -> [IR.Signal] -> Build [IR.Signal]
patCond pat scrS = case pat of
  PWildcard -> constBool True
  PVar _    -> constBool True
  PLit l    -> cmpLit l
  PTuple _  -> constBool True
  PList ps  -> cmpLen (length ps)
 where
  cmpLit l  = do s <- lit l; nid <- freshId
                 emit (IR.InstBinop nid "==" "int" scrS s)
                 pure [IR.SigInstPort nid 0 Nothing]
  cmpLen k  = do kn <- freshId
                 emit (IR.InstConst kn (toInteger k) "int")
                 cn <- freshId
                 emit (IR.InstBinop cn "==" "int" scrS [IR.SigInstPort kn 0 Nothing])
                 pure [IR.SigInstPort cn 0 Nothing]

bindPat :: Pattern -> [IR.Signal] -> Build ()
bindPat pat src = case pat of
  PVar x     -> remember x src
  PTuple ps  -> sequence_ [ bindPat p [src !! i] | (p,i) <- zip ps [0..] ]
  PList ps   -> sequence_ [ bindPat p src | p <- ps, not (isWild p) ]
  PCons p ps  -> sequence_ [ bindPat subPat src | subPat <- [p, ps], not (isWild subPat) ]
  _          -> pure ()
 where isWild PWildcard = True; isWild _ = False

-- ═════════════ Aplicação 1ª-ordem ═════════════════════
app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  let peel (App f a) xs = peel f (a:xs); peel other xs = (other,xs)
      (callee,args)     = peel (App fun arg) []
  fn <- case callee of Var v -> pure v; _ -> err "HO call"
  gid <- freshId; let IR.NodeId n = gid; grp = "cg" <> show n
  emit (IR.InstCallGrp gid fn grp)
  for_ (zip [1..] args) $ \(i,a) -> expr a >>= \s -> do
        nid <- freshId; emit (IR.InstCallSnd nid fn grp i s)
  rnid <- freshId; let ret = [IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd rnid fn grp ret)
  pure ret

-- ═════════════ util =====================================================
err :: String -> a
err = error . ("Builder: "++)
