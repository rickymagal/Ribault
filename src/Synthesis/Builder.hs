{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Tradução da AST para a IR de data-flow.
-- | Só depende de 'Syntax' e 'Synthesis.Instruction'.

module Synthesis.Builder (buildProgram) where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (fromMaybe)
import           Data.Foldable               (for_)

import           Syntax
import qualified Synthesis.Instruction      as IR

-- ═════════════════════════════════════════════════════════════════════
-- Estado
-- ═════════════════════════════════════════════════════════════════════

type Build = State BS
data BS = BS { env   :: Map Ident [IR.Signal]  -- variáveis → sinais
             , acc   :: [IR.Inst]              -- instruções (reverso)
             , nextN :: Int }                  -- contador de NodeId

empty :: BS
empty = BS M.empty [] 0

freshId :: Build IR.NodeId
freshId = do s@BS{..} <- get; put s{nextN = nextN + 1}; pure (IR.NodeId nextN)

emit :: IR.Inst -> Build ()
emit i = modify' $ \st -> st{acc = i : acc st}

remember :: Ident -> [IR.Signal] -> Build ()
remember v s = modify' $ \st -> st{env = M.insert v s (env st)}

lookupSig :: Ident -> Build [IR.Signal]
lookupSig v = gets (fromMaybe (err ("unbound var " <> v)) . M.lookup v . env)

-- ═════════════════════════════════════════════════════════════════════
-- API
-- ═════════════════════════════════════════════════════════════════════

buildProgram :: Program -> [IR.Inst]
buildProgram (Program ds) = reverse . acc $ execState (mapM_ topDecl ds) empty

-- ═════════════════════════════════════════════════════════════════════
-- Declarações topo-nível
-- ═════════════════════════════════════════════════════════════════════

topDecl :: Decl -> Build ()
topDecl (FunDecl f ps (Lambda ls body)) =
  topDecl (FunDecl f (ps ++ ls) body)

topDecl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing  -- parâmetros fictícios
  modify' $ \s -> s{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \s -> s{env = M.empty}

-- ═════════════════════════════════════════════════════════════════════
-- Expressões
-- ═════════════════════════════════════════════════════════════════════

expr :: Expr -> Build [IR.Signal]
expr = \case
  Var v        -> lookupSig v
  Lit l        -> lit l
  BinOp o a b  -> binop o a b
  UnOp  o e    -> unop  o e
  If c t f     -> ifExpr c t f
  Let ds e     -> letExpr ds e
  App f x      -> app f x
  Lambda{}     -> err "lambda expression not supported"
  Case scr as  -> caseExpr scr as
  List es      -> listLit es
  Tuple es     -> tupleLit es
  Cons h t     -> consExpr h t

-- ═════════════════════════════════════════════════════════════════════
-- Literais (com placeholder S1 para lista/tupla)
-- ═════════════════════════════════════════════════════════════════════

lit :: Literal -> Build [IR.Signal]
lit = \case
  LInt n    -> scalar (toInteger n) "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LFloat d  -> scalar (floor (d*100)) "float"
  LChar c   -> scalar (toInteger (fromEnum c)) "int"
  LString s -> concat <$> mapM (lit . LChar) s
 where
  scalar v ty = do nid <- freshId
                   emit (IR.InstConst nid v ty)
                   pure [IR.SigInstPort nid 0 Nothing]

listLit, tupleLit :: [Expr] -> Build [IR.Signal]
listLit  es = superPlaceholder es
tupleLit es = superPlaceholder es

superPlaceholder :: [Expr] -> Build [IR.Signal]
superPlaceholder es = do
  sigs <- concat <$> mapM expr es
  nid  <- freshId
  emit (IR.InstSuper nid 1 [sigs] 1 [[]])   -- “S1”
  pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════════════════════════════════════════════════════════════
-- Operadores
-- ═════════════════════════════════════════════════════════════════════

unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do
  v <- expr e
  nid <- freshId
  let (o,i) = case op of { Neg -> ("sub",0); Not -> ("eq",0) }
  emit (IR.InstBinopI nid o "int" i v)
  pure [IR.SigInstPort nid 0 Nothing]

binop :: BinOperator -> Expr -> Expr -> Build [IR.Signal]
binop op l r = do
  la <- expr l; rb <- expr r; nid <- freshId
  let o = case op of
            Add->"+"; Sub->"-"; Mul->"*"; Div->"/"; Mod->"%"
            Eq->"=="; Neq->"!="; Lt->"<"; Le->"<="; Gt->">"; Ge->">="
            And->"&&"; Or->"||"
  emit (IR.InstBinop nid o "int" la rb)
  pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════════════════════════════════════════════════════════════
-- let-in
-- ═════════════════════════════════════════════════════════════════════

letExpr :: [Decl] -> Expr -> Build [IR.Signal]
letExpr binds body = do
  env0 <- gets env
  mapM_ bindLocal binds
  res <- expr body
  modify' $ \s -> s{env = env0}
  pure res
 where
  bindLocal (FunDecl v [] rhs) = expr rhs >>= remember v
  bindLocal d@(FunDecl{})      = topDecl d

-- ═════════════════════════════════════════════════════════════════════
-- if-then-else (gera Steer + avalia ramos)
-- ═════════════════════════════════════════════════════════════════════

ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t f = do
  cond <- expr c
  sid  <- freshId
  emit (IR.InstSteer sid cond [])
  let tGate = [IR.SigSteerPort sid IR.T]
      fGate = [IR.SigSteerPort sid IR.F]
  env0 <- gets env
  remember "__if" tGate
  tSig <- expr t
  modify' $ \s -> s{env = env0}
  remember "__if" fGate
  fSig <- expr f
  modify' $ \s -> s{env = env0}
  pure (tSig ++ fSig)

-- ═════════════════════════════════════════════════════════════════════
-- case / cons – placeholder S1 (não aborta)
-- ═════════════════════════════════════════════════════════════════════

caseExpr :: Expr -> [(Pattern, Expr)] -> Build [IR.Signal]
caseExpr scr _alts = do
  inSig <- expr scr
  nid   <- freshId
  emit (IR.InstSuper nid 1 [inSig] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

consExpr :: Expr -> Expr -> Build [IR.Signal]
consExpr hd tl = do
  hSig <- expr hd
  tSig <- expr tl
  nid  <- freshId
  emit (IR.InstSuper nid 1 [hSig, tSig] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════════════════════════════════════════════════════════════
-- Aplicação 1ª-ordem
-- ═════════════════════════════════════════════════════════════════════

app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  -- desdobra f a b c → (f, [a,b,c])
  let peel (App f a) xs = peel f (a:xs)
      peel other xs     = (other, xs)
      (callee, args)    = peel (App fun arg) []
  fn <- case callee of
          Var v -> pure v
          _     -> err "higher-order application not supported"
  gid <- freshId
  let IR.NodeId n = gid
      grp = "cg" ++ show n
  emit (IR.InstCallGrp gid fn grp)
  for_ (zip [1..] args) $ \(i,a) -> do
        sig <- expr a
        nid <- freshId
        emit (IR.InstCallSnd nid fn grp i sig)
  rnid <- freshId
  let retSig = [IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd rnid fn grp retSig)
  pure retSig

-- ═════════════════════════════════════════════════════════════════════
-- util
-- ═════════════════════════════════════════════════════════════════════

err :: String -> a
err = error . ("Builder: " ++)
