{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Synthesis.Builder (buildProgram) where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Map.Strict             (Map)
import           Data.Foldable               (for_)

import           Syntax
import qualified Synthesis.Instruction      as IR
import           Synthesis.DFG

-- ═══════════════════════════════════════
-- estado
-- ═══════════════════════════════════════
type Build = State BS
data BS = BS { env   :: Map Ident [IR.Signal]  -- variáveis → sinais
             , gdfg  :: DFG                   -- grafo em construção
             , nextN :: Int }                 -- gerador de NodeId

initSt :: BS
initSt = BS M.empty emptyDFG 0

freshId :: Build IR.NodeId
freshId = do s@BS{..} <- get; put s{nextN = nextN + 1}; pure (IR.NodeId nextN)

emit :: IR.Inst -> Build ()
emit inst = modify' $ \s ->
  let srcs = [ n | IR.SigInstPort{sigNode=n} <- IR.inputs inst ]
  in s{ gdfg = addNode inst srcs (gdfg s) }

remember :: Ident -> [IR.Signal] -> Build ()
remember v s = modify' $ \st -> st{env = M.insert v s (env st)}

lookupSig :: Ident -> Build [IR.Signal]
lookupSig v = gets (maybe (err ("unbound var " ++ v)) id . M.lookup v . env)

-- ═══════════════════════════════════════
-- interface
-- ═══════════════════════════════════════
buildProgram :: Program -> DFG
buildProgram (Program ds) = gdfg $ execState (mapM_ topDecl ds) initSt

-- ═══════════════════════════════════════
-- declarações topo-nível
-- ═══════════════════════════════════════
topDecl :: Decl -> Build ()
topDecl (FunDecl f ps (Lambda ls body)) =
  topDecl (FunDecl f (ps ++ ls) body)

topDecl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing
  modify' $ \s -> s{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \s -> s{env = M.empty}

-- ═══════════════════════════════════════
-- expressão → sinais
-- ═══════════════════════════════════════
expr :: Expr -> Build [IR.Signal]
expr = \case
  Var v        -> lookupSig v
  Lit l        -> lit l
  BinOp o a b  -> binop o a b
  UnOp  o e    -> unop  o e
  If c t f     -> ifExpr c t f
  Let ds e     -> letExpr ds e
  App f x      -> app f x
  Case s alts  -> caseExpr s alts
  Tuple es     -> tupleExpr es
  List  es     -> listExpr  es
  Cons h t     -> consExpr  h t
  Lambda{}     -> err "lambda literal not supported"

-- ═══════════════════════════════════════
-- literais
-- ═══════════════════════════════════════
lit :: Literal -> Build [IR.Signal]
lit = \case
  LInt n    -> scalar (toInteger n) "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LFloat d  -> scalar (floor (d*100)) "float"
  LChar  c  -> scalar (toInteger (fromEnum c)) "int"
  LString s -> listExpr (map (Lit . LChar) s)
 where
  scalar v ty = do nid <- freshId
                   emit (IR.InstConst nid v ty)
                   pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════════════════════════════
-- tupla
-- ═══════════════════════════════════════
tupleExpr :: [Expr] -> Build [IR.Signal]
tupleExpr es = do
  ss  <- mapM expr es
  nid <- freshId
  emit (IR.InstMkTuple nid (length es) ss)
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════════════════════════════
-- lista  (string usa este mesmo caminho)
-- ═══════════════════════════════════════
listExpr :: [Expr] -> Build [IR.Signal]
listExpr es = do
  elemSigs <- concat <$> mapM expr es   -- ACHATA todas as portas
  nid      <- freshId
  emit (IR.InstSuper nid 1 [elemSigs] 1 [[]])   -- “super1”
  pure [IR.SigInstPort nid 0 Nothing]

consExpr :: Expr -> Expr -> Build [IR.Signal]
consExpr hd tl = do
  sh <- expr hd
  st <- expr tl
  nid <- freshId
  emit (IR.InstCons nid sh st)
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════════════════════════════
-- unário / binário
-- ═══════════════════════════════════════
unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do
  v <- expr e; nid <- freshId
  let (o,i) = case op of { Neg->("sub",0); Not->("eq",0) }
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

-- ═══════════════════════════════════════
-- if-then-else  (Steer T/F)
-- ═══════════════════════════════════════
ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t f = do
  cond <- expr c
  env0 <- gets env
  ts <- expr t; modify' $ \s -> s{env = env0}
  fs <- expr f; modify' $ \s -> s{env = env0}
  sid <- freshId
  emit (IR.InstSteer sid cond (ts ++ fs))
  pure [ IR.SigSteerPort sid IR.T
       , IR.SigSteerPort sid IR.F ]

-- ═══════════════════════════════════════
-- let-in
-- ═══════════════════════════════════════
letExpr :: [Decl] -> Expr -> Build [IR.Signal]
letExpr binds body = do
  env0 <- gets env
  mapM_ bindLocal binds
  res <- expr body
  modify' $ \s -> s{env = env0}
  pure res
 where
  bindLocal (FunDecl v [] rhs) = expr rhs >>= remember v
  bindLocal d@FunDecl{}        = topDecl d

-- ═══════════════════════════════════════
-- case … of   (super1 matcher)
-- ═══════════════════════════════════════
caseExpr :: Expr -> [(Pattern,Expr)] -> Build [IR.Signal]
caseExpr scr _alts = do
  s   <- expr scr
  nid <- freshId
  emit (IR.InstSuper nid 1 [s] 1 [[]])
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════════════════════════════
-- aplicação 1ª-ordem
-- ═══════════════════════════════════════
app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  let peel (App f a) xs = peel f (a:xs); peel o xs = (o,xs)
      (callee,args)    = peel (App fun arg) []
  fn <- case callee of
          Var v -> pure v
          _     -> err "higher-order call not supported"

  gid <- freshId; let IR.NodeId n = gid; grp = "cg" ++ show n
  emit (IR.InstCallGrp gid fn grp)

  for_ (zip [1..] args) $ \(i,a) -> do
    s <- expr a
    nid <- freshId
    emit (IR.InstCallSnd nid fn grp i s)

  rnid <- freshId
  let ret = [IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd rnid fn grp ret)
  pure ret

-- ═══════════════════════════════════════
-- util
-- ═══════════════════════════════════════
err :: String -> a
err = error . ("Builder: " ++)
