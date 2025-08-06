{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Synthesis.Builder (buildProgram) where

import           Prelude                 hiding (id)
import           Control.Monad.State.Strict
import           Control.Monad           (foldM, forM, zipWithM_)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL

import           Types
import           Node
import           Port
import           Unique
import qualified Syntax                  as S

data BState = BState
  { g      :: !(DGraph DNode)
  , env    :: !(Map S.Ident Port)
  , funEnv :: !(Map S.Ident ([S.Ident], S.Expr))  -- nome da função -> (params, body)
  }

type Builder = StateT BState Unique
emptyB :: BState
emptyB = BState emptyGraph Map.empty Map.empty

freshNid :: Builder NodeId
freshNid = lift freshId

addN :: DNode -> Builder ()
addN n = modify' $ \s -> s { g = addNode (nId n) n (g s) }

addE :: Edge -> Builder ()
addE e = modify' $ \s -> s { g = addEdge e (g s) }

bindVar :: S.Ident -> Port -> Builder ()
bindVar x p = modify' $ \s -> s { env = Map.insert x p (env s) }

lookupVar :: S.Ident -> Builder Port
lookupVar x =
  gets (Map.lookup x . env) >>= maybe (error ("[Builder] unbound: " <> x)) pure

-- Para função, busca na tabela de funções
lookupFun :: S.Ident -> Builder ([S.Ident], S.Expr)
lookupFun x =
  gets (Map.lookup x . funEnv) >>= maybe (error ("[Builder] undefined function: " <> x)) pure

litNode :: Literal -> Builder Port
litNode lit = do n <- freshNid
                 addN (InstConst n lit)
                 pure (InstPort n outPort)

boolConst :: Bool -> Builder Port
boolConst = litNode . LBool

-----------------------------------------------------------
-- EXPRESSÕES
-----------------------------------------------------------
compileExpr :: S.Expr -> Builder Port
compileExpr = \case
  S.Var x         -> lookupVar x
  S.Lit l         -> litNode (convLit l)

  S.Lambda params body -> do
    -- não gera nó, só põe no env se algum dia usar função anônima
    old <- gets env
    parList <- forM params $ \p -> do
                 pid <- freshNid
                 pure (p, InstPort pid "out0")
    modify' (\s -> s { env = Map.union (Map.fromList parList) (env s)})
    pBody <- compileExpr body
    modify' (\s -> s { env = old })
    pure pBody

  S.BinOp o a b   -> binop (convOp o) a b
  S.UnOp  o e     -> unop  (convUn o) e
  S.Cons h t      -> binop BCons h t
  S.If c t e      -> compileIf c t e
  S.Tuple es      -> compileTuple es
  S.List  es      -> compileList es

  S.App f a       -> compileFuncApp f [a]
  S.Let ds e      -> compileLet ds e
  S.Case s as     -> compileCase s as
 where
  binop bop a b = do
    pL <- compileExpr a; pR <- compileExpr b; n <- freshNid
    addN (InstBinop n bop (portNode pL) (portNode pR))
    addE (pL --> InstPort n "lhs"); addE (pR --> InstPort n "rhs")
    pure (InstPort n outPort)
 unop u e = do
  p <- compileExpr e; n <- freshNid
  addN (InstUnary n u (portNode p))
  addE (p --> InstPort n "arg")
  pure (InstPort n outPort)


-----------------------------------------------------------
-- IF / TUPLE / LIST
-----------------------------------------------------------
compileIf :: S.Expr -> S.Expr -> S.Expr -> Builder Port
compileIf c t e = do
  pC <- compileExpr c; s <- freshNid
  addN (InstSteer s (portNode pC))
  addE (pC --> InstPort s "pred")
  pT <- compileExpr t; pF <- compileExpr e
  addE (SteerPort s truePort  --> pT)
  addE (SteerPort s falsePort --> pF)
  pure pT

compileTuple :: [S.Expr] -> Builder Port
compileTuple es = do
  ps <- mapM compileExpr es
  n  <- freshNid
  addN (InstTuple n (map portNode ps))
  zipWithM_ (\i p -> addE (p --> InstPort n ("f"<>show i))) [0..] ps
  pure (InstPort n outPort)

compileList :: [S.Expr] -> Builder Port
compileList = foldr cons (litNode LUnit)
 where
  cons h tlM = do
    ph <- compileExpr h
    pt <- tlM
    n  <- freshNid
    addN (InstBinop n BCons (portNode ph) (portNode pt))
    addE (ph --> InstPort n "lhs"); addE (pt --> InstPort n "rhs")
    pure (InstPort n outPort)

-----------------------------------------------------------
-- LET
-----------------------------------------------------------
compileLet :: [S.Decl] -> S.Expr -> Builder Port
compileLet ds e = do
  old <- gets env
  mapM_ compileLocalDecl ds
  p <- compileExpr e
  modify' (\s -> s { env = old })
  pure p

compileLocalDecl :: S.Decl -> Builder ()
compileLocalDecl (S.FunDecl f ps body) = do
  s <- get
  put s { funEnv = Map.insert f (ps, body) (funEnv s) }

-----------------------------------------------------------
-- CHAMADA DE FUNÇÃO
-----------------------------------------------------------
compileFuncApp :: S.Expr -> [S.Expr] -> Builder Port
compileFuncApp fn args0 = do
  let (f0, as0) = flattenApp (foldl S.App fn args0)
  funName <- case f0 of
    S.Var s -> pure s
    _       -> error "Só suporta chamada de função nomeada"
  (params, funBody) <- lookupFun funName
  -- Criar callgroup com nome único
  callN <- freshNid
  let groupName = T.pack (funName ++ show callN)
  addN (InstCallGroup callN groupName)
  -- Compile argumentos e gere callsnd para cada parâmetro
  argPorts <- mapM compileExpr as0
  callsnds <- forM (zip [0..] argPorts) $ \(i, p) -> do
    sndN <- freshNid
    addN (InstCallSnd sndN groupName callN (portNode p))
    addE (p --> InstPort sndN "in0")
    pure (InstPort sndN outPort)
  -- Mapear parâmetros da função para os callsnd dessa instância
  old <- gets env
  modify' (\s -> s { env = Map.union (Map.fromList (zip params callsnds)) (env s)})
  res <- compileExpr funBody
  modify' (\s -> s { env = old })
  -- retsnd para resultado da chamada
  retN <- freshNid
  addN (InstRetSnd retN groupName callN (portNode res))
  addE (res --> InstPort retN "in0")
  pure (InstPort retN outPort)

flattenApp :: S.Expr -> (S.Expr,[S.Expr])
flattenApp = go [] where
  go acc (S.App f a) = go (a:acc) f
  go acc f           = (f,reverse acc)

-----------------------------------------------------------
-- CASE + PATTERNS
-----------------------------------------------------------
compileCase :: S.Expr -> [(S.Pattern,S.Expr)] -> Builder Port
compileCase scr alts =
  compileAlts =<< compileExpr scr
 where
  compileAlts scrP = go scrP alts

  go scrP [(pat, body)] =    -- Única alternativa: sem steer
    case pat of
      S.PWildcard -> compileExpr body
      _           -> do
        (condP, envX) <- patTest pat scrP
        old <- gets env
        modify' (\s -> s { env = Map.union envX (env s) })
        pRes <- compileExpr body
        modify' (\s -> s { env = old })
        pure pRes

  go scrP ((pat,body):rest) = do
    (condP, envX) <- patTest pat scrP
    sId           <- newSteer condP
    old <- gets env
    modify' (\s -> s { env = Map.union envX (env s) })
    pThen <- compileExpr body
    addE (SteerPort sId truePort --> pThen)
    modify' (\s -> s { env = old })
    pElse <- go scrP rest
    addE (SteerPort sId falsePort --> pElse)
    pure pThen

newSteer :: Port -> Builder NodeId
newSteer predP = do
  n <- freshNid
  addN (InstSteer n (portNode predP))
  addE (predP --> InstPort n "pred")
  pure n

----------------------------------------------------------------------
-- Conversão lit / op
----------------------------------------------------------------------
convLit :: S.Literal -> Literal
convLit = \case
  S.LInt n    -> LInt n
  S.LFloat d  -> LFloat d
  S.LBool b   -> LBool b
  S.LChar c   -> LChar c
  S.LString s -> LString (T.pack s)

convOp :: S.BinOperator -> BinOp
convOp = \case
  S.Add -> BAdd; S.Sub -> BSub; S.Mul -> BMul; S.Div -> BDiv; S.Mod -> BMod
  S.And -> BAnd; S.Or  -> BOr
  S.Lt  -> BLt ; S.Gt  -> BGt; S.Le -> BLe;  S.Ge  -> BGe
  S.Eq  -> BEq ; S.Neq -> BNe

convUn :: S.UnOperator -> UnaryOp
convUn = \case
  S.Neg -> UNeg
  S.Not -> UNot

-----------------------------------------------------------
-- PATTERN TEST
-----------------------------------------------------------
patTest :: S.Pattern -> Port -> Builder (Port, Map S.Ident Port)
patTest pat scrP = case pat of
  S.PWildcard ->
    (, Map.empty) <$> boolConst True
  S.PVar x ->
    pure (scrP, Map.singleton x scrP)
  S.PLit l -> do
    litP <- compileExpr (S.Lit l)
    eqP  <- mkEq scrP litP
    pure (eqP, Map.empty)
  S.PList [] ->
    (, Map.empty) <$> mkIsNil scrP
  S.PList (h:ts) ->
    patTest (S.PCons h (S.PList ts)) scrP
  S.PTuple pats ->
    matchTuple pats
  S.PCons ph pt ->
    matchCons ph pt
  where
  proj :: Int -> Builder Port
  proj idx = do
    n <- freshNid
    addN (InstProj n idx (portNode scrP))
    pure (InstPort n outPort)
  matchTuple :: [S.Pattern] -> Builder (Port, Map S.Ident Port)
  matchTuple pats = do
    results <- forM (zip [0 ..] pats) $ \(i, p) -> do
                 pField <- proj i
                 patTest p pField
    let (conds, envs) = unzip results
    condAll <- foldM1 mkAnd conds
    pure (condAll, Map.unions envs)
  matchCons :: S.Pattern -> S.Pattern -> Builder (Port, Map S.Ident Port)
  matchCons ph pt = do
    pHead <- proj 0
    pTail <- proj 1
    (cH, envH) <- patTest ph pHead
    (cT, envT) <- patTest pt pTail
    condAll    <- mkAnd cH cT
    pure (condAll, Map.union envH envT)

mkEq, mkAnd :: Port -> Port -> Builder Port
mkEq  = mkBin BEq
mkAnd = mkBin BAnd

mkUnary :: UnaryOp -> Port -> Builder Port
mkUnary op a = do
  n <- freshNid
  addN (InstUnary n op (portNode a))
  addE (a --> InstPort n "arg")
  pure (InstPort n outPort)

mkIsNil :: Port -> Builder Port
mkIsNil p = mkUnary UIsNil p

mkBin :: BinOp -> Port -> Port -> Builder Port
mkBin op a b =
  freshNid >>= \n ->
  addN (InstBinop n op (portNode a) (portNode b)) >>
  addE (a --> InstPort n "lhs") >>
  addE (b --> InstPort n "rhs") >>
  pure (InstPort n outPort)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ []       = error "foldM1: empty"
foldM1 _ [x]      = pure x
foldM1 f (x:y:xs) = do
  z <- f x y
  foldM f z xs

-----------------------------------------------------------
-- buildProgram
-----------------------------------------------------------
buildProgram :: S.Program -> DGraph DNode
buildProgram (S.Program ds) =
  evalUnique $ execStateT (predeclare ds >> compileMain ds) emptyB >>= pure . g

-- Só adiciona funções à funEnv
predeclare :: [S.Decl] -> Builder ()
predeclare ds =
  mapM_ (\(S.FunDecl f ps body) -> do
    modify' $ \s -> s { funEnv = Map.insert f (ps, body) (funEnv s) }) ds

-- Só compila a main (e nada mais!)
compileMain :: [S.Decl] -> Builder ()
compileMain ds = case [ (ps, body) | S.FunDecl "main" ps body <- ds ] of
  ((_, body):_) -> void (compileExpr body)
  []            -> error "[Builder] Não existe função main no programa."

