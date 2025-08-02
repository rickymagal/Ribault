{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

-- | Builder – converte a AST tipada (Syntax) em grafo Data-Flow (DGraph DNode)
module Synthesis.Builder (buildProgram) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------
import           Prelude                 hiding (id)
import           Control.Monad.State.Strict
import           Control.Monad           (foldM, forM)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           Types
import           Node
import           Port
import           Unique
import qualified Syntax                  as S

----------------------------------------------------------------------
-- Estado interno
----------------------------------------------------------------------
data BState = BState { g   :: !(DGraph DNode)
                     , env :: !(Map S.Ident Port) }

type Builder = StateT BState Unique
emptyB :: BState
emptyB = BState emptyGraph Map.empty

----------------------------------------------------------------------
-- Auxiliares de grafo
----------------------------------------------------------------------
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

litNode :: Literal -> Builder Port
litNode lit = do n <- freshNid
                 addN (InstConst n lit)
                 pure (InstPort n outPort)

boolConst :: Bool -> Builder Port
boolConst = litNode . LBool

----------------------------------------------------------------------
-- EXPRESSÕES
----------------------------------------------------------------------
compileExpr :: S.Expr -> Builder Port
compileExpr = \case
  S.Var x        -> lookupVar x
  S.Lit l        -> litNode (convLit l)
  S.Lambda _ _   -> litNode (LString "λ")
  S.BinOp o a b  -> binop (convOp o) a b
  S.UnOp  o e    -> unop  (convUn o) e
  S.Cons h t     -> binop BCons h t
  S.If c t e     -> compileIf c t e
  S.Tuple es     -> compileTuple es
  S.List es      -> compileList es
  S.App f a      -> compileApp f [a]
  S.Let ds e     -> compileLet ds e
  S.Case s as    -> compileCase s as
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

----------------------------------------------------------------------
-- IF / TUPLE / LIST
----------------------------------------------------------------------
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
  ps <- mapM compileExpr es; n <- freshNid
  addN (InstTuple n (map portNode ps))
  zipWithM_ (\i p -> addE (p --> InstPort n ("f"<>show i))) [0..] ps
  pure (InstPort n outPort)

compileList :: [S.Expr] -> Builder Port
compileList = foldr cons (litNode LUnit)
 where
  cons h tlM = do
    ph <- compileExpr h; pt <- tlM; n <- freshNid
    addN (InstBinop n BCons (portNode ph) (portNode pt))
    addE (ph --> InstPort n "lhs"); addE (pt --> InstPort n "rhs")
    pure (InstPort n outPort)
----------------------------------------------------------------------
-- LET
----------------------------------------------------------------------
compileLet :: [S.Decl] -> S.Expr -> Builder Port
compileLet ds e = do
  old <- gets env
  mapM_ compileLocalDecl ds
  p <- compileExpr e
  modify' (\s -> s { env = old })
  pure p

compileLocalDecl :: S.Decl -> Builder ()
compileLocalDecl (S.FunDecl f ps body)
  | null ps   = do
      p <- compileExpr body; n <- freshNid
      addN (InstSuper n ("<local:"<>T.pack f<>">") [portNode p] 1)
      addE (p --> InstPort n "in0")
      bindVar f (InstPort n "out0")
  | otherwise = do
      n <- freshNid
      let ins = [ InstPort n ("in"<>show i) | i <- [0..length ps-1] ]
      old <- gets env
      modify' (\s -> s { env = Map.union (Map.fromList (zip ps ins)) (env s) })
      pBody <- compileExpr body
      addN (InstSuper n ("<local:"<>T.pack f<>">") (map portNode ins) 1)
      addE (pBody --> InstPort n "out0")
      modify' (\s -> s { env = old })
      bindVar f (InstPort n "out0")

----------------------------------------------------------------------
-- APPLICATION
----------------------------------------------------------------------
compileApp :: S.Expr -> [S.Expr] -> Builder Port
compileApp fn args0 = do
  let (f0, as0) = flattenApp (foldl S.App fn args0)
  pFun  <- compileExpr f0
  pArgs <- mapM compileExpr as0
  n <- freshNid
  let ins = portNode pFun : map portNode pArgs
  addN (InstSuper n "<apply>" ins 1)
  addE (pFun --> InstPort n "in0")
  zipWithM_ (\i p -> addE (p --> InstPort n ("in"<>show (i+1)))) [0..] pArgs
  pure (InstPort n "out0")

flattenApp :: S.Expr -> (S.Expr,[S.Expr])
flattenApp = go [] where
  go acc (S.App f a) = go (a:acc) f
  go acc f           = (f,reverse acc)

----------------------------------------------------------------------
-- CASE + PATTERNS
----------------------------------------------------------------------
compileCase :: S.Expr -> [(S.Pattern,S.Expr)] -> Builder Port
compileCase scr alts = do
  pScr <- compileExpr scr
  compileAlts pScr alts
 where
  compileAlts _ [] = error "[Builder] empty case"
  compileAlts s ((p,b):rs) = do
    (cond,envX) <- patTest p s
    st          <- newSteer cond
    old <- gets env
    modify' (\st' -> st' { env = Map.union envX (env st') })
    pThen <- compileExpr b
    addE (SteerPort st truePort  --> pThen)
    modify' (\st' -> st' { env = old })
    pElse <- if null rs then boolConst False else compileAlts s rs
    addE (SteerPort st falsePort --> pElse)
    pure pThen
  newSteer cP = do n <- freshNid
                   addN (InstSteer n (portNode cP))
                   addE (cP --> InstPort n "pred")
                   pure n

----------------------------------------------------------------------
-- PATTERN TEST
----------------------------------------------------------------------
----------------------------------------------------------------------
-- PATTERN TEST  (_, Var, Lit, Tuple, Cons)
----------------------------------------------------------------------
patTest :: S.Pattern -> Port -> Builder (Port, Map S.Ident Port)
patTest pat scrP = case pat of
  ------------------------------------------------------------
  -- Coringa _
  ------------------------------------------------------------
  S.PWildcard ->
    (, Map.empty) <$> boolConst True

  ------------------------------------------------------------
  -- Variável vinculada
  ------------------------------------------------------------
  S.PVar x ->
    pure (scrP, Map.singleton x scrP)

  ------------------------------------------------------------
  -- Literal
  ------------------------------------------------------------
  S.PLit l -> do
    litP <- compileExpr (S.Lit l)
    eqP  <- mkEq scrP litP
    pure (eqP, Map.empty)

  ------------------------------------------------------------
  -- Lista vazia []
  ------------------------------------------------------------
  S.PList [] ->
    (, Map.empty) <$> mkIsNil scrP

  ------------------------------------------------------------
  -- Lista com elementos  [h, t1, t2, …]  -->  h : [t1,t2,…]
  ------------------------------------------------------------
  S.PList (h:ts) ->
    patTest (S.PCons h (S.PList ts)) scrP

  ------------------------------------------------------------
  -- Tupla  (p1, p2, ...)
  ------------------------------------------------------------
  S.PTuple pats ->
    matchTuple pats

  ------------------------------------------------------------
  -- Cons  h : t
  ------------------------------------------------------------
  S.PCons ph pt ->
    matchCons ph pt

  ------------------------------------------------------------
  -- Ainda não suportado
  ------------------------------------------------------------

  where
  ------------------------------------------------------------------
  -- Projeta o campo ‘idx’ do valor em análise
  ------------------------------------------------------------------
  proj :: Int -> Builder Port
  proj idx = do
    n <- freshNid
    addN (InstProj n idx (portNode scrP))
    pure (InstPort n outPort)

  ------------------------------------------------------------------
  -- Casamento de tupla
  ------------------------------------------------------------------
  matchTuple :: [S.Pattern] -> Builder (Port, Map S.Ident Port)
  matchTuple pats = do
    results <- forM (zip [0 ..] pats) $ \(i, p) -> do
                 pField <- proj i
                 patTest p pField
    let (conds, envs) = unzip results
    condAll <- foldM1 mkAnd conds
    pure (condAll, Map.unions envs)

  ------------------------------------------------------------------
  -- Casamento de Cons  (h : t)
  ------------------------------------------------------------------
  matchCons :: S.Pattern -> S.Pattern -> Builder (Port, Map S.Ident Port)
  matchCons ph pt = do
    pHead <- proj 0
    pTail <- proj 1
    (cH, envH) <- patTest ph pHead
    (cT, envT) <- patTest pt pTail
    condAll    <- mkAnd cH cT
    pure (condAll, Map.union envH envT)
----------------------------------------------------------------------
-- HELPERS p/ patterns
----------------------------------------------------------------------
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
----------------------------------------------------------------------
-- Declarações top-level
----------------------------------------------------------------------
compileTopDecl :: S.Decl -> Builder ()
compileTopDecl (S.FunDecl f ps body) = do
  n <- freshNid
  if null ps
    then do pVal <- compileExpr body
            addN (InstSuper n (T.pack f) [portNode pVal] 1)
            addE (pVal --> InstPort n "in0")
    else do
      let ins = [ InstPort n ("in"<>show i) | i <- [0..length ps-1] ]
      old <- gets env
      modify' (\s -> s { env = Map.union (Map.fromList (zip ps ins)) (env s) })
      pBody <- compileExpr body
      addN (InstSuper n (T.pack f) (map portNode ins) 1)
      addE (pBody --> InstPort n "out0")
      modify' (\s -> s { env = old })
      bindVar f (InstPort n "out0")

----------------------------------------------------------------------
-- Conversão de literais / operadores
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

----------------------------------------------------------------------
-- buildProgram: 1ª passo pré-declara nomes, 2ª compila
----------------------------------------------------------------------
buildProgram :: S.Program -> DGraph DNode
buildProgram (S.Program ds) =
  evalUnique $
    fmap g (execStateT (predeclare ds >> mapM_ compileTopDecl ds) emptyB)

predeclare :: [S.Decl] -> Builder ()
predeclare = mapM_ $ \(S.FunDecl f _ _) -> do
  n <- freshNid
  addN (InstSuper n ("<fwd:"<>T.pack f<>">") [] 1)
  bindVar f (InstPort n "out0")
