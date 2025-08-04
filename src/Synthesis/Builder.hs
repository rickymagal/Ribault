{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Synthesis.Builder (buildProgram) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------
import           Prelude                 hiding (id)
import           Control.Monad.State.Strict
import           Control.Monad           (foldM, forM, zipWithM_)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL

import           GHC.Conc                  (getNumCapabilities)
import           Control.Monad.IO.Class    (liftIO)
import System.IO.Unsafe (unsafePerformIO)

import           Types
import           Node
import           Port
import           Unique
import qualified Syntax                  as S

----------------------------------------------------------------------
-- Estado + helpers ---------------------------------------------------
----------------------------------------------------------------------
data BState = BState
  { g   :: !(DGraph DNode)
  , env :: !(Map S.Ident Port)
  , totalNodes :: Int 
  }

type Builder = StateT BState Unique
emptyB :: BState
emptyB = BState emptyGraph Map.empty 0

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
numCaps :: Int
numCaps = unsafePerformIO getNumCapabilities

countNodes :: S.Expr -> Int
countNodes = \case
  S.Var _       -> 1
  S.Lit _       -> 1
  S.Lambda _ e  -> 1 + countNodes e
  S.If c t e    -> 1 + countNodes c + countNodes t + countNodes e
  S.Case s alts -> 1 + countNodes s + sum [countNodes bd | (_,bd) <- alts]
  S.Let ds e    -> 1 + sum [ countNodes bd | S.FunDecl _ _ bd <- ds ] + countNodes e
  S.App f a     -> 1 + countNodes f + countNodes a
  S.BinOp _ l r -> 1 + countNodes l + countNodes r
  S.UnOp _ x    -> 1 + countNodes x
  S.List xs     -> 1 + sum (map countNodes xs)
  S.Tuple xs    -> 1 + sum (map countNodes xs)
  S.Cons h t    -> 1 + countNodes h + countNodes t

compileExpr :: S.Expr -> Builder Port
compileExpr expr = do
  total    <- gets totalNodes
  let grain = countNodes expr
  let threshold = fromIntegral total / fromIntegral numCaps
  if fromIntegral grain >= threshold
       then do
         n <- freshNid            -- usa freshNid (lift freshId)
         addN (InstSuper n "runHscCached" [] 1)
         pure (InstPort n "out0")
       else case expr of
         S.Var x         -> lookupVar x
         S.Lit l         -> litNode (convLit l)

         S.Lambda params body -> do
           parList <- forM params $ \p -> do
                        pid <- freshNid
                        addN (InstPar pid (T.pack p) [] 1)
                        pure (p, InstPort pid "out0")
           let ins = map snd parList
           oldEnv <- gets env
           modify' (\s -> s { env = Map.union (Map.fromList parList) oldEnv })
           pBody <- compileExpr body
           nFun  <- freshNid
           addN (InstSuper nFun ("λ#" <> T.pack (show nFun)) (map portNode ins) 1)
           addE (pBody --> InstPort nFun "out0")
           modify' (\s -> s { env = oldEnv })
           pure (InstPort nFun "out0")

         S.BinOp o a b   -> binop (convOp o) a b
         S.UnOp  o e     -> unop  (convUn o) e
         S.Cons h t      -> binop BCons h t
         S.If c t e      -> compileIf c t e
         S.Tuple es      -> compileTuple es
         S.List  es      -> compileList es
         S.App f a       -> compileApp f [a]
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

----------------------------------------------------------------------
-- IF / TUPLE / LIST --------------------------------------------------
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

----------------------------------------------------------------------
-- LET  (sem mudanças) -----------------------------------------------
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
      modify' (\s -> s { env = Map.union (Map.fromList (zip ps ins)) (env s)})
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
compileCase scr alts =
  compileAlts =<< compileExpr scr
 where
  compileAlts scrP = go scrP alts

  go scrP [(pat, body)] =    -- Única alternativa: sem steer
    -- se é coringa, só compila o corpo
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
  place <- lookupVar f
  let nId = case place of
              InstPort nid _ -> nid
              _              -> error "[Builder] expected InstPort"
  if null ps                                   -- binding simples
    then do pVal <- compileExpr body
            addN (InstSuper nId (T.pack f) [portNode pVal] 1)
            addE (pVal --> InstPort nId "in0")
    else do                                    -- função com parâmetros
      parNodes <- forM (zip [0..] ps) $ \(i, p) -> do
                    pid <- freshNid
                    addN (InstPar pid (T.pack p) [] 1)
                    pure (p, InstPort pid "out0")
      let ins = map snd parNodes
      old <- gets env
      modify' (\s -> s { env = Map.union (Map.fromList parNodes) (env s)})
      pBody <- compileExpr body
      addN (InstSuper nId (T.pack f) (map portNode ins) 1)
      addE (pBody --> InstPort nId "out0")
      modify' (\s -> s { env = old })
      bindVar f (InstPort nId "out0")

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

----------------------------------------------------------------------
-- buildProgram
----------------------------------------------------------------------
buildProgram :: S.Program -> DGraph DNode
buildProgram (S.Program ds) =
  -- avalia o Builder no monad Unique e retorna apenas o grafo
  evalUnique $ do
    -- executa o StateT, produzindo o estado final
    st <- execStateT setup emptyB
    -- extrai o grafo do estado
    pure (g st)
  where
    -- passos iniciais do Builder
    setup :: Builder ()
    setup = do
      -- 1) soma todos os nós do programa
      let total = sum [ countNodes bd | S.FunDecl _ _ bd <- ds ]
      modify' (\s -> s { totalNodes = total })

      -- 2) pré-declarações (foward declarations)
      predeclare ds

      -- 3) compila cada definição top-level
      mapM_ compileTopDecl ds

    -- contagem de nós (já existente em seu módulo)
    countNodes :: S.Expr -> Int
    countNodes = \case
      S.Var _         -> 1
      S.Lit _         -> 1
      S.Lambda _ e    -> 1 + countNodes e
      S.If c t e      -> 1 + countNodes c + countNodes t + countNodes e
      S.Case s alts   -> 1 + countNodes s + sum [ countNodes bd | (_, bd) <- alts ]
      S.Let ds e      -> 1 + sum [ countNodes bd | S.FunDecl _ _ bd <- ds ] + countNodes e
      S.App f a       -> 1 + countNodes f + countNodes a
      S.BinOp _ l r   -> 1 + countNodes l + countNodes r
      S.UnOp _ x      -> 1 + countNodes x
      S.List xs       -> 1 + sum (map countNodes xs)
      S.Tuple xs      -> 1 + sum (map countNodes xs)
      S.Cons h t      -> 1 + countNodes h + countNodes t

predeclare :: [S.Decl] -> Builder ()
predeclare =
  mapM_ $ \(S.FunDecl f _ _) -> do
    n <- freshNid
    addN (InstSuper n ("<fwd:" <> T.pack f <> ">") [] 1)
    bindVar f (InstPort n "out0")
