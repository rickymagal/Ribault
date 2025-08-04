{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Synthesis.Builder (buildProgram) where

import Prelude                 hiding (id)
import Control.Monad.State.Strict
import Control.Monad           (forM_, forM, zipWithM_)
import Data.Map.Strict         (Map)
import qualified Data.Map.Strict   as Map
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL

import GHC.Conc               (getNumCapabilities)
import System.IO.Unsafe       (unsafePerformIO)

import Types
import Node
import Port
import Unique
import qualified Syntax          as S

-- Estado do Builder: grafo, env e tamanho (work) da função corrente
data BState = BState
  { g          :: DGraph DNode
  , env        :: Map S.Ident Port
  , totalNodes :: Int    -- N = countNodes do corpo da função atual
  , funNames   :: [S.Ident]
  , currentFun :: S.Ident
  }

type Builder = StateT BState Unique

emptyB :: BState
emptyB = BState emptyGraph Map.empty 0 [] ""

freshNid :: Builder NodeId
freshNid = lift freshId

addN :: DNode -> Builder ()
addE :: Edge  -> Builder ()
addN n = modify' $ \s -> s { g = addNode (nId n) n (g s) }
addE e = modify' $ \s -> s { g = addEdge e (g s) }

bindVar :: S.Ident -> Port -> Builder ()
bindVar x p = modify' $ \s -> s { env = Map.insert x p (env s) }

lookupVar :: S.Ident -> Builder Port
lookupVar x =
  gets (Map.lookup x . env) >>= maybe (error ("[Builder] unbound: " <> x)) pure

litNode :: Literal -> Builder Port
litNode lit = do
  n <- freshNid
  addN (InstConst n lit)
  pure (InstPort n outPort)

boolConst :: Bool -> Builder Port
boolConst = litNode . LBool


-- Parâmetros de grain‐size no estilo Cilk
numCaps     :: Int
numCaps      = unsafePerformIO getNumCapabilities
{-# NOINLINE numCaps #-}

maxGrain     :: Int
maxGrain      = 2048

splitFactor  :: Int
splitFactor   = 8

-- Estimamos o “work” como o número de nós na AST
countNodes :: S.Expr -> Int
countNodes = \case
  S.Var _         -> 1
  S.Lit _         -> 1
  S.Lambda _ e    -> 1 + countNodes e
  S.If c t e      -> 1 + countNodes c + countNodes t + countNodes e
  S.Case s alts   -> 1 + countNodes s + sum [countNodes bd | (_, bd) <- alts]
  S.Let ds e      -> 1
                   + sum [countNodes bd | S.FunDecl _ _ bd <- ds]
                   + countNodes e
  S.App f a       -> 1 + countNodes f + countNodes a
  S.BinOp _ l r   -> 1 + countNodes l + countNodes r
  S.UnOp _ x      -> 1 + countNodes x
  S.List xs       -> 1 + sum (map countNodes xs)
  S.Tuple xs      -> 1 + sum (map countNodes xs)
  S.Cons h t      -> 1 + countNodes h + countNodes t

-- | Compila uma expressão. Se for uma chamada de função (S.App)
-- e “work <= grainSize”, dispara runHscCached; caso contrário,
-- faz a compilação normal do AST.
compileExpr :: S.Expr -> Builder Port
compileExpr expr = case expr of

  ----------------------------------------------------------------------------
  -- 1) Application of the currently-compiling function itself (“self-call”):
  --    spawn+cache when work ≤ grainSize.
  ----------------------------------------------------------------------------
  exprApp@(S.App _ _) -> do
    -- a) Which top-level function are we in?
    currFun <- gets currentFun    -- currentFun :: S.Ident stored in BState

    -- b) Flatten the application to (fn, [args])
    let (f0, args) = flattenApp exprApp

        -- c) Is this a recursive call to the very function we’re compiling?
        isSelfCall = case f0 of
                       S.Var name -> name == currFun
                       _          -> False

    -- d) Compile function and arguments normally
    pFn  <- compileExpr f0
    pArgs <- mapM compileExpr args

    -- e) Compute Cilk-style cutoff
    total     <- gets totalNodes
    let work      = countNodes expr
        grainSize = min maxGrain
                      ( ceiling ( fromIntegral total
                                / fromIntegral (splitFactor * numCaps)
                                )
                      )

    -- f) If it’s a self-call and small enough, spawn + cache
    if isSelfCall && work <= grainSize
      then do
        let ins = portNode pFn : map portNode pArgs
        n <- freshNid
        addN (InstSuper n "runHscCached" ins 1)

        -- wire up each input
        addE (pFn  --> InstPort n "in0")
        forM_ (zip [1..] pArgs) $ \(i,p) ->
          addE (p --> InstPort n ("in" <> show i))

        pure (InstPort n "out0")
      else
        -- otherwise, ordinary apply node
        compileApp f0 args

  ----------------------------------------------------------------------------
  -- 2) Variables & literals
  ----------------------------------------------------------------------------
  S.Var x   -> lookupVar x
  S.Lit l   -> litNode (convLit l)

  ----------------------------------------------------------------------------
  -- 3) Lambda abstraction (unchanged)
  ----------------------------------------------------------------------------
  S.Lambda ps body -> do
    parList <- forM ps $ \p -> do
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

  ----------------------------------------------------------------------------
  -- 4) Primitive ops & constructors
  ----------------------------------------------------------------------------
  S.BinOp op a b -> binop (convOp op) a b
  S.UnOp  op e   -> unop  (convUn op) e
  S.Cons  h t    -> binop BCons h t

  ----------------------------------------------------------------------------
  -- 5) Inline if/then/else
  ----------------------------------------------------------------------------
  S.If c t e -> do
    pc  <- compileExpr c
    sid <- freshNid
    addN  (InstSteer sid (portNode pc))
    addE  (pc --> InstPort sid "pred")
    pT  <- compileExpr t
    pF  <- compileExpr e
    addE  (SteerPort sid truePort  --> pT)
    addE  (SteerPort sid falsePort --> pF)
    pure pT

  ----------------------------------------------------------------------------
  -- 6) Tuples, lists, lets & cases
  ----------------------------------------------------------------------------
  S.Tuple es       -> compileTuple es
  S.List  es       -> compileList es
  S.Let   ds e     -> compileLet   ds e
  S.Case  scr alts -> compileCase  scr alts

 where
  -- Binary operator node
  binop :: BinOp -> S.Expr -> S.Expr -> Builder Port
  binop bop x y = do
    px <- compileExpr x
    py <- compileExpr y
    n  <- freshNid
    addN (InstBinop n bop (portNode px) (portNode py))
    addE (px --> InstPort n "lhs")
    addE (py --> InstPort n "rhs")
    pure (InstPort n outPort)

  -- Unary operator node
  unop :: UnaryOp -> S.Expr -> Builder Port
  unop u e = do
    pe <- compileExpr e
    n  <- freshNid
    addN (InstUnary n u (portNode pe))
    addE (pe --> InstPort n "arg")
    pure (InstPort n outPort)

----------------------------------------------------------------------
-- IF / TUPLE / LIST
----------------------------------------------------------------------
compileIf :: S.Expr -> S.Expr -> S.Expr -> Builder Port
compileIf c t e = do
  pC <- compileExpr c
  s  <- freshNid
  addN (InstSteer s (portNode pC))
  addE (pC --> InstPort s "pred")
  pT <- compileExpr t
  pF <- compileExpr e
  addE (SteerPort s truePort  --> pT)
  addE (SteerPort s falsePort --> pF)
  pure pT

compileTuple :: [S.Expr] -> Builder Port
compileTuple es = do
  ps <- mapM compileExpr es
  n  <- freshNid
  addN (InstTuple n (map portNode ps))
  zipWithM_ (\i p -> addE (p --> InstPort n ("f" <> show i))) [0..] ps
  pure (InstPort n outPort)

compileList :: [S.Expr] -> Builder Port
compileList = foldr cons (litNode LUnit)
 where
  cons h tlM = do
    ph <- compileExpr h
    pt <- tlM
    n  <- freshNid
    addN (InstBinop n BCons (portNode ph) (portNode pt))
    addE (ph --> InstPort n "lhs")
    addE (pt --> InstPort n "rhs")
    pure (InstPort n outPort)


----------------------------------------------------------------------
-- LET  / APPLICATION
----------------------------------------------------------------------
compileLet :: [S.Decl] -> S.Expr -> Builder Port
compileLet ds e = do
  old <- gets env
  mapM_ compileLocalDecl ds
  p   <- compileExpr e
  modify' (\s -> s { env = old })
  pure p

compileLocalDecl :: S.Decl -> Builder ()
compileLocalDecl (S.FunDecl f ps body)
  | null ps   = do
      p <- compileExpr body
      n <- freshNid
      addN (InstSuper n ("<local:" <> T.pack f <> ">") [portNode p] 1)
      addE (p --> InstPort n "in0")
      bindVar f (InstPort n "out0")
  | otherwise = do
      n <- freshNid
      let ins = [InstPort n ("in" <> show i) | i <- [0..length ps-1]]
      old <- gets env
      modify' (\s -> s { env = Map.union (Map.fromList (zip ps ins)) old })
      pBody <- compileExpr body
      addN (InstSuper n ("<local:" <> T.pack f <> ">") (map portNode ins) 1)
      addE (pBody --> InstPort n "out0")
      modify' (\s -> s { env = old })
      bindVar f (InstPort n "out0")

compileApp :: S.Expr -> [S.Expr] -> Builder Port
compileApp fn args0 = do
  let (f0, as0) = flattenApp (foldl S.App fn args0)
  pFun  <- compileExpr f0
  pArgs <- mapM compileExpr as0
  n     <- freshNid
  let ins = portNode pFun : map portNode pArgs
  addN (InstSuper n "<apply>" ins 1)
  addE (pFun --> InstPort n "in0")
  zipWithM_ (\i p -> addE (p --> InstPort n ("in" <> show (i+1)))) [0..] pArgs
  pure (InstPort n "out0")

flattenApp :: S.Expr -> (S.Expr,[S.Expr])
flattenApp = go [] where
  go acc (S.App f a) = go (a:acc) f
  go acc f           = (f,reverse acc)

----------------------------------------------------------------------
-- CASE + PATTERN MATCHING
----------------------------------------------------------------------
compileCase :: S.Expr -> [(S.Pattern,S.Expr)] -> Builder Port
compileCase scr alts = compileAlts =<< compileExpr scr
 where
  compileAlts scrP = go scrP alts

  go scrP [(pat, body)] = case pat of
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
    addE (SteerPort sId truePort  --> pThen)
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
-- PATTERN TEST HELPERS
----------------------------------------------------------------------
patTest :: S.Pattern -> Port -> Builder (Port, Map S.Ident Port)
patTest pat scrP = case pat of
  S.PWildcard -> (,Map.empty) <$> boolConst True
  S.PVar x    -> pure (scrP, Map.singleton x scrP)
  S.PLit l    -> do litP <- compileExpr (S.Lit l)
                    eqP  <- mkEq scrP litP
                    pure (eqP, Map.empty)
  S.PList []  -> (,Map.empty) <$> mkIsNil scrP
  S.PList (h:ts) -> patTest (S.PCons h (S.PList ts)) scrP
  S.PTuple pats -> matchTuple pats
  S.PCons ph pt -> matchCons ph pt
 where
  proj idx = do
    n <- freshNid
    addN (InstProj n idx (portNode scrP))
    pure (InstPort n outPort)

  matchTuple pats = do
    res <- forM (zip [0..] pats) $ \(i,p) -> do
      pF <- proj i
      patTest p pF
    let (cs, es) = unzip res
    condAll <- foldM1 mkAnd cs
    pure (condAll, Map.unions es)

  matchCons ph pt = do
    pH <- proj 0
    pT <- proj 1
    (cH, eH) <- patTest ph pH
    (cT, eT) <- patTest pt pT
    condAll  <- mkAnd cH cT
    pure (condAll, Map.union eH eT)

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
mkIsNil = mkUnary UIsNil

mkBin :: BinOp -> Port -> Port -> Builder Port
mkBin op a b = do
  n <- freshNid
  addN (InstBinop n op (portNode a) (portNode b))
  addE (a --> InstPort n "lhs")
  addE (b --> InstPort n "rhs")
  pure (InstPort n outPort)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ []       = error "foldM1: empty"
foldM1 _ [x]      = pure x
foldM1 f (x:y:xs) = f x y >>= \z -> foldM1 f (z:xs)

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
-- convLit / convOp / convUn
----------------------------------------------------------------------
convLit :: S.Literal -> Literal
convLit = \case
  S.LInt  n    -> LInt n
  S.LFloat d   -> LFloat d
  S.LBool  b   -> LBool b
  S.LChar  c   -> LChar c
  S.LString s  -> LString (T.pack s)

convOp :: S.BinOperator -> BinOp
convOp = \case
  S.Add -> BAdd; S.Sub -> BSub; S.Mul -> BMul; S.Div -> BDiv; S.Mod -> BMod
  S.And -> BAnd; S.Or  -> BOr
  S.Lt  -> BLt; S.Gt  -> BGt; S.Le  -> BLe; S.Ge  -> BGe
  S.Eq  -> BEq; S.Neq -> BNe

convUn :: S.UnOperator -> UnaryOp
convUn = \case
  S.Neg -> UNeg
  S.Not -> UNot

----------------------------------------------------------------------
-- buildProgram: antes de compilar cada função, grava seu tamanho
----------------------------------------------------------------------
buildProgram :: S.Program -> DGraph DNode
buildProgram (S.Program ds) =
  evalUnique $ do
    st <- execStateT setup emptyB
    pure (g st)
  where
    setup :: Builder ()
    setup = do
      -- (a) forward-declare todas as funções
      predeclare ds

      -- (b) guarde a lista de nomes top-level em funNames
      let names = [ name | S.FunDecl name _ _ <- ds ]
      modify' $ \s -> s { funNames = names }

      -- (c) para cada declaração, set currentFun e totalNodes antes de compilar
      forM_ ds $ \(S.FunDecl name params body) -> do
        -- agora 'name' está em escopo e você pode usá-lo
        modify' $ \s -> s
          { currentFun = name
          , totalNodes = countNodes body
          }
        compileTopDecl (S.FunDecl name params body)

predeclare :: [S.Decl] -> Builder ()
predeclare = mapM_ $ \(S.FunDecl f _ _) -> do
  n <- freshNid
  addN (InstSuper n ("<fwd:" <> T.pack f <> ">") [] 1)
  bindVar f (InstPort n "out0")
