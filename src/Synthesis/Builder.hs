{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synthesis.Builder
  ( DFG
  , buildProgram
  ) where

import           Prelude hiding (lookup)
import           Control.Monad          (forM_, foldM, zipWithM_)
import           Control.Monad.State    (StateT, get, put, runStateT, gets, modify)
import           Control.Monad.Trans    (lift)
import qualified Data.Map              as M

import           Syntax
import           Types                  (DGraph(..), Edge, NodeId, emptyGraph, addNode, addEdge)
import           Port                   (Port(..), (-->))
import           Unique                 (Unique, evalUnique, MonadUnique(..))
import           Node                   (DNode(..))

-- Grafo final
type DFG = DGraph DNode

-- Ambiente: variáveis podem ser portas OU lambdas
data Binding
  = BPort Port
  | BLam  [Ident] Expr

type Env = M.Map Ident Binding

-- Fan-in transparente: se eu entregar uma Port X como resultado de if/case,
-- guardo aqui "fontes alternativas" que também devem ser conectadas a um
-- mesmo destino quando X for usado.
type AliasMap = M.Map (NodeId, String) [Port]

-- Rastreamento simples de funções “ativas” para evitar inline recursivo infinito
type ActiveStack = [Ident]

data BuildS = BuildS
  { bsGraph   :: !DFG
  , bsEnv     :: ![Env]
  , bsAliases :: !AliasMap
  , bsActive  :: !ActiveStack
  }

emptyS :: BuildS
emptyS = BuildS emptyGraph [M.empty] M.empty []

newtype Build a = Build { unBuild :: StateT BuildS Unique a }
  deriving (Functor, Applicative, Monad)

runBuild :: Build a -> (a, BuildS)
runBuild m = evalUnique (runStateT (unBuild m) emptyS)

-- ---------------------------------------------------------------------
-- escopo

pushEnv :: Build ()
pushEnv = Build $ modify (\s -> s { bsEnv = M.empty : bsEnv s })

popEnv :: Build ()
popEnv = Build $ modify (\s -> case bsEnv s of
                                 []     -> s
                                 (_:rs) -> s { bsEnv = rs })

withEnv :: Build a -> Build a
withEnv m = do
  pushEnv
  x <- m
  popEnv
  pure x

insertB :: Ident -> Binding -> Build ()
insertB x b = Build $ modify $ \s ->
  case bsEnv s of
    (e:rs) -> s { bsEnv = M.insert x b e : rs }
    []     -> s

lookupB :: Ident -> Build (Maybe Binding)
lookupB x = Build $ do
  envs <- gets bsEnv
  let go []     = Nothing
      go (e:rs) = case M.lookup x e of
                    Just b  -> Just b
                    Nothing -> go rs
  pure (go envs)

-- ---------------------------------------------------------------------
-- edges + aliases

emit :: Edge -> Build ()
emit e = Build $ modify (\s -> s { bsGraph = addEdge e (bsGraph s) })

connect :: Port -> Port -> Build ()
connect a b = emit (a --> b)

registerAlias :: Port -> [Port] -> Build ()
registerAlias src extras = Build $ modify $ \s ->
  let k = (pNode src, pName src)
  in s { bsAliases = M.insertWith (++) k extras (bsAliases s) }

connectPlus :: Port -> Port -> Build ()
connectPlus src dst = Build $ do
  s <- get
  let k     = (pNode src, pName src)
      alts  = M.findWithDefault [] k (bsAliases s)
      allPs = src : alts
  put s { bsGraph = foldr (\p g -> addEdge (p --> dst) g) (bsGraph s) allPs }

-- ---------------------------------------------------------------------
-- nós

newNode :: String -> DNode -> Build NodeId
newNode label node = Build $ do
  s   <- get
  nid <- lift freshId
  let g' = addNode nid (setName label node) (bsGraph s)
  put s { bsGraph = g' }
  pure nid

naryNode :: String -> DNode -> [Port] -> Build NodeId
naryNode label node ins = do
  nid <- newNode label node
  zipWithM_ (\i p -> connectPlus p (InstPort nid (show i))) [0..] ins
  pure nid

out0, out1 :: NodeId -> Port
out0 nid = InstPort nid "0"
out1 nid = InstPort nid "1"

setName :: String -> DNode -> DNode
setName l n = case n of
  NConstI{}   -> n{ nName = l }
  NConstF{}   -> n{ nName = l }
  NConstD{}   -> n{ nName = l }
  NAdd{}      -> n{ nName = l }
  NSub{}      -> n{ nName = l }
  NMul{}      -> n{ nName = l }
  NDiv{}      -> n{ nName = l }
  NAddI{}     -> n{ nName = l }
  NSubI{}     -> n{ nName = l }
  NMulI{}     -> n{ nName = l }
  NFMulI{}    -> n{ nName = l }
  NDivI{}     -> n{ nName = l }
  NFAdd{}     -> n{ nName = l }
  NDAdd{}     -> n{ nName = l }
  NBand{}     -> n{ nName = l }
  NSteer{}    -> n{ nName = l }
  NLThan{}    -> n{ nName = l }
  NGThan{}    -> n{ nName = l }
  NEqual{}    -> n{ nName = l }
  NLThanI{}   -> n{ nName = l }
  NGThanI{}   -> n{ nName = l }
  NIncTag{}   -> n{ nName = l }
  NIncTagI{}  -> n{ nName = l }
  NCallSnd{}  -> n{ nName = l }
  NRetSnd{}   -> n{ nName = l }
  NRet{}      -> n{ nName = l }
  NTagVal{}   -> n{ nName = l }
  NValTag{}   -> n{ nName = l }
  NCpHToDev{} -> n{ nName = l }
  NCpDevToH{} -> n{ nName = l }
  NCommit{}   -> n{ nName = l }
  NStopSpec{} -> n{ nName = l }
  NSuper{}    -> n{ nName = l }   -- (não usamos aqui)

-- ---------------------------------------------------------------------
-- Constantes e helpers para codificação aritmética de pares/listas

pairBase :: Int
pairBase = 1000003          -- primo > quaisquer valores típicos; ajustável

constI :: Int -> Build Port
constI k = newNode ("const_" ++ show k) (NConstI "" k) >>= \nid -> pure (out0 nid)

nilP :: Build Port
nilP = constI (-1)

isNilP :: Port -> Build Port
isNilP xs = do
  n <- nilP
  bin2 "equal" (NEqual "") xs n

pairEnc :: Port -> Port -> Build Port
pairEnc a b = do
  k  <- constI pairBase
  m  <- bin2Node "mul" (NMul "") a k
  bin2 "add" (NAdd "") (out0 m) b

fstDec :: Port -> Build Port
fstDec p = do
  k <- constI pairBase
  bin2 "div" (NDiv "") p k

sndDec :: Port -> Build Port
sndDec p = do
  k  <- constI pairBase
  -- p % k = p - ((p / k) * k)
  qN <- bin2Node "div" (NDiv "") p k
  mN <- bin2Node "mul" (NMul "") (out0 qN) k
  bin2 "sub" (NSub "") p (out0 mN)

-- foldrM local (para construir listas sem importar libs extras)
foldrM' :: (a -> b -> Build b) -> b -> [a] -> Build b
foldrM' f z0 = go
  where
    go []     = pure z0
    go (y:ys) = do r <- go ys
                   f y r

-- ---------------------------------------------------------------------
-- API

buildProgram :: Program -> DFG
buildProgram (Program decls) =
  let ((), st) = runBuild (mapM_ goDecl decls)
  in bsGraph st

-- ---------------------------------------------------------------------
-- Declarações

goDecl :: Decl -> Build ()
goDecl (FunDecl f ps body)
  | null ps   = do
      p <- withEnv (goExpr body)
      insertB f (BPort p)
  | otherwise =
      insertB f (BLam ps body)

-- ---------------------------------------------------------------------
-- Expressões

goExpr :: Expr -> Build Port
goExpr = \case
  Var x -> do
    mb <- lookupB x
    case mb of
      Just (BPort p)  -> pure p
      Just (BLam _ _) -> pure (InstPort (-1) x)
      Nothing         -> pure (InstPort (-1) x)

  Lit lit ->
    litNode lit

  Lambda ps e ->
    withEnv $ do
      mapM_ (\v -> insertB v (BPort (InstPort (-1) v))) ps
      _ <- goExpr e
      pure (InstPort (-1) "<lambda>")

  -- if c then t else e  (com 2 steers e alias)
  If c t e -> do
    pc   <- goExpr c
    vt   <- withEnv (goExpr t)
    ve   <- withEnv (goExpr e)

    stT  <- newNode "steer" (NSteer "")
    connect vt (InstPort stT "0")
    connect pc (InstPort stT "1")
    let outT = SteerPort stT "t"

    stF  <- newNode "steer" (NSteer "")
    connect ve (InstPort stF "0")
    connect pc (InstPort stF "1")
    let outF = SteerPort stF "f"

    registerAlias outT [outF]
    pure outT

  -- listas e tuplas (valor) — codificadas aritmeticamente
  Cons a b -> do
    pa <- goExpr a
    pb <- goExpr b
    pairEnc pa pb

  List xs -> do
    z  <- nilP
    es <- mapM goExpr xs
    foldrM' (\h acc -> pairEnc h acc) z es

  Tuple [a,b] -> do
    pa <- goExpr a
    pb <- goExpr b
    pairEnc pa pb
  Tuple (a:_) -> goExpr a
  Tuple []    -> constI 0  -- unit ≡ 0

  -- case scr of ...
  Case scr alts ->
    compileCase scr alts

  Let decls body ->
    withEnv (mapM_ goDecl decls >> goExpr body)

  App f x -> do
    let (g, args) = flattenApp (App f x)
    goApp g args

  BinOp op l r -> do
    pl <- goExpr l
    pr <- goExpr r
    case op of
      Add -> bin2 "add"   (NAdd  "") pl pr
      Sub -> bin2 "sub"   (NSub  "") pl pr
      Mul -> bin2 "mul"   (NMul  "") pl pr
      Div -> bin2 "div"   (NDiv  "") pl pr
      -- mod ≡ a - ((a / b) * b)
      Mod -> do
        qN <- bin2Node "div" (NDiv "") pl pr
        let q = out0 qN
        mN <- bin2Node "mul" (NMul "") q  pr
        bin2 "sub" (NSub "") pl (out0 mN)
      Eq  -> bin2 "equal" (NEqual "") pl pr
      Lt  -> bin2 "lthan" (NLThan "") pl pr
      Gt  -> bin2 "gthan" (NGThan "") pl pr
      And -> bin2 "band"  (NBand "") pl pr
      Or  -> orP  pl pr
      Le  -> do lt <- bin2 "lthan" (NLThan "") pl pr
                eq <- bin2 "equal" (NEqual "") pl pr
                orP lt eq
      Ge  -> do gt <- bin2 "gthan" (NGThan "") pl pr
                eq <- bin2 "equal" (NEqual "") pl pr
                orP gt eq
      Neq -> do eq <- bin2 "equal" (NEqual "") pl pr
                notP eq

  UnOp u e -> do
    pe <- goExpr e
    case u of
      Neg -> do
        z <- constI 0
        bin2 "sub" (NSub "") z pe
      Not -> notP pe

  _ ->
    constI 0

-- ---------------------------------------------------------------------
-- Aplicação n-ária

flattenApp :: Expr -> (Expr, [Expr])
flattenApp = \case
  App f x ->
    let (g, xs) = flattenApp f
    in (g, xs ++ [x])
  e -> (e, [])

withActive :: Ident -> Build a -> Build a
withActive f m = Build $ do
  s <- get
  put s{ bsActive = f : bsActive s }
  r <- unBuild m
  s' <- get
  put s'{ bsActive = tail (bsActive s') }
  pure r

isActive :: Ident -> Build Bool
isActive f = Build $ gets (\s -> f `elem` bsActive s)

goApp :: Expr -> [Expr] -> Build Port
goApp fun args = case fun of
  -- chamada a função de topo **sem super**:
  Var f -> do
    mb <- lookupB f
    case mb of
      Just (BLam ps body) -> do
        cyc <- isActive f
        if cyc
          then do
            _ <- mapM goExpr args
            case args of
              [] -> constI 0
              _  -> goExpr (last args)
          else withActive f (applyLambda ps body args)
      _ -> do
        _ <- mapM goExpr args
        case args of
          [] -> constI 0
          _  -> goExpr (last args)

  Lambda ps body ->
    applyLambda ps body args

  _ -> do
    _ <- mapM goExpr args
    case args of
      [] -> constI 0
      _  -> goExpr (last args)

applyLambda :: [Ident] -> Expr -> [Expr] -> Build Port
applyLambda ps body args = do
  let (use, rest) = splitAt (length ps) args
  argv <- mapM goExpr use
  withEnv $ do
    forM_ (zip ps argv) $ \(v,p) -> insertB v (BPort p)
    res <- goExpr body
    foldM (\acc _ -> pure acc) res rest

-- ---------------------------------------------------------------------
-- Literais

litNode :: Literal -> Build Port
litNode = \case
  LInt n    -> constI n
  LFloat d  -> newNode "fconst" (NConstF "" (realToFrac d)) >>= \nid -> pure (out0 nid)
  LChar c   -> constI (fromEnum c)
  LString _ -> constI 0
  LBool b   -> constI (if b then 1 else 0)

-- ---------------------------------------------------------------------
-- Helpers binários / booleanos

bin2 :: String -> DNode -> Port -> Port -> Build Port
bin2 label node a b = do
  nid <- bin2Node label node a b
  pure (out0 nid)

bin2Node :: String -> DNode -> Port -> Port -> Build NodeId
bin2Node label node a b = do
  nid <- newNode label node
  connectPlus a (InstPort nid "0")
  connectPlus b (InstPort nid "1")
  pure nid

notP :: Port -> Build Port
notP x = do
  z <- constI 0
  bin2 "equal" (NEqual "") x z      -- x == 0

orP :: Port -> Port -> Build Port
orP a b = do
  s <- bin2Node "add" (NAdd "") a b
  z <- constI 0
  bin2 "gthan" (NGThan "") (out0 s) z  -- (a+b) > 0

andP :: Port -> Port -> Build Port
andP a b = bin2 "band" (NBand "") a b

trueP, falseP :: Build Port
trueP  = constI 1
falseP = constI 0

-- ---------------------------------------------------------------------
-- CASE / Pattern matching

compileCase :: Expr -> [(Pattern, Expr)] -> Build Port
compileCase scr alts = do
  pscr <- goExpr scr
  taken0 <- falseP
  outs <- goAlts pscr taken0 alts []
  case outs of
    []       -> falseP
    (h:rest) -> registerAlias h rest >> pure h
  where
    goAlts :: Port -> Port -> [(Pattern, Expr)] -> [Port] -> Build [Port]
    goAlts _    _     []            acc = pure (reverse acc)
    goAlts pscr taken ((p,e):rs) acc = do
      (pPred, binds) <- patPred pscr p
      ntaken <- notP taken
      guardi <- andP pPred ntaken

      val <- withEnv $ do
               mapM_ (\(x,v) -> insertB x (BPort v)) binds
               goExpr e

      sid <- newNode "steer" (NSteer "")
      connect val    (InstPort sid "0")
      connect guardi (InstPort sid "1")
      let out = SteerPort sid "t"

      taken' <- orP taken pPred
      goAlts pscr taken' rs (out:acc)

-- Predicado do padrão e binders
patPred :: Port -> Pattern -> Build (Port, [(Ident, Port)])
patPred scr = \case
  -- Tupla 2: decodifica com div/mod
  PTuple [p1,p2] -> do
    a <- fstDec scr
    b <- sndDec scr
    t <- trueP
    pure (t, bindIfVar p1 a ++ bindIfVar p2 b)

  -- Lista vazia []
  PList [] -> do
    p <- isNilP scr
    pure (p, [])

  -- Lista singleton [x]
  PList [p] -> do
    nz   <- notP =<< isNilP scr
    tl   <- sndDec scr
    isTl <- isNilP tl
    g    <- andP nz isTl
    hd   <- fstDec scr
    pure (g, bindIfVar p hd)

  -- Cons x:xs
  PCons ph pt -> do
    nz <- notP =<< isNilP scr
    hd <- fstDec scr
    tl <- sndDec scr
    pure (nz, bindIfVar ph hd ++ bindIfVar pt tl)

  -- Variável / coringa / literal
  PWildcard -> trueP >>= \t -> pure (t, [])
  PVar x    -> trueP >>= \t -> pure (t, [(x, scr)])
  PLit lit  -> do litP <- litNode' lit
                  eq   <- bin2 "equal" (NEqual "") scr litP
                  pure (eq, [])
  _         -> trueP >>= \t -> pure (t, [])

bindIfVar :: Pattern -> Port -> [(Ident, Port)]
bindIfVar = \case
  PVar x -> \p -> [(x,p)]
  _      -> \_ -> []

-- versão de literal que devolve Port direto (usada em patPred)
litNode' :: Literal -> Build Port
litNode' = litNode
