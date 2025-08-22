{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synthesis.Builder
  ( DFG
  , buildProgram
  ) where

import           Prelude hiding (lookup)
import           Control.Monad          (forM_, foldM, zipWithM_, when)
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

-- Ambiente
data Binding = BPort Port | BLam [Ident] Expr
type Env = M.Map Ident Binding

type AliasMap    = M.Map (NodeId, String) [Port]
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

-- escopo ---------------------------------------------------------------

pushEnv, popEnv :: Build ()
pushEnv = Build $ modify (\s -> s { bsEnv = M.empty : bsEnv s })
popEnv  = Build $ modify (\s -> case bsEnv s of [] -> s; (_:rs) -> s { bsEnv = rs })

withEnv :: Build a -> Build a
withEnv m = do pushEnv; x <- m; popEnv; pure x

insertB :: Ident -> Binding -> Build ()
insertB x b = Build $ modify $ \s -> case bsEnv s of
  (e:rs) -> s { bsEnv = M.insert x b e : rs }
  []     -> s

lookupB :: Ident -> Build (Maybe Binding)
lookupB x = Build $ gets $ \s ->
  let go []     = Nothing
      go (e:rs) = maybe (go rs) Just (M.lookup x e)
  in go (bsEnv s)

-- edges + aliases ------------------------------------------------------

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

-- nós ------------------------------------------------------------------

newNode :: String -> DNode -> Build NodeId
newNode lbl nd = Build $ do
  s   <- get
  nid <- lift freshId
  let g' = addNode nid (setName lbl nd) (bsGraph s)
  put s { bsGraph = g' }
  pure nid

naryNode :: String -> DNode -> [Port] -> Build NodeId
naryNode lbl nd ins = do
  nid <- newNode lbl nd
  zipWithM_ (\i p -> connectPlus p (InstPort nid (show i))) [0..] ins
  pure nid

out0 :: NodeId -> Port
out0 nid = InstPort nid "0"

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
  NCallGroup{}-> n{ nName = l }
  NCallSnd{}  -> n{ nName = l }
  NRetSnd{}   -> n{ nName = l }
  NRet{}      -> n{ nName = l }
  NTagVal{}   -> n{ nName = l }
  NValTag{}   -> n{ nName = l }
  NCpHToDev{} -> n{ nName = l }
  NCpDevToH{} -> n{ nName = l }
  NCommit{}   -> n{ nName = l }
  NStopSpec{} -> n{ nName = l }
  NArg{}      -> n{ nName = l }
  NSuper{}    -> n{ nName = l }

-- codificação aritmética de pares/listas --------------------------------

pairBase :: Int
pairBase = 1000003

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
  qN <- bin2Node "div" (NDiv "") p k
  mN <- bin2Node "mul" (NMul "") (out0 qN) k
  bin2 "sub" (NSub "") p (out0 mN)

-- foldrM local
foldrM' :: (a -> b -> Build b) -> b -> [a] -> Build b
foldrM' f z0 = go
  where
    go []     = pure z0
    go (y:ys) = do r <- go ys
                   f y r

-- API ------------------------------------------------------------------

buildProgram :: Program -> DFG
buildProgram (Program decls) =
  let (_, st) = runBuild $ do
        mapM_ goDecl decls
        -- liga main -> ret (raiz do grafo)
        mb <- lookupB "main"
        case mb of
          Just (BPort p) -> do
            r <- newNode "ret" (NRet "")
            connectPlus p (InstPort r "0")
          _ -> pure ()
  in bsGraph st

-- Declarações ----------------------------------------------------------

goDecl :: Decl -> Build ()
goDecl (FunDecl f ps body)
  | null ps   = do p <- withEnv (goExpr body)
                   insertB f (BPort p)
  | otherwise = insertB f (BLam ps body)

-- Expressões -----------------------------------------------------------

goExpr :: Expr -> Build Port
goExpr = \case
  Var x -> do
    mb <- lookupB x
    pure $ case mb of
      Just (BPort p)  -> p
      _               -> InstPort (-1) x

  Lit lit -> litNode lit

  Lambda ps e ->
    withEnv $ do
      mapM_ (\v -> insertB v (BPort (InstPort (-1) v))) ps
      goExpr e

  If c t e -> do
    pc <- goExpr c
    vt <- withEnv (goExpr t)
    ve <- withEnv (goExpr e)

    stT <- newNode "steer" (NSteer "")
    connect vt (InstPort stT "0")
    connect pc (InstPort stT "1")
    let outT = SteerPort stT "t"

    stF <- newNode "steer" (NSteer "")
    connect ve (InstPort stF "0")
    connect pc (InstPort stF "1")
    let outF = SteerPort stF "f"

    registerAlias outT [outF]
    pure outT

  -- listas / tuplas via codificação aritmética
  Cons a b -> do pa <- goExpr a; pb <- goExpr b; pairEnc pa pb
  List xs  -> do z <- nilP; es <- mapM goExpr xs; foldrM' pairEnc z es
  Tuple [a,b] -> do pa <- goExpr a; pb <- goExpr b; pairEnc pa pb
  Tuple (a:_) -> goExpr a
  Tuple []    -> constI 0

  Case scr alts -> compileCase scr alts

  Let decls body -> withEnv (mapM_ goDecl decls >> goExpr body)

  App f x -> let (g,args) = flattenApp (App f x) in goApp g args

  BinOp op l r -> case op of
      Add -> go2 (NAdd  "") ; Sub -> go2 (NSub  "")
      Mul -> go2 (NMul  "") ; Div -> go2 (NDiv  "")
      Mod -> do
        pl <- goExpr l; pr <- goExpr r
        qN <- bin2Node "div" (NDiv "") pl pr
        mN <- bin2Node "mul" (NMul "") (out0 qN) pr
        bin2 "sub" (NSub "") pl (out0 mN)
      Eq  -> go2 (NEqual"") ; Lt  -> go2 (NLThan"")
      Gt  -> go2 (NGThan"") ; And -> go2 (NBand "")
      Or  -> do pl <- goExpr l; pr <- goExpr r; orP pl pr
      Le  -> do pl <- goExpr l; pr <- goExpr r
                lt <- bin2 "lthan" (NLThan "") pl pr
                eq <- bin2 "equal" (NEqual "") pl pr
                orP lt eq
      Ge  -> do pl <- goExpr l; pr <- goExpr r
                gt <- bin2 "gthan" (NGThan "") pl pr
                eq <- bin2 "equal" (NEqual "") pl pr
                orP gt eq
      Neq -> do pl <- goExpr l; pr <- goExpr r
                eq <- bin2 "equal" (NEqual "") pl pr
                notP eq
    where
      go2 nd = do pl <- goExpr l; pr <- goExpr r; bin2 "op" nd pl pr

  UnOp u e -> do
    pe <- goExpr e
    case u of
      Neg -> do z <- constI 0; bin2 "sub" (NSub "") z pe
      Not -> notP pe

  _ -> constI 0

-- Aplicação n-ária -----------------------------------------------------

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

-- taskId determinístico
funTaskId :: Ident -> Int
funTaskId ident = foldl (\h c -> h * 131 + fromEnum c) 7 (show ident)

-- cria um nó de argumento formal e retorna sua porta
argPort :: String -> Int -> Build Port
argPort fun i = do
  nid <- newNode ("arg " ++ fun ++ "#" ++ show i) (NArg (fun ++ "#" ++ show i))
  pure (out0 nid)

goApp :: Expr -> [Expr] -> Build Port
goApp fun args = case fun of
  Var f -> do
    argv <- mapM goExpr args

    -- tag da chamada
    let tid = funTaskId f
    cg <- newNode ("callgroup " ++ f) (NCallGroup f)
    let tag = out0 cg

    -- envia argumentos
    forM_ (zip [0..] argv) $ \(i,a) -> do
      cs <- newNode ("callsnd " ++ f ++ "#" ++ show i) (NCallSnd (f ++ "#" ++ show i) tid)
      connectPlus a   (InstPort cs "0")
      connectPlus tag (InstPort cs "1")

    -- nó de retorno
    rs <- newNode ("retsnd " ++ f) (NRetSnd f tid)
    connectPlus tag (InstPort rs "1")   -- porta 1 = tag

    mb <- lookupB f
    case mb of
      Just (BLam ps body) -> do
        cyc <- isActive f
        -- compila corpo com nós de argumento
        res <- withActive f $ withEnv $ do
                 forM_ (zip [0..] ps) $ \(i, v) -> do
                   ap <- argPort f i
                   insertB v (BPort ap)
                 goExpr body

        -- valor do corpo vai para retsnd (porta 0)
        connectPlus res (InstPort rs "0")

        -- valor observado da aplicação
        if cyc
          then pure (out0 rs)  -- em recursão: observe o retsnd
          else pure res        -- fora de recursão: observe o corpo (ex.: + → ret)
      _ -> do
        -- variável não-lambda: ainda deixa o retsnd como valor
        pure (out0 rs)

  Lambda ps body -> do
    -- nome sintético simples
    let fname = "lambda"
        tid   = funTaskId fname
    argv <- mapM goExpr args
    cg <- newNode ("callgroup " ++ fname) (NCallGroup fname)
    let tag = out0 cg
    forM_ (zip [0..] argv) $ \(i,a) -> do
      cs <- newNode ("callsnd " ++ fname ++ "#" ++ show i) (NCallSnd (fname ++ "#" ++ show i) tid)
      connectPlus a   (InstPort cs "0")
      connectPlus tag (InstPort cs "1")
    rs <- newNode ("retsnd " ++ fname) (NRetSnd fname tid)
    connectPlus tag (InstPort rs "1")
    res <- withEnv $ do
             forM_ (zip [0..] ps) $ \(i,v) -> argPort fname i >>= \p -> insertB v (BPort p)
             goExpr body
    connectPlus res (InstPort rs "0")
    pure res

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

-- Literais -------------------------------------------------------------

litNode :: Literal -> Build Port
litNode = \case
  LInt n    -> constI n
  LFloat d  -> newNode "fconst" (NConstF "" (realToFrac d)) >>= \nid -> pure (out0 nid)
  LChar c   -> constI (fromEnum c)
  LString _ -> constI 0
  LBool b   -> constI (if b then 1 else 0)

-- Helpers --------------------------------------------------------------

bin2 :: String -> DNode -> Port -> Port -> Build Port
bin2 _lbl nd a b = out0 <$> bin2Node "b2" nd a b

bin2Node :: String -> DNode -> Port -> Port -> Build NodeId
bin2Node lbl nd a b = do
  nid <- newNode lbl nd
  connectPlus a (InstPort nid "0")
  connectPlus b (InstPort nid "1")
  pure nid

notP :: Port -> Build Port
notP x = do z <- constI 0; bin2 "equal" (NEqual "") x z

orP :: Port -> Port -> Build Port
orP a b = do s <- bin2Node "add" (NAdd "") a b
             z <- constI 0
             bin2 "gthan" (NGThan "") (out0 s) z

andP :: Port -> Port -> Build Port
andP a b = bin2 "band" (NBand "") a b

trueP, falseP :: Build Port
trueP  = constI 1
falseP = constI 0

-- CASE / Pattern matching ----------------------------------------------

compileCase :: Expr -> [(Pattern, Expr)] -> Build Port
compileCase scr alts = do
  pscr <- goExpr scr
  taken0 <- falseP
  outs <- goAlts pscr taken0 alts []
  case outs of
    []       -> falseP
    (h:rest) -> registerAlias h rest >> pure h
  where
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
  PTuple [p1,p2] -> do
    a <- fstDec scr; b <- sndDec scr; t <- trueP
    pure (t, bindIfVar p1 a ++ bindIfVar p2 b)
  PList [] -> do
    p <- isNilP scr; pure (p, [])
  PList [p] -> do
    nz   <- notP =<< isNilP scr
    tl   <- sndDec scr
    isTl <- isNilP tl
    g    <- andP nz isTl
    hd   <- fstDec scr
    pure (g, bindIfVar p hd)
  PCons ph pt -> do
    nz <- notP =<< isNilP scr
    hd <- fstDec scr
    tl <- sndDec scr
    pure (nz, bindIfVar ph hd ++ bindIfVar pt tl)
  PWildcard -> trueP >>= \t -> pure (t, [])
  PVar x    -> trueP >>= \t -> pure (t, [(x, scr)])
  PLit lit  -> do litP <- litNode' lit
                  eq   <- bin2 "equal" (NEqual "") scr litP
                  pure (eq, [])
  _         -> trueP >>= \t -> pure (t, [])

bindIfVar :: Pattern -> Port -> [(Ident, Port)]
bindIfVar (PVar x) p = [(x,p)]
bindIfVar _        _ = []

-- versão de literal que devolve Port direto (usada em patPred)
litNode' :: Literal -> Build Port
litNode' = litNode
