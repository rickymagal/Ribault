{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synthesis.Builder
  ( DFG
  , buildProgram
  ) where

import           Prelude hiding (lookup)
import           Control.Monad          (forM_, zipWithM_, when)
import           Control.Monad.State    (StateT, get, put, runStateT, gets, modify)
import           Control.Monad.Trans    (lift)
import qualified Data.Map              as M
import qualified Data.Set              as S

import           Semantic              (assignSuperNames)
import           Syntax
import           Types                  (DGraph(..), Edge, NodeId, emptyGraph, addNode, addEdge)
import           Port                   (Port(..), (-->))
import           Unique                 (Unique, evalUnique, freshId)
import           Node                   (DNode(..))

-- Grafo final
type DFG = DGraph DNode

-- Ambiente
data Binding = BPort Port | BLam [Ident] Expr
type Env = M.Map Ident Binding

type AliasMap    = M.Map (NodeId, String) [Port]
type ActiveStack = [Ident]

data BuildS = BuildS
  { bsGraph      :: !DFG
  , bsEnv        :: ![Env]
  , bsAliases    :: !AliasMap
  , bsActive     :: !ActiveStack
  , bsFloatFuns  :: !(S.Set Ident)
  }

emptyS :: BuildS
emptyS = BuildS emptyGraph [M.empty] M.empty [] S.empty

newtype Build a = Build { unBuild :: StateT BuildS Unique a }
  deriving (Functor, Applicative, Monad)

runBuild :: Build a -> (a, BuildS)
runBuild m = evalUnique (runStateT (unBuild m) emptyS)

-- escopo ----------------------------------------------------------------

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

-- marcação de função float ---------------------------------------------

markFloatFun :: Ident -> Build ()
markFloatFun f = Build $ modify (\s -> s { bsFloatFuns = S.insert f (bsFloatFuns s) })

isFloatFun :: Ident -> Build Bool
isFloatFun f = Build $ gets (\s -> S.member f (bsFloatFuns s))

isFloatActive :: Build Bool
isFloatActive = Build $ gets $ \s -> case bsActive s of
  (f:_) -> S.member f (bsFloatFuns s)
  _     -> False

-- helper: está construindo 'f' agora?
isActiveFun :: Ident -> Build Bool
isActiveFun f = Build $ gets (\s -> f `elem` bsActive s)

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
  NFSub{}     -> n{ nName = l }
  NFMul{}     -> n{ nName = l }
  NFDiv{}     -> n{ nName = l }
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

-- codificação pares/listas ---------------------------------------------

pairBase :: Int
pairBase = 1000003

constI :: Int -> Build Port
constI k = newNode ("const_" ++ show k) (NConstI "" k) >>= \nid -> pure (out0 nid)

-- immediates (sem aliases!)
addI :: Port -> Int -> Build Port
addI p k = do
  nid <- newNode ("addi_" ++ show k) (NAddI "" k)
  connect p (InstPort nid "0")
  pure (out0 nid)

subI :: Port -> Int -> Build Port
subI p k = do
  nid <- newNode ("subi_" ++ show k) (NSubI "" k)
  connect p (InstPort nid "0")
  pure (out0 nid)

-- fmuli helper (sem aliases!)
fmulI :: Port -> Float -> Build Port
fmulI p k = do
  nid <- newNode ("fmuli_" ++ show k) (NFMulI "" k)
  connect p (InstPort nid "0")
  pure (out0 nid)

nilP :: Build Port
nilP = constI (-1)

isNilP :: Port -> Build Port
isNilP xs = do
  n <- nilP
  bin2 "equal" (NEqual "") xs n

-- cons(a,b) = (a+2)*B + (b+2)
pairEnc :: Port -> Port -> Build Port
pairEnc a b = do
  a2 <- addI a 2
  b2 <- addI b 2
  k  <- constI pairBase
  m  <- bin2Node "mul" (NMul "") a2 k
  bin2 "add" (NAdd "") (out0 m) b2

fstDec :: Port -> Build Port
fstDec p = do
  k  <- constI pairBase
  qN <- bin2Node "div" (NDiv "") p k
  subI (out0 qN) 2

sndDec :: Port -> Build Port
sndDec p = do
  k  <- constI pairBase
  qN <- bin2Node "div" (NDiv "") p k
  mN <- bin2Node "mul" (NMul "") (out0 qN) k
  r  <- bin2 "sub" (NSub "") p (out0 mN)
  subI r 2

-- foldrM local
foldrM' :: (a -> b -> Build b) -> b -> [a] -> Build b
foldrM' f z0 = go
  where
    go []     = pure z0
    go (y:ys) = do r <- go ys
                   f y r

-- API ------------------------------------------------------------------

buildProgram :: Program -> DFG
buildProgram p0 =
  let Program decls = assignSuperNames p0
      (_, st) = runBuild $ do
        mapM_ (\(FunDecl f ps body) -> insertB f (BLam ps body)) decls
        mapM_ buildZero decls
  in bsGraph st
  where
    buildZero (FunDecl f ps body) | null ps = do
      p <- withEnv (goExpr body)
      insertB f (BPort p)
      r <- newNode f (NRet f)
      connectPlus p (InstPort r "0")
    buildZero _ = pure ()

-- função já construída?
funBuilt :: Ident -> Build Bool
funBuilt f = Build $ gets $ \s -> any isRet (M.toList (dgNodes (bsGraph s)))
  where isRet (_, n) = case n of { NRet f' -> f' == f; _ -> False }

-- evita reentrada recursiva
ensureBuilt :: Ident -> [Ident] -> Expr -> Build ()
ensureBuilt f ps body = do
  done   <- funBuilt f
  active <- isActiveFun f
  if done || active
     then pure ()
     else do
       _ <- withActive f $ withEnv $ do
              forM_ (zip [0..] ps) $ \(i, v) -> do
                a <- argNode f i
                insertB v (BPort a)
              res <- goExpr body
              r   <- newNode f (NRet f)
              connectPlus res (InstPort r "0")
       pure ()

-- variável livre -> NArg
freeVar :: Ident -> Build Port
freeVar x = do
  nid <- newNode x (NArg x)
  let p = out0 nid
  insertB x (BPort p)
  pure p

-- detecta se um Port carrega float (por origem)
portIsFloat :: Port -> Build Bool
portIsFloat p = Build $ gets $ \s -> case M.lookup (pNode p) (dgNodes (bsGraph s)) of
  Just NConstF{}     -> True
  Just NFAdd{}       -> True
  Just NFSub{}       -> True
  Just NFMul{}       -> True
  Just NFDiv{}       -> True
  Just (NRetSnd f _) -> S.member f (bsFloatFuns s)
  _                  -> False

isFloatContext :: Port -> Port -> Build Bool
isFloatContext a b = do
  af <- isFloatActive
  pa <- portIsFloat a
  pb <- portIsFloat b
  pure (af || pa || pb)

-- Expressões -----------------------------------------------------------

goExpr :: Expr -> Build Port
goExpr = \case
  Var x -> do
    lookupB x >>= \case
      Just (BPort p) -> pure p
      Just (BLam ps body) ->
        if null ps
          then do p <- withEnv (goExpr body)
                  insertB x (BPort p)
                  pure p
          else freeVar x
      Nothing -> freeVar x

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

  -- listas / tuplas
  Cons a b     -> do pa <- goExpr a; pb <- goExpr b; pairEnc pa pb
  List xs      -> do z <- nilP; es <- mapM goExpr xs; foldrM' pairEnc z es
  Tuple [a,b]  -> do pa <- goExpr a; pb <- goExpr b; pairEnc pa pb
  Tuple (a:_)  -> goExpr a
  Tuple []     -> constI 0

  Case scr alts -> compileCase scr alts

  Let decls body -> withEnv (mapM_ goDecl decls >> goExpr body)

  App f x -> let (g,args) = flattenApp (App f x) in goApp g args

  BinOp op l r -> do
    pl <- goExpr l
    pr <- goExpr r
    fctx <- isFloatContext pl pr
    case op of
      Add | fctx      -> bin2 "fadd" (NFAdd "")  pl pr
          | otherwise -> bin2 "add"  (NAdd  "")  pl pr
      Sub | fctx      -> do ny <- fmulI pr (-1.0)     -- fsub = fadd(x, y*-1)
                            bin2 "fadd" (NFAdd "") pl ny
          | otherwise -> bin2 "sub"  (NSub  "")  pl pr
      Mul | fctx      -> bin2 "mul"  (NMul  "")  pl pr  -- sem fmult no ASM
          | otherwise -> bin2 "mul"  (NMul  "")  pl pr
      Div | fctx      -> bin2 "div"  (NDiv  "")  pl pr  -- sem fdiv no ASM
          | otherwise -> bin2 "div"  (NDiv  "")  pl pr
      Mod -> do
        qN <- bin2Node "div" (NDiv "") pl pr
        mN <- bin2Node "mul" (NMul "") (out0 qN) pr
        bin2 "sub" (NSub "") pl (out0 mN)
      Eq  -> bin2 "equal" (NEqual "") pl pr
      Lt  -> bin2 "lthan" (NLThan "") pl pr
      Gt  -> bin2 "gthan" (NGThan "") pl pr
      And -> bin2 "band"  (NBand  "") pl pr
      Or  -> do s <- bin2Node "add" (NAdd "") pl pr
                z <- constI 0
                bin2 "gthan" (NGThan "") (out0 s) z
      Le  -> do lt <- bin2 "lthan" (NLThan "") pl pr
                eq <- bin2 "equal" (NEqual "")  pl pr
                orP lt eq
      Ge  -> do gt <- bin2 "gthan" (NGThan "") pl pr
                eq <- bin2 "equal" (NEqual "")  pl pr
                orP gt eq
      Neq -> do eq <- bin2 "equal" (NEqual "")  pl pr
                notP eq

  UnOp u e -> do
    pe <- goExpr e
    case u of
      Neg -> do z <- constI 0; bin2 "sub" (NSub "") z pe
      Not -> notP pe

  Super nm kind inp out _ -> do
    pIn <- goExpr (Var inp)
    nid <- naryNode nm NSuper
             { nName     = ""
             , superNum  = 0
             , superOuts = 1
             , superSpec = case kind of { SuperParallel -> True; SuperSingle -> False }
             , superImm  = Nothing
             } [pIn]
    insertB out (BPort (out0 nid))
    pure (out0 nid)

-- Declarações ----------------------------------------------------------

goDecl :: Decl -> Build ()
goDecl (FunDecl f ps body) = insertB f (BLam ps body)

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

-- taskId determinístico
funTaskId :: Ident -> Int
funTaskId ident = foldl (\h c -> h * 131 + fromEnum c) 7 (show ident)

-- nó de argumento formal
argNode :: String -> Int -> Build Port
argNode fun i = Build $ do
  s <- get
  let nm    = fun ++ "#" ++ show i
      found = [ nid
              | (nid, n) <- M.toList (dgNodes (bsGraph s))
              , case n of
                  NArg nm' -> nm' == nm
                  _        -> False
              ]
  nid <- case found of
           (h:_) -> pure h
           []    -> do
             nid' <- lift freshId
             let g' = addNode nid' (NArg nm) (bsGraph s)
             put s { bsGraph = g' }
             pure nid'
  pure (out0 nid)

-- formal + alias para o real
bindFormal :: String -> Int -> Ident -> Port -> Build ()
bindFormal fun i formal actual = do
  ap <- argNode fun i
  insertB formal (BPort ap)
  case ap of
    InstPort nid _ -> connectPlus actual (InstPort nid "0")
    _              -> pure ()

goApp :: Expr -> [Expr] -> Build Port
goApp fun args = case fun of
  Var f -> do
    argv <- mapM goExpr args
    anyFloat <- or <$> mapM portIsFloat argv
    when anyFloat (markFloatFun f)

    lookupB f >>= \case
      Just (BLam ps body) -> ensureBuilt f ps body
      _                   -> pure ()

    let tid = funTaskId f
    cg <- newNode f (NCallGroup f)
    let tag = out0 cg

    forM_ (zip [0..] argv) $ \(i,a) -> do
      cs <- newNode (f ++ "#" ++ show i) (NCallSnd (f ++ "#" ++ show i) tid)
      connectPlus a   (InstPort cs "0")
      connectPlus tag (InstPort cs "1")

    rs <- newNode f (NRetSnd f tid)
    connectPlus tag (InstPort rs "1")
    pure (out0 rs)

  Lambda ps body -> do
    let fname = "lambda"
        tid   = funTaskId fname
    argv <- mapM goExpr args
    cg <- newNode fname (NCallGroup fname)
    let tag = out0 cg
    forM_ (zip [0..] argv) $ \(i,a) -> do
      cs <- newNode (fname ++ "#" ++ show i) (NCallSnd (fname ++ "#" ++ show i) tid)
      connectPlus a   (InstPort cs "0")
      connectPlus tag (InstPort cs "1")
    rs <- newNode fname (NRetSnd fname tid)
    connectPlus tag (InstPort rs "1")
    res <- withEnv $ do
             forM_ (zip3 [0..] ps argv) $ \(i,v,p) -> bindFormal fname i v p
             goExpr body
    connectPlus res (InstPort rs "0")
    pure res

  _ -> do
    _ <- mapM goExpr args
    case args of
      [] -> constI 0
      _  -> goExpr (last args)

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

litNode' :: Literal -> Build Port
litNode' = litNode
