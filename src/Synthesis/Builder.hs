{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Synthesis.Builder
  ( DFG
  , buildProgram
  ) where

import           Prelude hiding (lookup)
import           Control.Monad          (forM_, foldM)
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

data BuildS = BuildS
  { bsGraph :: !DFG
  , bsEnv   :: ![Env]
  }

emptyS :: BuildS
emptyS = BuildS emptyGraph [M.empty]

newtype Build a = Build { unBuild :: StateT BuildS Unique a }
  deriving (Functor, Applicative, Monad)

runBuild :: Build a -> (a, BuildS)
runBuild m = evalUnique (runStateT (unBuild m) emptyS)

-- escopo ---------------------------------------------------------------

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

emit :: Edge -> Build ()
emit e = Build $ modify (\s -> s { bsGraph = addEdge e (bsGraph s) })

connect :: Port -> Port -> Build ()
connect a b = emit (a --> b)

-- nós ------------------------------------------------------------------

newNode :: String -> DNode -> Build NodeId
newNode label node = Build $ do
  s   <- get
  nid <- lift freshId
  let g' = addNode nid (setName label node) (bsGraph s)
  put s { bsGraph = g' }
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
  NSuper{}    -> n{ nName = l }

-- API ------------------------------------------------------------------

buildProgram :: Program -> DFG
buildProgram (Program decls) =
  let ((), st) = runBuild (mapM_ goDecl decls)
  in bsGraph st

-- Declarações ----------------------------------------------------------

goDecl :: Decl -> Build ()
goDecl (FunDecl f ps body)
  | null ps   = do                 -- nullary: materializa agora
      p <- withEnv (goExpr body)
      insertB f (BPort p)
  | otherwise =                    -- com parâmetros: guarda lambda
      insertB f (BLam ps body)

-- Expressões -----------------------------------------------------------

goExpr :: Expr -> Build Port
goExpr = \case
  Var x -> do
    mb <- lookupB x
    case mb of
      Just (BPort p) -> pure p
      Just (BLam _ _) -> pure (InstPort (-1) x)
      Nothing         -> pure (InstPort (-1) x)

  Lit lit ->
    litNode lit

  Lambda ps e ->
    withEnv $ do
      mapM_ (\v -> insertB v (BPort (InstPort (-1) v))) ps
      _ <- goExpr e
      pure (InstPort (-1) "<lambda>")

  If c t e -> do
    pc  <- goExpr c
    sid <- newNode "steer" (NSteer "")
    connect pc (InstPort sid "0")
    _ <- withEnv (goExpr t)
    _ <- withEnv (goExpr e)
    pure (SteerPort sid "t")

  Cons a _b ->
    goExpr a

  List xs ->
    case xs of
      []    -> newNode "const_0" (NConstI "" 0) >>= \nid -> pure (out0 nid)
      (y:_) -> goExpr y

  Tuple xs ->
    case xs of
      []    -> newNode "const_unit" (NConstI "" 0) >>= \nid -> pure (out0 nid)
      (y:_) -> goExpr y

  Case scr alts -> do
    _ <- goExpr scr
    case alts of
      []        -> newNode "const_0" (NConstI "" 0) >>= \nid -> pure (out0 nid)
      ((_,r):_) -> withEnv (goExpr r)

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
      Mod -> do
        nid <- bin2Node "div" (NDiv "") pl pr
        pure (out1 nid)
      Eq  -> bin2 "equal" (NEqual "") pl pr
      Lt  -> bin2 "lthan" (NLThan "") pl pr
      Gt  -> bin2 "gthan" (NGThan "") pl pr
      And -> bin2 "band"  (NBand "") pl pr
      Or  -> bin2 "band"  (NBand "") pl pr
      Le  -> bin2 "lthan" (NLThan "") pl pr
      Ge  -> bin2 "gthan" (NGThan "") pl pr
      Neq -> bin2 "equal" (NEqual "") pl pr

  UnOp u e -> do
    pe <- goExpr e
    case u of
      Neg -> do
        z <- newNode "const_0" (NConstI "" 0)
        bin2 "sub" (NSub "") (out0 z) pe
      Not -> do
        one <- newNode "const_1" (NConstI "" 1)
        bin2 "equal" (NEqual "") pe (out0 one)

  _ ->
    newNode "const_0" (NConstI "" 0) >>= \nid -> pure (out0 nid)

-- Aplicação n-ária -----------------------------------------------------

flattenApp :: Expr -> (Expr, [Expr])
flattenApp = \case
  App f x ->
    let (g, xs) = flattenApp f
    in (g, xs ++ [x])
  e -> (e, [])

goApp :: Expr -> [Expr] -> Build Port
goApp fun args = case fun of
  Var f -> do
    mb <- lookupB f
    case mb of
      Just (BLam ps body) -> applyLambda ps body args
      _ -> do
        _ <- mapM goExpr args
        case args of
          [] -> newNode "const_unit" (NConstI "" 0) >>= \nid -> pure (out0 nid)
          _  -> goExpr (last args)
  Lambda ps body ->
    applyLambda ps body args
  _ -> do
    _ <- mapM goExpr args
    case args of
      [] -> newNode "const_unit" (NConstI "" 0) >>= \nid -> pure (out0 nid)
      _  -> goExpr (last args)

applyLambda :: [Ident] -> Expr -> [Expr] -> Build Port
applyLambda ps body args = do
  let (use, rest) = splitAt (length ps) args
  argv <- mapM goExpr use
  withEnv $ do
    forM_ (zip ps argv) $ \(v,p) -> insertB v (BPort p)
    res <- goExpr body
    -- se sobrar, só devolvemos res (subset atual não compõe de novo)
    foldM (\acc _ -> pure acc) res rest

-- Literais -------------------------------------------------------------

litNode :: Literal -> Build Port
litNode = \case
  LInt n    -> newNode ("const_" ++ show n) (NConstI "" n) >>= \nid -> pure (out0 nid)
  LFloat d  -> newNode "fconst" (NConstF "" (realToFrac d)) >>= \nid -> pure (out0 nid)
  LChar c   -> newNode ("const_" ++ show (fromEnum c)) (NConstI "" (fromEnum c)) >>= \nid -> pure (out0 nid)
  LString _ -> newNode "const_str" (NConstI "" 0) >>= \nid -> pure (out0 nid)
  LBool b   -> newNode ("const_" ++ if b then "1" else "0") (NConstI "" (if b then 1 else 0)) >>= \nid -> pure (out0 nid)

-- Helpers binários -----------------------------------------------------

bin2 :: String -> DNode -> Port -> Port -> Build Port
bin2 label node a b = do
  nid <- bin2Node label node a b
  pure (out0 nid)

bin2Node :: String -> DNode -> Port -> Port -> Build NodeId
bin2Node label node a b = do
  nid <- newNode label node
  connect a (InstPort nid "0")
  connect b (InstPort nid "1")
  pure nid
