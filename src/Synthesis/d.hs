{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Synthesis.Builder (buildProgram) where

import           Prelude                 hiding (id)
import           Control.Monad.State.Strict
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import           Debug.Trace (traceShowId, traceM)

import           Types
import           Node
import           Port
import           Unique
import qualified Syntax                  as S

-------------------------------------------------------------------------------
-- Estado e helpers
-------------------------------------------------------------------------------

data BState = BState
  { g      :: !(DGraph DNode)
  , env    :: !(Map S.Ident Port)
  , lifted :: ![S.Decl]
  , lcount :: !Int
  }

type Builder = StateT BState Unique
emptyB :: BState
emptyB = BState emptyGraph Map.empty [] 0

freshNid :: Builder NodeId
freshNid = lift freshId

addN :: DNode -> Builder ()
addN n = modify' $ \s -> s { g = addNode (nId n) n (g s) }

addE :: Edge -> Builder ()
addE e = modify' $ \s -> s { g = addEdge e (g s) }

bindVar :: S.Ident -> Port -> Builder ()
bindVar x p = modify' $ \s -> s { env = Map.insert x p (env s) }

lookupVar :: S.Ident -> Builder Port
lookupVar x = do
  m <- gets (Map.lookup x . env)
  case m of
    Just p  -> pure p
    Nothing -> do
      e <- gets env
      error ("[Builder] unbound: " <> x <> " | Env: " <> show (Map.keys e))

litNode :: Literal -> Builder Port
litNode lit = do n <- freshNid
                 addN (InstConst n lit)
                 pure (InstPort n outPort)

boolConst :: Bool -> Builder Port
boolConst = litNode . LBool

-------------------------------------------------------------------------------
-- EXPRESSÕES SEM SUPORTE A LAMBDA/LIST/CASO/SUPER
-------------------------------------------------------------------------------

compileExpr :: S.Expr -> Builder [Port]
compileExpr = \case
  S.Lambda _ _   -> error "[Builder] Lambda não suportado."
  S.Var x        -> (:[]) <$> lookupVar x
  S.Lit l        -> (:[]) <$> litNode (convLit l)
  S.BinOp o a b  -> do
    pa <- compileExpr a
    pb <- compileExpr b
    let (pL, pR) = (get1 pa "binop LHS", get1 pb "binop RHS")
    n <- freshNid
    addN (InstBinop n (convOp o) (portNode pL) (portNode pR))
    addE (pL --> InstPort n "lhs")
    addE (pR --> InstPort n "rhs")
    pure [InstPort n outPort]
  S.UnOp o e     -> do
    pe <- compileExpr e
    let p = get1 pe "unop"
    n <- freshNid
    addN (InstUnary n (convUn o) (portNode p))
    addE (p --> InstPort n "arg")
    pure [InstPort n outPort]
  S.Cons _ _     -> error "[Builder] Cons não suportado."
  S.If _ _ _     -> error "[Builder] If não suportado."
  S.Tuple _      -> error "[Builder] Tuple não suportado."
  S.List _       -> error "[Builder] List literal não suportado."
  S.Let ds e     -> do
    old <- gets env
    ps <- compileLet ds e
    modify' (\s -> s { env = old })
    pure ps
  S.Case _ _     -> error "[Builder] Case não suportado."
  S.App f a      -> do
    f' <- stripLambdas f
    a' <- stripLambdas a
    compileAppMulti (S.App f' a')

get1 :: [Port] -> String -> Port
get1 [x] _ = x
get1 xs what = error $ "[Builder] Esperado exatamente um valor em " ++ what ++ ", mas recebi: " ++ show (length xs)

-------------------------------------------------------------------------------
-- FLATTEN APP (sem suporte especial)
-------------------------------------------------------------------------------

flattenApp :: S.Expr -> (S.Expr, [S.Expr])
flattenApp = go []
  where
    go acc (S.App f a) = go (a:acc) f
    go acc f           = (f, acc)

compileAppMulti :: S.Expr -> Builder [Port]
compileAppMulti app =
  let (fun, args) = flattenApp app in
  case fun of
    S.Var fname -> compileCall fname args
    _ -> error $ "[Builder] Function application must be Var: " <> show fun

stripLambdas :: S.Expr -> Builder S.Expr
stripLambdas = \case
  S.Lambda _ _ -> error "[Builder] Lambda não suportado."
  S.App f a -> do
    f' <- stripLambdas f
    a' <- stripLambdas a
    pure (S.App f' a')
  S.Let ds e -> do
    ds' <- mapM (\(S.FunDecl n ps b) -> do b' <- stripLambdas b; pure (S.FunDecl n ps b)) ds
    e' <- stripLambdas e
    pure (S.Let ds' e')
  S.BinOp op a b -> do
    a' <- stripLambdas a
    b' <- stripLambdas b
    pure (S.BinOp op a' b')
  S.UnOp op e -> do
    e' <- stripLambdas e
    pure (S.UnOp op e')
  x -> pure x

-------------------------------------------------------------------------------
-- LET
-------------------------------------------------------------------------------

compileLet :: [S.Decl] -> S.Expr -> Builder [Port]
compileLet ds e = do
  mapM_ compileLocalDecl ds
  compileExpr e

compileLocalDecl :: S.Decl -> Builder ()
compileLocalDecl (S.FunDecl f ps body) = compileNamedFun f ps body

-------------------------------------------------------------------------------
-- CHAMADA DE FUNÇÃO
-------------------------------------------------------------------------------

compileCall :: S.Ident -> [S.Expr] -> Builder [Port]
compileCall f args = do
  cgNid <- freshNid
  addN (InstCallGroup cgNid (T.pack f))
  argPsList <- mapM compileExpr args
  let argPs = map (\ps -> get1 ps "fun arg") argPsList
  callsnds <- forM argPs $ \p -> do
    sndNid <- freshNid
    addN (InstCallSnd sndNid (T.pack f) cgNid (portNode p))
    addE (p --> InstPort sndNid "in0")
    pure (InstPort sndNid outPort)
  retsndNid <- freshNid
  addN (InstRetSnd retsndNid (T.pack f) cgNid (portNode (last callsnds)))
  pure [InstPort retsndNid outPort]

-------------------------------------------------------------------------------
-- Declarações top-level + compileNamedFun
-------------------------------------------------------------------------------
compileTopDecl :: S.Decl -> Builder ()
compileTopDecl (S.FunDecl f ps body) = compileNamedFun f ps body

compileNamedFun :: S.Ident -> [S.Ident] -> S.Expr -> Builder ()
compileNamedFun f ps body = do
  n <- freshNid
  let ins = [ InstPort n ("in"<>show i) | i <- [0..length ps-1] ]
  old <- gets env
  modify' (\s -> s { env = Map.union (Map.fromList (zip ps ins)) (env s) })
  _ <- compileExpr body
  modify' (\s -> s { env = old })
  bindVar f (InstPort n "out0")

-------------------------------------------------------------------------------
-- Conversão lit/op
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- buildProgram e fixpoint de lambdas
-------------------------------------------------------------------------------
buildProgram :: S.Program -> DGraph DNode
buildProgram (S.Program ds) =
  let graph = evalUnique $ fmap g $ execStateT (fixLiftingAll ds) emptyB
      nodes = Map.toAscList (dgNodes graph)
      _ = traceM "=== [DEBUG] Nodes criados pelo Builder ==="
      _ = mapM_ (\(nid, dnode) -> traceM $ "NodeId: " ++ show nid ++ " | " ++ show dnode
                                ++ " | Deps: " ++ show (nodeDeps dnode)) nodes
  in graph
  
fixLiftingAll :: [S.Decl] -> Builder ()
fixLiftingAll decls = go decls
  where
    go [] = pure ()
    go todo = do
      predeclare todo
      mapM_ compileTopDecl todo
      news <- gets lifted
      if null news
        then pure ()
        else do
          modify' $ \s -> s { lifted = [] }
          go (reverse news)

predeclare :: [S.Decl] -> Builder ()
predeclare =
  mapM_ $ \(S.FunDecl f ps _) -> do
    n <- freshNid
    addN (InstSuper n "<PREDECLARE>" [] 1)  -- dummy node para garantir presença no grafo
    bindVar f (InstPort n "out0")
    
-- Função auxiliar para extrair dependências de um nó
nodeDeps :: DNode -> [NodeId]
nodeDeps = \case
  InstConst{} -> []
  InstBinop{lhs, rhs} -> [lhs, rhs]
  InstBinopI{lhs} -> [lhs]
  InstUnary{arg} -> [arg]
  InstSuper{ins} -> ins
  InstSteer{predN} -> [predN]
  InstIncTag{base} -> [base]
  InstTuple{fields} -> fields
  InstProj{tuple} -> [tuple]
  InstCallGroup{} -> []
  InstCallSnd{argId, groupId} -> [argId, groupId]
  InstRetSnd{retValId, groupId} -> [retValId, groupId]
  InstRet{valId, retSndIds} -> valId : retSndIds
  InstPar{} -> []
