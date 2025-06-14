{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Módulo de geração de grafo dataflow a partir de AST desugarizada no estilo do Trebuchet.
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Char      (isAlphaNum)
import           Syntax         (Program(..), Decl(..), Expr(..), Literal(..), Ident, BinOperator(..), Pattern(..))
import           Semantic       (desugarProgram)
import           Control.Monad.State
import           Control.Monad (replicateM, forM, forM_, zipWithM)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)

-- | Contexto com contador, nós, arestas e ambiente de variáveis e defs
data CGCtx = CGCtx
  { counter :: Int
  , nodes   :: [Text]
  , edges   :: [Text]
  , env     :: [(Ident, String)]
  , defs    :: [(Ident, ([Ident], Expr))]
  }

type GenM = State CGCtx

-- | Gera novo identificador único
freshId :: String -> GenM String
freshId base = do
  st <- get
  let n = counter st
  put st { counter = n + 1 }
  pure (base ++ show n)

-- | Gera nó Graphviz
emitNode :: String -> String -> GenM String
emitNode label body = do
  name <- freshId label
  modify $ \s -> s { nodes = nodes s ++ [T.pack $ name ++ " [label=\"" ++ body ++ "\", shape=box, style=rounded];"] }
  pure name

-- | Gera aresta Graphviz
emitEdge :: String -> String -> GenM ()
emitEdge from to =
  modify $ \s -> s { edges = edges s ++ [T.pack $ from ++ " -> " ++ to ++ ";"] }

-- | Vincula identificador a registrador
bindVar :: Ident -> String -> GenM ()
bindVar name reg = modify $ \s -> s { env = (name, reg) : env s }

lookupVar :: Ident -> GenM String
lookupVar name = do
  st <- get
  case lookup name (env st) of
    Just r  -> pure r
    Nothing -> pure name

bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s -> s { defs = (f, (ps, e)) : defs s }

lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = do
  st <- get
  pure (lookup f (defs st))

-- | Entrada principal: gera .dot
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      CGCtx _ ns es _ _ = execState (mapM_ recordDecl decls >> mapM_ genDecl decls) (CGCtx 0 [] [] [] [])
  in T.unlines (["digraph G {"] ++ ns ++ es ++ ["}"])

recordDecl :: Decl -> GenM ()
recordDecl (FunDecl name ps body) = bindDef name ps body

genDecl :: Decl -> GenM ()
genDecl (FunDecl name [] body) = do
  res <- genExpr [] body
  r <- emitNode "ret" ("ret " ++ name)
  emitEdge res r
  pure ()
genDecl (FunDecl _ (_:_) _) = pure ()

genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case
  Var x -> lookupVar x >>= emitNode "var"

  Lit l -> case l of
    LInt i   -> emitNode "const" ("const #" ++ show i)
    LBool b  -> emitNode "const" ("const #" ++ if b then "1" else "0")
    _        -> emitNode "const" "const #?"

  App f x -> do
    let flatten (App f1 x1) = let (fn, args) = flatten f1 in (fn, args ++ [x1])
        flatten e = (e, [])
    let (f', args) = flatten (App f x)
    case f' of
      Var fid -> do
        mdef <- lookupDef fid
        case mdef of
          Just (params, body) ->
            if fid `elem` stack
              then fallbackToCall fid args stack
              else do
                argRegs <- mapM (genExpr stack) args
                oldEnv <- gets env
                mapM_ (uncurry bindVar) (zip params argRegs)
                res <- genExpr (fid : stack) body
                modify $ \s -> s { env = oldEnv }
                pure res
          Nothing -> fallbackToCall fid args stack
      _ -> fallbackToCall "anon" (f' : args) stack

  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let opname = case op of
          Add -> "add"; Sub -> "sub"; Mul -> "mul"; Div -> "div"; Mod -> "mod"
          Eq -> "eq"; Neq -> "neq"; Lt -> "lt"; Le -> "le"
          Gt -> "gt"; Ge -> "ge"; And -> "and"; Or -> "or"
    n <- emitNode opname opname
    emitEdge ra n >> emitEdge rb n
    pure n

  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n <- emitNode "steer" "steer"
    emitEdge rc n >> emitEdge rt n >> emitEdge re n
    pure n

  Let decls body -> do
    mapM_ (\(FunDecl x [] e) -> genExpr stack e >>= bindVar x) decls
    genExpr stack body

  Tuple es -> do
    rs <- mapM (genExpr stack) es
    n <- emitNode "super" ("S" ++ show (length rs))
    mapM_ (`emitEdge` n) rs
    pure n

  List es -> do
    rs <- mapM (genExpr stack) es
    n <- emitNode "super" ("S" ++ show (length rs))
    mapM_ (`emitEdge` n) rs
    pure n

  Lambda{} -> emitNode "lambda" "lambda"

  Case e alts -> do
    scrut <- genExpr stack e
    altResults <- forM alts $ \(pat, expr) -> case pat of
      PVar x -> do
        bindVar x scrut
        r <- genExpr stack expr
        pure r
      _ -> genExpr stack expr
    n <- emitNode "steer" "steer"
    mapM_ (`emitEdge` n) (scrut : altResults)
    pure n

fallbackToCall :: Ident -> [Expr] -> [Ident] -> GenM String
fallbackToCall fname args stack = do
  argRegs <- mapM (genExpr stack) args
  callgroup <- emitNode "callgroup" ("callgroup(" ++ fname ++ ")")
  zipWithM_ (\i a -> do
    snd <- emitNode "callsnd" ("callsnd(" ++ fname ++ ", " ++ show i ++ ")")
    emitEdge a snd
    emitEdge snd callgroup) [1..] argRegs
  r <- emitNode "retsnd" ("retsnd(" ++ fname ++ ")")
  emitEdge callgroup r
  pure r
