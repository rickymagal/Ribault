{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Módulo de geração de grafo dataflow a partir de AST desugarizada no estilo do Trebuchet.
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Char      (isAlphaNum)
import           Syntax         (Program(..), Decl(..), Expr(..), Literal(..), Ident, BinOperator(..))
import           Semantic       (desugarProgram)
import           Control.Monad.State
import           Data.List (intercalate)

-- | Contexto com contador, nós, arestas e ambiente de variáveis e defs

-- prettier names for constants, parameters, temporaries
formatReg :: String -> String
formatReg name
  | "const" `prefixOf` name = name
  | "t"     `prefixOf` name = "temp" ++ drop 1 name
  | otherwise               = name
  where prefixOf p s = take (length p) s == p

-- | Contexto com contador, nós, arestas e ambiente de variáveis e defs
data CGCtx = CGCtx { counter :: Int, nodes :: [Text], edges :: [Text], env :: [(Ident, String)], defs :: [(Ident, ([Ident], Expr))] }

type GenM = State CGCtx

freshId :: String -> GenM String
freshId base = do
  st <- get
  let n = counter st
  put st { counter = n + 1 }
  pure (base ++ show n)

emitNode :: String -> String -> GenM String
emitNode label body = do
  name <- freshId label
  let formatted = formatReg name
  modify (\s -> s { nodes = nodes s ++ [T.pack (formatted ++ " [label=\"{" ++ formatted ++ "|" ++ escape body ++ "}\"]") ] })
  pure formatted
  where escape = concatMap escapeChar
        escapeChar '"' = "\\\""
        escapeChar c   = [c]

emitEdge :: String -> String -> GenM ()
emitEdge from to =
  modify (\s -> s { edges = edges s ++ [T.pack (formatReg from ++ " -> " ++ formatReg to)] })

bindVar :: Ident -> String -> GenM ()
bindVar name reg = modify (\s -> s { env = (name, reg) : env s })

lookupVar :: Ident -> GenM String
lookupVar name = do
  st <- get
  case lookup name (env st) of
    Just r  -> pure r
    Nothing -> pure name

bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify (\s -> s { defs = (f, (ps, e)) : defs s })

lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = do
  st <- get
  pure (lookup f (defs st))

-- | Entrada principal: gera .dot
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      CGCtx _ ns es _ _ = execState (mapM_ recordDecl decls >> mapM_ genDecl decls) (CGCtx 0 [] [] [] [])
  in T.unlines ( ["digraph Dataflow {", "  node [shape=record,fontname=Courier];"] ++ ns ++ es ++ ["}"])

-- | Primeiro passo: armazena definições no ambiente
recordDecl :: Decl -> GenM ()
recordDecl (FunDecl name ps body) = bindDef name ps body

-- | Gera grafo para declaração top-level
-- Agora, só gera se não tiver parâmetros (isto é, bindings diretos como "main = ...")
genDecl :: Decl -> GenM ()
genDecl (FunDecl name [] body) = do
  res <- genExpr [] body
  r <- emitNode "ret" ("ret " ++ name ++ ", " ++ res)
  emitEdge res r
  pure ()
genDecl (FunDecl _ (_:_) _) = pure ()

-- | Expressões com controle de recursão (stack de chamadas)
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case
  Var x -> lookupVar x >>= emitNode "var"

  Lit l -> case l of
    LInt i   -> emitNode "const" ("const " ++ show i)
    LFloat f -> emitNode "fconst" ("fconst " ++ show f)
    LBool b  -> emitNode "const" ("const " ++ if b then "1" else "0")
    LChar c  -> emitNode "const" ("const " ++ show (fromEnum c))
    LString s -> do
      cs <- mapM (\ch -> emitNode "char" ("const " ++ show (fromEnum ch))) s
      sn <- emitNode "super" ("super, 1, " ++ show (length cs))
      mapM_ (\c -> emitEdge c sn) cs
      pure sn

  App f x -> do
    let flatten (App f1 x1) = let (fn, args) = flatten f1 in (fn, args ++ [x1])
        flatten e = (e, [])
    let (f', args) = flatten (App f x)

    fname <- case f' of Var n -> pure n; _ -> error "Only Var application supported"
    mdef <- lookupDef fname
    case mdef of
      Just (params, body) ->
        if fname `elem` stack
          then fallbackToCall fname args stack
          else do
            argRegs <- mapM (genExpr stack) args
            oldEnv <- gets env
            mapM_ (uncurry bindVar) (zip params argRegs)
            res <- genExpr (fname:stack) body
            modify (\s -> s { env = oldEnv })
            pure res
      Nothing -> fallbackToCall fname args stack

  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let opname = case op of
          Add -> "add"; Sub -> "sub"; Mul -> "mul"; Div -> "div"; Mod -> "mod"
          Eq -> "equal"; Neq -> "notequal"; Lt -> "lthan"; Le -> "lthani"
          Gt -> "gthan"; Ge -> "gthani"; And -> "and"; Or -> "or"
    n <- emitNode opname (opname ++ " result, " ++ ra ++ ", " ++ rb)
    emitEdge ra n >> emitEdge rb n
    pure n

  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n <- emitNode "steer" ("steer result, " ++ rc ++ ", " ++ rt ++ ", " ++ re)
    emitEdge rc n >> emitEdge rt n >> emitEdge re n
    pure n

  Let decls body -> do
    mapM_ (\(FunDecl x [] e) -> do r <- genExpr stack e; bindVar x r) decls
    genExpr stack body

  Tuple es -> do
    rs <- mapM (genExpr stack) es
    n <- emitNode "super" ("super, 1, " ++ show (length rs))
    mapM_ (\r -> emitEdge r n) rs
    pure n

  List es -> do
    rs <- mapM (genExpr stack) es
    n <- emitNode "super" ("super, 1, " ++ show (length rs))
    mapM_ (\r -> emitEdge r n) rs
    pure n

  Lambda{} -> emitNode "lambda" "// lambda (ignored)"
  Case{}   -> emitNode "case" "// case (not yet supported)"

fallbackToCall :: Ident -> [Expr] -> [Ident] -> GenM String
fallbackToCall fname args stack = do
  argRegs <- mapM (genExpr stack) args
  callgroup <- emitNode "cg" ("callgroup(\"cg_" ++ fname ++ "\", \"" ++ fname ++ "\")")
  snds <- zipWithM (\i a -> do
            n <- emitNode "snd" ("callsnd " ++ fname ++ "[" ++ show (i+1) ++ "], " ++ a ++ ", cg_" ++ fname)
            emitEdge a n
            emitEdge n callgroup
            pure n) [0..] argRegs
  retreg <- freshId "rtsnd"
  nret <- emitNode "retsnd" ("retsnd " ++ fname ++ "[0], " ++ retreg ++ ", cg_" ++ fname)
  emitEdge callgroup nret
  pure retreg
