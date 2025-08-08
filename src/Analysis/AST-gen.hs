{-# LANGUAGE LambdaCase #-}

-- Gera DOT para a AST.
-- Cobre TODOS os construtores usados:
--  - Patterns: PWildcard, PVar, PLit, PList, PTuple, PCons
--  - Expr: Var, Lit, Lambda, If, Cons, Case, Let, App, BinOp, UnOp, List, Tuple, Super

module ASTGen
  ( astToDot       -- Program -> Text
  , programToDot   -- sinônimo
  ) where

import Syntax
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Text.Lazy as TL

-- -----------------------------------------------------------------------------
-- Infra de geração de DOT
-- -----------------------------------------------------------------------------

type NodeId = Int

data G = G { gid :: !Int, out :: [String] }

type M a = State G a

push :: String -> M ()
push s = modify $ \g -> g{ out = s : out g }

fresh :: M NodeId
fresh = do
  i <- gets gid
  modify $ \g -> g{ gid = i + 1 }
  pure i

emitNode :: NodeId -> String -> M ()
emitNode n label =
  push $ show n ++ " [shape=box,label=\"" ++ esc label ++ "\"];"

emitEdge :: NodeId -> NodeId -> M ()
emitEdge a b = push $ show a ++ " -> " ++ show b ++ ";"

esc :: String -> String
esc = concatMap f
  where
    f '"'  = "\\\""
    f '\\' = "\\\\"
    f '\n' = "\\n"
    f c    = [c]

runM :: M NodeId -> TL.Text
runM m =
  let g0  = G 0 []
      (_root, g1) = runState m g0
      ls = [ "digraph AST {"
           , "  rankdir=TB;"
           , "  node [shape=box,fontname=\"monospace\"];"
           ] ++ map ("  "++) (reverse (out g1))
             ++ ["}"]
  in TL.unlines (map TL.pack ls)

-- -----------------------------------------------------------------------------
-- API
-- -----------------------------------------------------------------------------

astToDot :: Program -> TL.Text
astToDot = programToDot

programToDot :: Program -> TL.Text
programToDot p = runM (visitProgram p)

-- -----------------------------------------------------------------------------
-- Visitantes
-- -----------------------------------------------------------------------------

visitProgram :: Program -> M NodeId
visitProgram (Program ds) = do
  me <- fresh
  emitNode me "Program"
  mapM_ (\d -> visitDecl d >>= emitEdge me) ds
  pure me

visitDecl :: Decl -> M NodeId
visitDecl (FunDecl f ps e) = do
  me <- fresh
  emitNode me ("FunDecl " ++ f)
  -- params
  when (not (null ps)) $ do
    pnode <- fresh
    emitNode pnode ("Params " ++ intercalate " " ps)
    emitEdge me pnode
  -- body
  be <- visitExpr e
  emitEdge me be
  pure me

visitExpr :: Expr -> M NodeId
visitExpr = \case
  Var x -> leaf ("Var " ++ x)
  Lit l -> leaf ("Lit " ++ showLit l)

  Lambda ps b -> do
    me <- fresh
    emitNode me ("Lambda " ++ intercalate " " ps)
    b' <- visitExpr b
    emitEdge me b'
    pure me

  If c t e -> do
    me <- fresh
    emitNode me "If"
    c' <- visitExpr c; emitEdge me c'
    t' <- visitExpr t; emitEdge me t'
    e' <- visitExpr e; emitEdge me e'
    pure me

  Cons h t -> do
    me <- fresh
    emitNode me "Cons (:)"
    h' <- visitExpr h; emitEdge me h'
    t' <- visitExpr t; emitEdge me t'
    pure me

  Case scr alts -> do
    me <- fresh
    emitNode me "Case"
    s' <- visitExpr scr
    emitEdge me s'
    -- alternativas
    mapM_ (\(p,bd) -> do
              altN <- fresh
              emitNode altN "Alt"
              pn <- visitPat p
              bn <- visitExpr bd
              emitEdge altN pn
              emitEdge altN bn
              emitEdge me altN
          ) alts
    pure me

  Let ds e -> do
    me <- fresh
    emitNode me "Let"
    dsN <- fresh
    emitNode dsN "Decls"
    emitEdge me dsN
    mapM_ (\d -> visitDecl d >>= emitEdge dsN) ds
    e' <- visitExpr e
    emitEdge me e'
    pure me

  -- aplicação n-ária achatada
  e0@(App _ _) -> do
    let (f, xs) = flattenApp e0
    me <- fresh
    emitNode me "App"
    fn <- visitExpr f
    emitEdge me fn
    mapM_ (\a -> visitExpr a >>= emitEdge me) xs
    pure me

  BinOp op l r -> do
    me <- fresh
    emitNode me ("BinOp " ++ showBin op)
    l' <- visitExpr l; emitEdge me l'
    r' <- visitExpr r; emitEdge me r'
    pure me

  UnOp op x -> do
    me <- fresh
    emitNode me ("UnOp " ++ showUn op)
    x' <- visitExpr x
    emitEdge me x'
    pure me

  List xs -> do
    me <- fresh
    emitNode me "List"
    mapM_ (\a -> visitExpr a >>= emitEdge me) xs
    pure me

  Tuple xs -> do
    me <- fresh
    emitNode me ("Tuple/" ++ show (length xs))
    mapM_ (\a -> visitExpr a >>= emitEdge me) xs
    pure me

  -- <<< SUPORTE À SUPER INSTRUCTION >>>
  Super nm kind inp out _body -> do
    me <- fresh
    let k = case kind of { SuperSingle -> "single"; SuperParallel -> "parallel" }
    emitNode me ("Super[" ++ k ++ "]\\nname=" ++ nm
                 ++ "\\ninput=" ++ inp ++ "\\noutput=" ++ out)
    pure me


  where
    leaf :: String -> M NodeId
    leaf s = do n <- fresh; emitNode n s; pure n

-- ajuda: achatar árvore de aplicação em (função, args)
flattenApp :: Expr -> (Expr, [Expr])
flattenApp (App f x) = let (fn, xs) = flattenApp f in (fn, xs ++ [x])
flattenApp e         = (e, [])

visitPat :: Pattern -> M NodeId
visitPat = \case
  PWildcard   -> leaf "PWildcard"
  PVar x      -> leaf ("PVar " ++ x)
  PLit l      -> leaf ("PLit " ++ showLit l)
  PList ps    -> do
    me <- fresh
    emitNode me "PList"
    mapM_ (\p -> visitPat p >>= emitEdge me) ps
    pure me
  PTuple ps   -> do
    me <- fresh
    emitNode me ("PTuple/" ++ show (length ps))
    mapM_ (\p -> visitPat p >>= emitEdge me) ps
    pure me
  -- padrão cons (x:xs)
  PCons p ps  -> do
    me <- fresh
    emitNode me "PCons (:)"
    a <- visitPat p
    b <- visitPat ps
    emitEdge me a
    emitEdge me b
    pure me
  where
    leaf s = do n <- fresh; emitNode n s; pure n

-- -----------------------------------------------------------------------------
-- impressão de literais e operadores
-- -----------------------------------------------------------------------------

showLit :: Literal -> String
showLit = \case
  LInt n    -> show n
  LFloat f  -> show f
  LBool b   -> show b
  LChar c   -> show c
  LString s -> show s

showBin :: BinOperator -> String
showBin = \case
  Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
  Eq  -> "=="; Neq -> "/="; Lt  -> "<"; Le -> "<="; Gt -> ">"; Ge -> ">="
  And -> "&&"; Or  -> "||"

showUn :: UnOperator -> String
showUn = \case
  Neg -> "negate"
  Not -> "not"
