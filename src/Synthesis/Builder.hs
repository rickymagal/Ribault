-- src/Synthesis/Builder.hs
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}

-- | AST → data-flow graph (first incremental step).
--   This version *only* maps literal expressions to constant
--   instructions understood by the assembler. Everything else
--   is skipped for now.
module Synthesis.Builder
  ( buildProgram      -- :: Program -> DGraph DNode
  ) where

-- Front-end ------------------------------------------------------------------
import           Syntax

-- Graph core ------------------------------------------------------------------
import           Types              (DGraph, NodeId, emptyGraph, addNode)
import           Node               (DNode(..))

-- Internal -------------------------------------------------------------------
import           Control.Monad.State.Strict
import qualified Data.Map                       as Map

-------------------------------------------------------------------------------
-- State monad helpers
-------------------------------------------------------------------------------
data St = St
  { nextId :: !Int
  , dfg    :: !(DGraph DNode)
  }

type M a = State St a

fresh :: M NodeId
fresh = do
  s@St{..} <- get
  put s { nextId = nextId + 1 }
  return nextId

emit :: DNode -> M ()
emit n = do
  nid <- fresh
  modify' $ \s -> s { dfg = addNode nid n (dfg s) }

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------
buildProgram :: Program -> DGraph DNode
buildProgram p = dfg $ execState (visitProgram p) (St 0 emptyGraph)

-------------------------------------------------------------------------------
-- Visitors (constants only)
-------------------------------------------------------------------------------
visitProgram :: Program -> M ()
visitProgram (Program ds) = mapM_ visitDecl ds

visitDecl :: Decl -> M ()
visitDecl (FunDecl _ _ body) = visitExpr body

visitExpr :: Expr -> M ()
visitExpr = \case
  Lit l         -> visitLit l
  Lambda _ e    -> visitExpr e
  If c t f      -> mapM_ visitExpr [c, t, f]
  Case scr as'  -> visitExpr scr >> mapM_ (visitExpr . snd) as'
  Let ds e'     -> mapM_ visitDecl ds >> visitExpr e'
  App f x       -> visitExpr f >> visitExpr x
  BinOp _ l r   -> visitExpr l >> visitExpr r
  UnOp  _ e     -> visitExpr e
  List es       -> mapM_ visitExpr es
  Tuple es      -> mapM_ visitExpr es
  Cons h t      -> visitExpr h >> visitExpr t
  _             -> return ()   -- everything else ignored for now

visitLit :: Literal -> M ()
visitLit = \case
  LInt n    -> emit $ NConstI { nName = "c" ++ show n, cInt = n }
  LFloat d  -> emit $ NConstD { nName = "d" ++ show d, cDouble = d }
  LBool b   -> emit $ NConstI { nName = "b" ++ show b, cInt = if b then 1 else 0 }
  LChar c   -> emit $ NConstI { nName = "ch" ++ show (fromEnum c)
                              , cInt   = fromEnum c }
  LString s -> mapM_ (visitLit . LChar) s
