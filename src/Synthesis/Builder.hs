{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Synthesis.Builder (buildProgram) where

import           Syntax
import qualified Synthesis.Instruction as IR

import           Control.Monad.State.Strict
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe           (fromMaybe)
import           Data.Foldable        (for_)

-- ════════════════════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════════════════════

type Build = State BS

data BS = BS
  { env   :: Map Ident [IR.Signal]  -- ^ live variables
  , acc   :: [IR.Inst]              -- ^ reversed instruction list
  , nextN :: Int                    -- ^ NodeId counter
  }

empty :: BS
empty = BS M.empty [] 0

freshId :: Build IR.NodeId
freshId = do s@BS{..} <- get; put s{nextN = nextN + 1}; pure (IR.NodeId nextN)

emit :: IR.Inst -> Build ()
emit i = modify' $ \st -> st{acc = i : acc st}

remember :: Ident -> [IR.Signal] -> Build ()
remember v s = modify' $ \st -> st{env = M.insert v s (env st)}

lookupSig :: Ident -> Build [IR.Signal]
lookupSig v = gets (fromMaybe (error ("unbound var " ++ v)) . M.lookup v . env)

-- ════════════════════════════════════════════════════════════════════════
-- Public entry
-- ════════════════════════════════════════════════════════════════════════

buildProgram :: Program -> [IR.Inst]
buildProgram (Program ds) = reverse . acc $ execState (mapM_ decl ds) empty

-- ════════════════════════════════════════════════════════════════════════
-- Declarations
-- ════════════════════════════════════════════════════════════════════════

decl :: Decl -> Build ()
decl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing
  modify' $ \st -> st{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \st -> st{env = M.empty}

-- ════════════════════════════════════════════════════════════════════════
-- Expressions
-- ════════════════════════════════════════════════════════════════════════

expr :: Expr -> Build [IR.Signal]
expr = \case
  Var v      -> lookupSig v
  Lit l      -> compileLit l
  BinOp o a b-> binop o a b
  UnOp  o e  -> unop  o e
  If c t e   -> ifExpr c t e
  Let ds e   -> mapM_ decl ds >> expr e
  App f x    -> app f x
  Lambda{}   -> err "lambda not supported"
  Case{}     -> err "case not supported"
  List es    -> listLit es
  Tuple es   -> tupleLit es

-- ════════════════════════════════════════════════════════════════════════
-- Literals
-- ════════════════════════════════════════════════════════════════════════

compileLit :: Literal -> Build [IR.Signal]
compileLit lit = case lit of
  LString s -> fmap concat $ mapM (compileLit . LChar) s

  LFloat d  -> scalar (floor (d * 100)) "float"   -- *GraphViz mostrará fconst
  LInt n    -> scalar (toInteger n)     "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LChar c   -> scalar (toInteger (fromEnum c)) "int"
  where
    scalar val ty = do
      nid <- freshId
      emit (IR.InstConst nid val ty)
      pure [IR.SigInstPort nid 0 Nothing]

-- listas/tuplas continuam emitindo apenas o comprimento -------------------
listLit, tupleLit :: [Expr] -> Build [IR.Signal]
listLit es  = for_ es expr >> makeCount (length es)
tupleLit es = listLit es

makeCount :: Int -> Build [IR.Signal]
makeCount n = do
  nid <- freshId
  emit (IR.InstConst nid (toInteger n) "int")
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════  Unário  ════════════════════════════════════════════════
unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do
  s <- expr e; nid <- freshId
  let (o,imm) = case op of { Neg -> ("sub",0); Not -> ("eq",0) }
  emit (IR.InstBinopI nid o "int" imm s)
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════  Binário ═══════════════════════════════════════════════
binop :: BinOperator -> Expr -> Expr -> Build [IR.Signal]
binop op l r = do
  la <- expr l; rb <- expr r; nid <- freshId
  let o = case op of
        Add->"+"; Sub->"-"; Mul->"*"; Div->"/"; Mod->"%"
        Eq ->"=="; Neq->"!="; Lt->"<"; Le->"<="; Gt->">"; Ge->">="
        And->"&&"; Or ->"||"
  emit (IR.InstBinop nid o "int" la rb)
  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════  If / Steer ════════════════════════════════════════════
ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t e = do
  cond <- expr c; nid <- freshId
  emit (IR.InstSteer nid cond [])
  (++) <$> expr t <*> expr e

-- ═══════════════  Aplicação  ════════════════════════════════════════════
app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  let peel (App f a) xs = peel f (a:xs)
      peel other xs     = (other,xs)
      (callee,args)     = peel (App fun arg) []
  fn <- case callee of
          Var v -> pure v
          _     -> err "higher-order call"
  gid <- freshId; let IR.NodeId n = gid; grp = "cg"<>show n
  emit (IR.InstCallGrp gid fn grp)
  for_ (zip [1..] args) $ \(ix,a) -> do
     s <- expr a; nid <- freshId
     emit (IR.InstCallSnd nid fn grp ix s)
  nidR <- freshId
  let retS = [IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd nidR fn grp retS)
  pure retS

-- ═══════════════  Utils  ════════════════════════════════════════════════
err :: String -> a
err = error . ("Builder: " ++)
