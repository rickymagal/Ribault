{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

-- | Synthesis.Builder – percorre a AST (Syntax) e gera lista linear de
--   instruções (Inst) definidas em Synthesis.Instruction.
--
--   Suporta:
--     • Literais      (Float vira fconst; String = lista de Char)
--     • Binários / Unários
--     • if-then-else  (Steer simplificado)
--     • let / FunDecl (inclui lambdas desaçucaradas)
--     • Aplicações de 1ª ordem

module Synthesis.Builder (buildProgram) where

import           Syntax
import qualified Synthesis.Instruction as IR

import           Control.Monad.State.Strict
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe           (fromMaybe)
import           Data.Foldable        (for_)

-- ════════════════════════════════════════════════════════
-- State
-- ════════════════════════════════════════════════════════
type Build = State BS
data BS = BS { env   :: Map Ident [IR.Signal]
             , acc   :: [IR.Inst]    -- ^ reversed
             , nextN :: Int }
empty :: BS
empty = BS M.empty [] 0

freshId :: Build IR.NodeId
freshId = do s@BS{..} <- get; put s{nextN = nextN+1}; pure (IR.NodeId nextN)

emit :: IR.Inst -> Build ()
emit i = modify' $ \st -> st{acc = i:acc st}

remember :: Ident -> [IR.Signal] -> Build ()
remember v s = modify' $ \st -> st{env = M.insert v s (env st)}

lookupSig :: Ident -> Build [IR.Signal]
lookupSig v = gets (fromMaybe (error ("unbound var "++v)) . M.lookup v . env)

-- ════════════════════════════════════════════════════════
-- Public entry
-- ════════════════════════════════════════════════════════
buildProgram :: Program -> [IR.Inst]
buildProgram (Program ds) = reverse . acc $ execState (mapM_ decl ds) empty

-- ════════════════════════════════════════════════════════
-- Declarations
-- ════════════════════════════════════════════════════════
decl :: Decl -> Build ()
-- Dessacar:   f = \p1 p2 -> body   ⇒   f p1 p2 = body
decl (FunDecl f ps (Lambda lamPs body)) =
  decl (FunDecl f (ps ++ lamPs) body)

-- Cláusula regular
decl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing
  modify' $ \st -> st{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \st -> st{env = M.empty}

-- ════════════════════════════════════════════════════════
-- Expressions
-- ════════════════════════════════════════════════════════
expr :: Expr -> Build [IR.Signal]
expr = \case
  Var v      -> lookupSig v
  Lit l      -> lit l
  BinOp o a b-> binop o a b
  UnOp  o e  -> unop  o e
  If c t e   -> ifExpr c t e
  Let ds e   -> mapM_ decl ds >> expr e
  App f x    -> app f x
  Lambda{}   -> err "lambda expression not supported"
  Case{}     -> err "case not supported"
  List es    -> listLit es
  Tuple es   -> tupleLit es

-- ═══════════════ Literals ═══════════════════════════════
lit :: Literal -> Build [IR.Signal]
lit = \case
  LFloat d  -> scalar (floor (d*100)) "float"        -- => fconst #3.14
  LString s -> fmap concat $ mapM (lit . LChar) s
  LInt n    -> scalar (toInteger n) "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LChar c   -> scalar (toInteger (fromEnum c)) "int"
 where
  scalar v ty = do nid <- freshId
                   emit (IR.InstConst nid v ty)
                   pure [IR.SigInstPort nid 0 Nothing]

listLit, tupleLit :: [Expr] -> Build [IR.Signal]
listLit es  = for_ es expr >> makeCount (length es)
tupleLit es = listLit es
makeCount n = do nid<-freshId
                 emit (IR.InstConst nid (toInteger n) "int")
                 pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════ Unário ════════════════════════════════
unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do v<-expr e; nid<-freshId
               let (o,i)=case op of{Neg->("sub",0); Not->("eq",0)}
               emit (IR.InstBinopI nid o "int" i v)
               pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════ Binário ═══════════════════════════════
binop :: BinOperator -> Expr -> Expr -> Build [IR.Signal]
binop op l r = do la<-expr l; rb<-expr r; nid<-freshId
                  let o=case op of
                         Add->"+"; Sub->"-"; Mul->"*"; Div->"/"; Mod->"%"
                         Eq->"=="; Neq->"!="; Lt->"<"; Le->"<="; Gt->">"; Ge->">="
                         And->"&&"; Or->"||"
                  emit (IR.InstBinop nid o "int" la rb)
                  pure [IR.SigInstPort nid 0 Nothing]

-- ═══════════════ If / Steer ════════════════════════════
ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t e = do cond<-expr c; nid<-freshId
                  emit (IR.InstSteer nid cond [])
                  (++) <$> expr t <*> expr e

-- ═══════════════ Aplicação 1ª ordem ════════════════════
app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  let collect (App f a) xs = collect f (a:xs)
      collect other xs     = (other,xs)
      (callee,args)        = collect (App fun arg) []
  fn <- case callee of Var v -> pure v; _ -> err "HO call"
  gid<-freshId; let IR.NodeId n=gid; grp="cg"<>show n
  emit (IR.InstCallGrp gid fn grp)
  for_ (zip [1..] args) $ \(ix,a)-> do
    s<-expr a; nid<-freshId
    emit (IR.InstCallSnd nid fn grp ix s)
  nidR<-freshId; let ret=[IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd nidR fn grp ret); pure ret

-- ═══════════════ Util ══════════════════════════════════
err :: String -> a
err = error . ("Builder: "++)
