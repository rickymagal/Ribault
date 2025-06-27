{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

-- Synthesis.Builder – AST → lista de Inst

module Synthesis.Builder (buildProgram) where

import           Syntax
import qualified Synthesis.Instruction as IR

import           Control.Monad.State.Strict
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe           (fromMaybe)
import           Data.Foldable        (for_)

-- ════════════════════════════════════════════════════════
-- Estado do builder
-- ════════════════════════════════════════════════════════
type Build = State BS
data BS = BS { env   :: Map Ident [IR.Signal]
             , acc   :: [IR.Inst]    -- ^ acumulador (reverso)
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

-- Bool constante rápida ---------------------------------------------------
constBool :: Bool -> Build [IR.Signal]
constBool b = do nid<-freshId
                 emit (IR.InstConst nid (if b then 1 else 0) "int")
                 pure [IR.SigInstPort nid 0 Nothing]

-- ════════════════════════════════════════════════════════
-- Entrada pública
-- ════════════════════════════════════════════════════════
buildProgram :: Program -> [IR.Inst]
buildProgram (Program ds) = reverse . acc $ execState (mapM_ decl ds) empty

-- ════════════════════════════════════════════════════════
-- Declarações
-- ════════════════════════════════════════════════════════
decl :: Decl -> Build ()
decl (FunDecl f ps (Lambda lamPs body)) =
  decl (FunDecl f (ps ++ lamPs) body)
decl (FunDecl f ps body) = do
  let dummy ix = IR.SigInstPort (IR.NodeId (-ix)) 0 Nothing
  modify' $ \st -> st{env = M.fromList (zip ps (map (pure . dummy) [1..]))}
  res <- expr body
  nid <- freshId
  emit (IR.InstReturn nid f res res)
  modify' $ \st -> st{env = M.empty}

-- ════════════════════════════════════════════════════════
-- Expressões
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
  Case s as  -> caseExpr s as
  List es    -> listLit es
  Tuple es   -> tupleLit es

-- ═════════════ Literais ════════════════════════════════
lit :: Literal -> Build [IR.Signal]
lit = \case
  LFloat d  -> scalar (floor (d*100)) "float"
  LString s -> fmap concat $ mapM (lit . LChar) s
  LInt n    -> scalar (toInteger n) "int"
  LBool b   -> scalar (if b then 1 else 0) "int"
  LChar c   -> scalar (toInteger (fromEnum c)) "int"
 where
  scalar v ty = do nid<-freshId
                   emit (IR.InstConst nid v ty)
                   pure [IR.SigInstPort nid 0 Nothing]

listLit :: [Expr] -> Build [IR.Signal]
listLit es  = for_ es expr >> makeCount (length es)

tupleLit :: [Expr] -> Build [IR.Signal]
tupleLit es = concat <$> mapM expr es      -- cada campo devolve seu sinal

makeCount n = do nid<-freshId
                 emit (IR.InstConst nid (toInteger n) "int")
                 pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════ Unário / Binário ═════════════════════════
unop :: UnOperator -> Expr -> Build [IR.Signal]
unop op e = do v<-expr e; nid<-freshId
               let (o,i)=case op of{Neg->("sub",0); Not->("eq",0)}
               emit (IR.InstBinopI nid o "int" i v)
               pure [IR.SigInstPort nid 0 Nothing]

binop :: BinOperator -> Expr -> Expr -> Build [IR.Signal]
binop op l r = do la<-expr l; rb<-expr r; nid<-freshId
                  let o=case op of
                         Add->"+"; Sub->"-"; Mul->"*"; Div->"/"; Mod->"%"
                         Eq->"=="; Neq->"!="; Lt->"<"; Le->"<="; Gt->">"; Ge->">="
                         And->"&&"; Or->"||"
                  emit (IR.InstBinop nid o "int" la rb)
                  pure [IR.SigInstPort nid 0 Nothing]

-- ═════════════ If / Steer ══════════════════════════════
ifExpr :: Expr -> Expr -> Expr -> Build [IR.Signal]
ifExpr c t e = do cond<-expr c; nid<-freshId
                  emit (IR.InstSteer nid cond [])
                  (++) <$> expr t <*> expr e

-- ═════════════ Case expression ═════════════════════════
caseExpr :: Expr -> [(Pattern,Expr)] -> Build [IR.Signal]
caseExpr scr alts = expr scr >>= go alts
 where
  go [] _ = err "non-exhaustive patterns"
  go ((p,r):rest) scrSig = do
    cond <- patCond p scrSig
    nid  <- freshId
    emit (IR.InstSteer nid cond [])
    env0 <- gets env
    bindPat p scrSig
    thenS <- expr r
    modify' $ \st -> st{env = env0}
    elseS <- if null rest then constBool False else go rest scrSig
    pure (thenS ++ elseS)

-- * padrões suportados ----------------------------------------------------
patCond :: Pattern -> [IR.Signal] -> Build [IR.Signal]
patCond pat scrS = case pat of
  PWildcard      -> constBool True
  PVar _         -> constBool True
  PLit litConst  -> do sig <- lit litConst
                       nid <- freshId
                       emit (IR.InstBinop nid "==" "int" scrS sig)
                       pure [IR.SigInstPort nid 0 Nothing]
  PTuple _       -> constBool True
  _              -> err "unsupported pattern"

bindPat :: Pattern -> [IR.Signal] -> Build ()
bindPat pat src = case pat of
  PVar x     -> remember x src
  PTuple ps  -> sequence_ [ bindPat p [src !! i] | (p,i) <- zip ps [0..] ]
  _          -> pure ()

-- ═════════════ Aplicação 1ª ordem ═════════════════════
app :: Expr -> Expr -> Build [IR.Signal]
app fun arg = do
  let peel (App f a) xs = peel f (a:xs)
      peel other xs     = (other,xs)
      (callee,args)     = peel (App fun arg) []
  fn <- case callee of Var v -> pure v; _ -> err "HO call"
  gid<-freshId; let IR.NodeId n = gid; grp="cg"<>show n
  emit (IR.InstCallGrp gid fn grp)
  for_ (zip [1..] args) $ \(ix,a)-> do
    s<-expr a; nid<-freshId
    emit (IR.InstCallSnd nid fn grp ix s)
  nidR<-freshId; let ret=[IR.SigReturnPort fn grp]
  emit (IR.InstRetSnd nidR fn grp ret); pure ret

-- ═════════════ util ═══════════════════════════════════
err :: String -> a
err = error . ("Builder: "++)
