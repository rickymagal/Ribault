{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Semantic analysis e type checking para uma linguagem funcional simples.
module Semantic where

import Syntax
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import Control.Monad.Except
import Control.Monad.State
import Control.Monad (forM, unless, zipWithM_)

-- ======================================================
-- 1) Desugaring de lambdas de topo
-- ======================================================

desugarDecl :: Decl -> Decl
desugarDecl (FunDecl f [] (Lambda ps e)) = FunDecl f ps e
desugarDecl d                           = d

desugarProgram :: Program -> Program
desugarProgram (Program ds) = Program (map desugarDecl ds)

-- ======================================================
-- 2) Definições de erro
-- ======================================================

data SemanticError
  = UndefinedVar Ident
  | ArityMismatch Ident Int Int
  | DuplicateFunc Ident
  | DuplicateParam Ident
  | DuplicatePatternVar Ident
  deriving (Show, Eq)

data TypeError
  = Mismatch Expr Type Type
  | CondNotBool Expr Type
  | BranchesTypeDiffer Expr Expr Type Type
  | BinOpTypeErr BinOperator Type Type
  | UnOpTypeErr UnOperator Type
  | UnknownVar Ident
  deriving Show

data Error
  = SemErr SemanticError
  | TypErr TypeError
  deriving Show

-- ======================================================
-- 3) Tipos e ambientes
-- ======================================================

type Sig     = Map.Map Ident Int
type Env     = Set.Set Ident

data Type
  = TInt | TFloat | TBool | TChar | TString
  | TList Type
  | TTuple [Type]
  | TVar String
  | TFun [Type] Type
  deriving (Eq, Show)

type TypeEnv = Map.Map Ident Type
type FuncEnv = Map.Map Ident ([Type], Type)

data InferState = InferState { count :: Int }
type Infer a    = ExceptT TypeError (State InferState) a

-- ======================================================
-- 4) Construção de assinatura e ambiente de funções
-- ======================================================

buildSig :: [Decl] -> Sig
buildSig = foldr (\(FunDecl f ps _) acc ->
                    Map.insertWith (const id) f (length ps) acc)
                 Map.empty

buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = Map.fromList . map (\(FunDecl f args _) ->
  let tvs = replicate (length args) (TVar "_")
      tr  = TVar ("r_" ++ f)
  in (f, (tvs, tr)))

-- ======================================================
-- 5) Checagem semântica (escopo & aridade)
-- ======================================================

semanticCheck :: Program -> [Error]
semanticCheck prog =
  let Program ds = desugarProgram prog
      sig0       = buildSig ds
  in map SemErr $ concatMap (checkDecl sig0) ds

checkDecl :: Sig -> Decl -> [SemanticError]
checkDecl sig (FunDecl _ ps b) =
  let dupParams = [ DuplicateParam x | x <- ps, length (filter (==x) ps) > 1 ]
      env0      = Set.fromList ps
  in dupParams ++ checkExpr sig env0 b

checkExpr :: Sig -> Env -> Expr -> [SemanticError]
checkExpr sig env expr = case expr of
  Var x
    | Set.member x env || Map.member x sig -> []
    | otherwise                            -> [UndefinedVar x]

  Lit _       -> []

  Lambda ps e ->
    let dup  = [ DuplicateParam p | p <- ps, length (filter (==p) ps) > 1 ]
        env' = Set.union env (Set.fromList ps)
    in dup ++ checkExpr sig env' e

  If c t e    ->
    concatMap (checkExpr sig env) [c,t,e]

  Cons x xs   ->
    checkExpr sig env x ++ checkExpr sig env xs

  Case s alts ->
    checkExpr sig env s ++ concatMap (checkAlt sig env) alts

  Let ds e    ->
    let sig'  = Map.union (buildSig ds) sig
        envDs = Set.union env (Set.fromList (concatMap (\(FunDecl _ ps _) -> ps) ds))
        errsD = concatMap (\(FunDecl _ ps bd) ->
                   checkExpr sig' (Set.union envDs (Set.fromList ps)) bd
                 ) ds
    in errsD ++ checkExpr sig' envDs e

  App{}       ->
    let (fn, args) = flattenApp expr
        e1 = checkExpr sig env fn
        e2 = concatMap (checkExpr sig env) args
        ar = case fn of
               Var f | Just ar0 <- Map.lookup f sig, ar0 /= length args
                       -> [ArityMismatch f ar0 (length args)]
               _ -> []
    in e1 ++ e2 ++ ar

  BinOp _ l r -> checkExpr sig env l ++ checkExpr sig env r
  UnOp _ x    -> checkExpr sig env x
  List xs     -> concatMap (checkExpr sig env) xs
  Tuple xs    -> concatMap (checkExpr sig env) xs

checkAlt :: Sig -> Env -> (Pattern,Expr) -> [SemanticError]
checkAlt sig env (pat,bd) =
  let vs   = patVars pat
      dupV = [ DuplicatePatternVar v | v <- vs, length (filter (==v) vs) > 1 ]
      env' = Set.union env (Set.fromList vs)
  in dupV ++ checkExpr sig env' bd

flattenApp :: Expr -> (Expr,[Expr])
flattenApp (App f x) = let (fn,xs) = flattenApp f in (fn, xs ++ [x])
flattenApp e         = (e,[])

patVars :: Pattern -> [Ident]
patVars = \case
  PWildcard   -> []
  PVar x      -> [x]
  PLit _      -> []
  PList ps    -> concatMap patVars ps
  PTuple ps   -> concatMap patVars ps
  PCons p ps  -> patVars p ++ patVars ps

-- ======================================================
-- 6) Inferência de tipos (Hindley–Milner)
-- ======================================================

checkProgram :: Program -> [Error]
checkProgram prog =
  let Program ds = desugarProgram prog
      fenv        = buildFuncEnv ds
  in concatMap (runDecl fenv Map.empty) ds
  where
    runDecl fenv tenv (FunDecl f ps b) =
      let argTys = replicate (length ps) (TVar "_")
          retTy  = TVar ("r_" ++ f)
          env'   = Map.union (Map.fromList (zip ps argTys)) tenv
          act    = inferExpr fenv env' b >>= unifyReturn retTy
          st0    = InferState 0
      in case runState (runExceptT act) st0 of
           (Left te, _) -> [TypErr te]
           _            -> []

unifyReturn :: Type -> Type -> Infer Type
unifyReturn expected actual
  | TVar _    <- expected          = return actual
  | TVar _    <- actual            = return expected
  | expected == actual             = return actual
  | TList e  <- expected, TList a <- actual
                                   = unifyReturn e a >> return actual
  | TTuple es<- expected, TTuple as<- actual, length es == length as
                                   = zipWithM_ unifyReturn es as >> return actual
  | otherwise                      = throwError (Mismatch (Var "<return>") expected actual)

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _        = False

inferExpr :: FuncEnv -> TypeEnv -> Expr -> Infer Type
inferExpr fenv tenv expr = case expr of
  Var x -> case Map.lookup x tenv of
    Just t  -> return t
    Nothing -> case Map.lookup x fenv of
      Just (argTys,retT) -> return (TFun argTys retT)
      Nothing            -> throwError (UnknownVar x)

  Lit l -> return $ literalType l

  Cons hd tl -> do
    tHd <- inferExpr fenv tenv hd
    tTl <- inferExpr fenv tenv tl
    case tTl of
      TList tEl | match tHd tEl -> return (TList (resolve tHd tEl))
      TVar _                     -> return (TList tHd)
      _                          -> throwError (Mismatch expr (TList tHd) tTl)

  Lambda ps bd -> do
    tys   <- mapM (const freshTypeVar) ps
    let tenv' = Map.union (Map.fromList (zip ps tys)) tenv
    tr    <- inferExpr fenv tenv' bd
    return (TFun tys tr)

  If c t e -> do
    _     <- inferExpr fenv tenv c >>= ensureBool c
    tc    <- inferExpr fenv tenv t
    te    <- inferExpr fenv tenv e
    unifyReturn tc te

  Case scr alts -> do
    scrT  <- inferExpr fenv tenv scr
    rs    <- forM alts $ \(pat,bd') -> do
      (vs,pT) <- inferPattern pat
      _        <- unifyReturn scrT pT
      inferExpr fenv (Map.union (Map.fromList vs) tenv) bd'
    case rs of
      (r0:rs') -> foldM unifyReturn r0 rs'
      []       -> throwError (Mismatch scr scrT scrT)

  Let ds e -> do
    let fenv' = Map.union (buildFuncEnv ds) fenv
    tenv' <- foldM (\envAcc (FunDecl fn [] bd') -> do
                     tbd <- inferExpr fenv envAcc bd'
                     return $ Map.insert fn tbd envAcc
                   ) tenv ds
    inferExpr fenv' tenv' e

  App _ _ -> do
    let (fn,args) = flattenApp expr
    fty   <- inferExpr fenv tenv fn
    argTs <- mapM (inferExpr fenv tenv) args
    case fty of
      TFun ps r
        | length ps /= length argTs -> throwError (Mismatch expr (TFun ps r) (TFun argTs r))
        | and (zipWith match ps argTs) -> return r
        | otherwise -> throwError (Mismatch expr (TFun ps r) (TFun argTs r))
      TVar _ -> freshTypeVar
      _      -> throwError (Mismatch expr (TVar "_") fty)

  BinOp op l r -> do
    tl <- inferExpr fenv tenv l
    tr <- inferExpr fenv tenv r
    case op of
      Add -> numBin op tl tr; Sub -> numBin op tl tr
      Mul -> numBin op tl tr; Div -> numBin op tl tr
      Mod -> numBin op tl tr
      Eq  -> compBin op tl tr; Neq -> compBin op tl tr
      Lt  -> compBin op tl tr; Le  -> compBin op tl tr
      Gt  -> compBin op tl tr; Ge  -> compBin op tl tr

  UnOp op e -> do
    te <- inferExpr fenv tenv e
    case op of
      Neg | te `elem` [TInt,TFloat] -> return te
          | isPoly te               -> return TInt
          | otherwise               -> throwError (UnOpTypeErr op te)
      Not | te == TBool             -> return TBool
          | otherwise               -> throwError (UnOpTypeErr op te)

  List xs -> case xs of
    []      -> return (TList TInt)
    (y:ys') -> do
      t0   <- inferExpr fenv tenv y
      rest <- mapM (inferExpr fenv tenv) ys'
      forM_ rest $ \t ->
        unless (match t0 t) $ throwError (Mismatch expr t0 t)
      return (TList t0)

  Tuple xs -> TTuple <$> mapM (inferExpr fenv tenv) xs

-- — auxiliares —————————————————————————————————

match :: Type -> Type -> Bool
match (TVar _) _ = True
match _ (TVar _) = True
match a b       = a == b

resolve :: Type -> Type -> Type
resolve (TVar _) t = t
resolve t _        = t

inferPattern :: Pattern -> Infer ([(Ident,Type)],Type)
inferPattern = \case
  PVar x -> do
    tv <- freshTypeVar
    return ([(x, tv)], tv)
  PWildcard   -> return ([], TVar "_")
  PLit l      -> return ([], literalType l)
  PList ps    -> do
    ps'    <- mapM inferPattern ps
    let (vs,ts) = unzip ps'
    elemTy <- case ts of
      []    -> freshTypeVar
      (t:_) -> return t
    return (concat vs, TList elemTy)
  PTuple ps   -> do
    ps'    <- mapM inferPattern ps
    let (vs,ts) = unzip ps'
    return (concat vs, TTuple ts)
  PCons p ps' -> do
    (v1,t1) <- inferPattern p
    (v2,t2) <- inferPattern ps'
    unless (match t2 (TList t1)) $
      throwError (Mismatch (Lit (LString "pattern")) (TList t1) t2)
    return (v1++v2, TList t1)

literalType :: Literal -> Type
literalType = \case
  LInt _    -> TInt
  LFloat _  -> TFloat
  LBool _   -> TBool
  LChar _   -> TChar
  LString _ -> TString

numBin, boolBin :: BinOperator -> Type -> Type -> Infer Type
numBin _ TInt   TInt   = return TInt
numBin _ TFloat TFloat = return TFloat
numBin op a b         = throwError (BinOpTypeErr op a b)

boolBin _ TBool TBool = return TBool
boolBin op a b        = throwError (BinOpTypeErr op a b)

-- **compBin: aceita igualdade e comparação em tipos polimórficos**
compBin :: BinOperator -> Type -> Type -> Infer Type
compBin _ a b
  | a == b && a `elem` [TInt,TFloat,TChar,TString] = return TBool
compBin _ a b
  | isPoly a || isPoly b                          = return TBool
compBin op a b                                   = throwError (BinOpTypeErr op a b)

ensureBool :: Expr -> Type -> Infer ()
ensureBool _ TBool = return ()
ensureBool e t     = throwError (CondNotBool e t)

freshTypeVar :: Infer Type
freshTypeVar = do
  s <- get
  let n = count s + 1
  put s { count = n }
  return (TVar ("t" ++ show n))

isPoly :: Type -> Bool
isPoly (TVar _) = True
isPoly _        = False

-- ======================================================
-- 7) Ponto de entrada
-- ======================================================

checkAll :: Program -> [Error]
checkAll prog =
  semanticCheck prog ++ checkProgram prog
