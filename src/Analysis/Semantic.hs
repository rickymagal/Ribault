{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Análise semântica + checagem/inferência de tipos para a linguagem.
-- Inclui dessugar de lambdas toplevel, checagens semânticas e
-- um HM-ish simples para Case, Let, aplicação, bin/un ops e Super.
module Semantic where

import Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State
import Control.Monad (forM, when, foldM, zipWithM, unless)

-- ======================================================
-- 1) Dessugar: lambdas toplevel -> parâmetros
-- ======================================================

desugarDecl :: Decl -> Decl
desugarDecl (FunDecl f [] (Lambda ps e)) = FunDecl f ps e
desugarDecl d                           = d

desugarProgram :: Program -> Program
desugarProgram (Program ds) = Program (map desugarDecl ds)

-- ======================================================
-- 2) Erros (semânticos e de tipo)
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
  deriving Show  -- <- sem Eq (Expr não tem Eq)

data Error
  = SemErr SemanticError
  | TypErr TypeError
  deriving Show

-- ======================================================
-- 3) Ambientes e assinaturas
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
-- 4) Construção de assinaturas e ambiente de funções
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
-- 5) Checagens semânticas
-- ======================================================

semanticCheck :: Program -> [Error]
semanticCheck prog =
  let Program ds = desugarProgram prog
      sig0       = buildSig ds
      counts     = foldr (\f m -> Map.insertWith (+) f 1 m)
                         Map.empty
                         [f | FunDecl f _ _ <- ds]
      dupFs      = [ SemErr (DuplicateFunc f) | (f,n) <- Map.toList counts, n > 1 ]
      errs       = concatMap (map SemErr . checkDecl sig0) ds
  in dupFs ++ errs

checkDecl :: Sig -> Decl -> [SemanticError]
checkDecl sig (FunDecl _ ps b) =
  let env0      = Set.fromList ps
      dupParams = [ DuplicateParam x | x <- ps, length (filter (==x) ps) > 1 ]
  in dupParams ++ checkExpr sig env0 b

checkExpr :: Sig -> Env -> Expr -> [SemanticError]
checkExpr sig env expr = case expr of
  Var x
    | Set.member x env || Map.member x sig -> []
    | otherwise -> [UndefinedVar x]

  Lit _ -> []

  Lambda ps body ->
    let dups   = [ p | p <- ps, length (filter (==p) ps) > 1 ]
        env'   = Set.union env (Set.fromList ps)
    in map DuplicateParam dups ++ checkExpr sig env' body

  If c t e ->
    checkExpr sig env c ++ checkExpr sig env t ++ checkExpr sig env e

  Cons x xs ->
    checkExpr sig env x ++ checkExpr sig env xs

  Case scr alts ->
    checkExpr sig env scr ++ concatMap (checkAlt sig env) alts

  Let ds body ->
    let sig'     = Map.union (buildSig ds) sig
        decls    = [ (f,ps,fBody) | FunDecl f ps fBody <- ds ]
        errsDecl = concat
          [ checkExpr sig' ( Set.union env (Set.fromList (f:ps)) ) fBody
          | (f,ps,fBody) <- decls
          ]
        fnames   = [ f  | (f,_,_) <- decls ]
        params   = concat [ ps | (_,ps,_) <- decls ]
        env'     = Set.union env (Set.fromList (fnames ++ params))
    in errsDecl ++ checkExpr sig env' body

  App{} ->
    let (fn0,args) = flattenApp expr
        eFn       = checkExpr sig env fn0
        eAs       = concatMap (checkExpr sig env) args
        arErr     = case fn0 of
          Var f
            | Just ar <- Map.lookup f sig
            , ar /= length args
              -> [ArityMismatch f ar (length args)]
          _ -> []
    in eFn ++ eAs ++ arErr

  BinOp _ l r ->
    checkExpr sig env l ++ checkExpr sig env r

  UnOp _ x ->
    checkExpr sig env x

  List xs ->
    concatMap (checkExpr sig env) xs

  Tuple xs ->
    concatMap (checkExpr sig env) xs

  Super _ _ inId _ _ ->
    [UndefinedVar inId | not (Set.member inId env || Map.member inId sig)]

checkAlt :: Sig -> Env -> (Pattern, Expr) -> [SemanticError]
checkAlt sig env (pat, bd) =
  let vs   = patVars pat
      dupV = [ DuplicatePatternVar v | v <- vs, length (filter (== v) vs) > 1 ]
      env' = Set.union env (Set.fromList vs)
  in dupV ++ checkExpr sig env' bd

flattenApp :: Expr -> (Expr, [Expr])
flattenApp (App f x) = let (fn0,xs) = flattenApp f in (fn0, xs ++ [x])
flattenApp e         = (e, [])

patVars :: Pattern -> [Ident]
patVars = \case
  PWildcard   -> []
  PVar x      -> [x]
  PLit _      -> []
  PList ps    -> concatMap patVars ps
  PTuple ps   -> concatMap patVars ps
  PCons p ps  -> patVars p ++ patVars ps

-- ======================================================
-- 6) Checagem/inferência de tipos
-- ======================================================

checkProgram :: Program -> [Error]
checkProgram prog =
  let Program ds = desugarProgram prog
      fenv       = buildFuncEnv ds
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
  | TList e  <- expected
  , TList a  <- actual             = unifyReturn e a >> return actual
  | TTuple es <- expected
  , TTuple as <- actual
  , length es == length as         = mapM_ (uncurry unifyReturn) (zip es as) >> return actual
  | TFun ps r <- expected
  , TFun qs s <- actual
  , length ps == length qs         = mapM_ (uncurry unifyReturn) (zip ps qs) >> unifyReturn r s
  | otherwise                      = throwError (Mismatch (Var "<return>") expected actual)

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _        = False

inferExpr :: FuncEnv -> TypeEnv -> Expr -> Infer Type
inferExpr fenv tenv expr = case expr of
  -- VAR: se for função de aridade 0 no fenv, trata como constante (retorno).
  Var x -> case Map.lookup x tenv of
             Just t  -> return t
             Nothing -> case Map.lookup x fenv of
               Just ([], retT)     -> return retT
               Just (argTys, retT) -> return (TFun argTys retT)
               Nothing             -> throwError (UnknownVar x)

  Lit l -> return $ literalType l

  Cons hd tl -> do
    tHd <- inferExpr fenv tenv hd
    tTl <- inferExpr fenv tenv tl
    case tTl of
      TList tEl | match tHd tEl -> return (TList (resolve tHd tEl))
      TVar _ -> return (TList tHd)
      _ -> throwError (Mismatch expr (TList tHd) tTl)

  Lambda ps bd -> do
    tys <- mapM (const freshTypeVar) ps
    let tenv' = Map.union (Map.fromList (zip ps tys)) tenv
    tr <- inferExpr fenv tenv' bd
    return (TFun tys tr)

  If c t e -> do
    _  <- inferExpr fenv tenv c >>= ensureBool c
    tc <- inferExpr fenv tenv t
    te <- inferExpr fenv tenv e
    unifyReturn tc te

  Case scr alts -> do
    scrT <- inferExpr fenv tenv scr
    rs   <- forM alts $ \(pat, bd') -> do
      (vs,pT) <- inferPattern pat
      _ <- unifyReturn scrT pT
      inferExpr fenv (Map.union (Map.fromList vs) tenv) bd'
    case rs of
      (r0:rs') -> foldM unifyReturn r0 rs'
      []       -> throwError (Mismatch scr scrT scrT)

  Let ds e -> do
    let fenv' = Map.union (buildFuncEnv ds) fenv
    tenv' <- foldM (\envAcc d -> case d of
                 FunDecl fn [] bd' -> do
                   t <- inferExpr fenv envAcc bd'
                   return (Map.insert fn t envAcc)
                 _ -> return envAcc
               ) tenv ds
    inferExpr fenv' tenv' e

  App{} -> do
    let (fn0,args) = flattenApp expr
    fty   <- inferExpr fenv tenv fn0
    argTs <- mapM (inferExpr fenv tenv) args
    case fty of
      TFun ps r
        | length ps /= length argTs
          -> throwError (Mismatch expr (TFun ps r) (TFun argTs r))
        | and (zipWith match ps argTs)
          -> return r
        | otherwise
          -> throwError (Mismatch expr (TFun ps r) (TFun argTs r))
      TVar _ -> freshTypeVar
      _      -> throwError (Mismatch expr (TVar "_") fty)

  BinOp op l r -> do
    tl <- inferExpr fenv tenv l
    tr <- inferExpr fenv tenv r
    case op of
      Add -> numBin  op tl tr
      Sub -> numBin  op tl tr
      Mul -> numBin  op tl tr
      Div -> numBin  op tl tr
      Mod -> numBin  op tl tr
      Eq  -> compBin op tl tr
      Neq -> compBin op tl tr
      Lt  -> compBin op tl tr
      Le  -> compBin op tl tr
      Gt  -> compBin op tl tr
      Ge  -> compBin op tl tr
      And -> boolBin op tl tr
      Or  -> boolBin op tl tr

  UnOp op e -> do
    te <- inferExpr fenv tenv e
    case op of
      Neg | te `elem` [TInt,TFloat] -> return te
          | isPoly te               -> freshTypeVar
          | otherwise               -> throwError (UnOpTypeErr op te)
      Not | te == TBool             -> return TBool
          | isPoly te               -> return TBool
          | otherwise               -> throwError (UnOpTypeErr op te)

  List xs -> do
    ts    <- mapM (inferExpr fenv tenv) xs
    eltTy <- case ts of
               []      -> freshTypeVar
               (t:ts') -> foldM (unifyTypes expr) t ts'
    return (TList eltTy)

  Tuple xs -> TTuple <$> mapM (inferExpr fenv tenv) xs

  -- Super: chamada opaca que devolve o mesmo tipo da entrada.
  Super _ _ inId _ _ ->
    case Map.lookup inId tenv of
      Just t  -> return t
      Nothing -> freshTypeVar

  where
    -- unificação recursiva p/ listas/tuplas
    unifyTypes :: Expr -> Type -> Type -> Infer Type
    unifyTypes e t1 t2 = case (t1,t2) of
      (TVar _, t)            -> return t
      (t, TVar _)            -> return t
      (TList a, TList b)     -> TList <$> unifyTypes e a b
      (TTuple as, TTuple bs)
        | length as == length bs -> TTuple <$> zipWithM (unifyTypes e) as bs
      _ | t1 == t2           -> return t1
      _                      -> throwError (Mismatch e t1 t2)

-- | Match permissivo com variáveis de tipo.
match :: Type -> Type -> Bool
match (TVar _) _ = True
match _ (TVar _) = True
match a b        = a == b

resolve :: Type -> Type -> Type
resolve (TVar _) t = t
resolve t       _  = t

inferPattern :: Pattern -> Infer ([(Ident,Type)],Type)
inferPattern = \case
  PVar x    -> do tv <- freshTypeVar; return ([(x,tv)],tv)
  PWildcard -> return ([], TVar "_")
  PLit l    -> return ([], literalType l)
  PList ps  -> do
    xs <- mapM inferPattern ps
    let (vs,ts) = unzip xs
    elemTy <- case ts of
                []      -> freshTypeVar
                (t:ts') -> foldM unify t ts'
    return (concat vs, TList elemTy)
    where
      unify acc t
        | match acc t = return (resolve acc t)
        | otherwise   = throwError
                           (Mismatch (Lit (LString "pattern"))
                                     (TList acc)
                                     (TList t))
  PTuple ps -> do
    xs <- mapM inferPattern ps
    let (vs,ts) = unzip xs
    return (concat vs, TTuple ts)
  PCons p ps -> do
    (v1, t1) <- inferPattern p
    (v2, t2) <- inferPattern ps
    unless (match t2 (TList t1) || isPoly t1 || isPoly t2)
      $ throwError (Mismatch (Lit (LString "pattern")) (TList t1) t2)
    return (v1 ++ v2, TList t1)

literalType :: Literal -> Type
literalType = \case
  LInt _    -> TInt
  LFloat _  -> TFloat
  LBool _   -> TBool
  LChar _   -> TChar
  LString _ -> TString

-- Binops
numBin, boolBin, compBin :: BinOperator -> Type -> Type -> Infer Type
numBin _ TInt   TInt   = return TInt
numBin _ TFloat TFloat = return TFloat
numBin _ (TVar _) t | t `elem` [TInt,TFloat] = return t
numBin _ t (TVar _) | t `elem` [TInt,TFloat] = return t
numBin _ (TVar _) (TVar _) = freshTypeVar
numBin op a b = throwError (BinOpTypeErr op a b)

boolBin _ TBool TBool = return TBool
boolBin _ (TVar _) _  = return TBool
boolBin _ _ (TVar _)  = return TBool
boolBin op a b        = throwError (BinOpTypeErr op a b)

compBin _ a b
  | a==b && a `elem` [TInt,TFloat,TChar,TString] = return TBool
compBin _ (TVar _) _ = return TBool
compBin _ _ (TVar _) = return TBool
compBin op a b       = throwError (BinOpTypeErr op a b)

ensureBool :: Expr -> Type -> Infer ()
ensureBool _ TBool    = return ()
ensureBool _ (TVar _) = return ()
ensureBool e t        = throwError (CondNotBool e t)

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
-- 7) Export API
-- ======================================================

checkAll :: Program -> [Error]
checkAll p = semanticCheck p ++ checkProgram p

-- Atribui nomes s# às supers (feito na main / pipeline).
assignSuperNames :: Program -> Program
assignSuperNames (Program ds) =
  Program (evalState (mapM goDecl ds) (1 :: Int))
  where
    goDecl (FunDecl f ps e) = FunDecl f ps <$> goExpr e

    goExpr :: Expr -> State Int Expr
    goExpr = \case
      Var x       -> pure (Var x)
      Lit l       -> pure (Lit l)
      Lambda ps b -> Lambda ps <$> goExpr b
      If c t e    -> If <$> goExpr c <*> goExpr t <*> goExpr e
      Cons h t    -> Cons <$> goExpr h <*> goExpr t
      Case s as   -> do
        s'  <- goExpr s
        as' <- mapM (\(p,e0) -> do e1 <- goExpr e0; pure (p,e1)) as
        pure (Case s' as')
      Let ds0 e0  -> do
        ds1 <- mapM (\(FunDecl f ps b) -> FunDecl f ps <$> goExpr b) ds0
        e1  <- goExpr e0
        pure (Let ds1 e1)
      App f x     -> App <$> goExpr f <*> goExpr x
      BinOp o l r -> BinOp o <$> goExpr l <*> goExpr r
      UnOp  o e0  -> UnOp  o <$> goExpr e0
      List xs     -> List  <$> mapM goExpr xs
      Tuple xs    -> Tuple <$> mapM goExpr xs
      -- injeta o nome s#
      Super _nm kind inp out body -> do
        i <- get
        put (i + 1)
        let nm = "s" ++ show i
        pure (Super nm kind inp out body)
