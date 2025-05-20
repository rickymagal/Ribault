{-# LANGUAGE LambdaCase, FlexibleContexts #-}

-- | Module providing semantic analysis and type checking for a simple functional language.
-- It includes desugaring of top-level lambdas, semantic error checking, and Hindley-Milner
-- type inference with support for Case, Let, and generic application.
module Semantic where

import Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State
import Control.Monad (forM, when, foldM)

-- ======================================================
-- 1) Desugaring: transform top-level lambdas into parameters
-- ======================================================

-- | Transform a top-level function declaration with a lambda body
-- into an equivalent declaration with explicit parameters.
desugarDecl :: Decl -> Decl
-- | Perform desugaring only when the function has no parameters
-- and its body is a lambda abstraction.
desugarDecl (FunDecl f [] (Lambda ps e)) = FunDecl f ps e
-- | Leave other declarations unchanged.
desugarDecl d                           = d

-- | Apply desugaring to all declarations in a program.
desugarProgram :: Program -> Program
-- | Desugar each declaration in the program.
desugarProgram (Program ds) = Program (map desugarDecl ds)

-- ======================================================
-- 2) Errors (semantic and type errors)
-- ======================================================

-- | Semantic errors detected during scope and arity checking.
data SemanticError
  = UndefinedVar Ident          -- ^ Variable used without definition
  | ArityMismatch Ident Int Int -- ^ Function called with wrong number of arguments
  | DuplicateFunc Ident         -- ^ Function name defined more than once
  | DuplicateParam Ident        -- ^ Parameter name appears multiple times in declaration
  | DuplicatePatternVar Ident   -- ^ Pattern variable appears multiple times in the same pattern
  deriving (Show, Eq)

-- | Type errors detected during type inference and checking.
data TypeError
  = Mismatch Expr Type Type       -- ^ Expression has unexpected type
  | CondNotBool Expr Type         -- ^ Condition expression is not boolean
  | BranchesTypeDiffer Expr Expr Type Type -- ^ Then/else branches have different types
  | BinOpTypeErr BinOperator Type Type -- ^ Binary operator applied to incompatible types
  | UnOpTypeErr UnOperator Type   -- ^ Unary operator applied to non-matching type
  | UnknownVar Ident              -- ^ Variable not found in type environment
  deriving Show

-- | Combined error type for semantic or type errors.
data Error
  = SemErr SemanticError  -- ^ A semantic error occurred
  | TypErr TypeError      -- ^ A type error occurred
  deriving Show

-- ======================================================
-- 3) Environments and signatures
-- ======================================================

-- | Signature mapping function identifiers to their arity.
type Sig     = Map.Map Ident Int

-- | Environment of variables currently in scope.
type Env     = Set.Set Ident

-- | Types in the language, including base types, lists, tuples, type variables, and function types.
data Type
  = TInt | TFloat | TBool | TChar | TString    -- ^ Primitive types
  | TList Type       -- ^ Homogeneous list types
  | TTuple [Type]    -- ^ Tuple types with fixed arity
  | TVar String      -- ^ Type variable for inference
  | TFun [Type] Type -- ^ Function type with argument types and return type
  deriving (Eq, Show)

-- | Typing environment: map from identifiers to their inferred types.
type TypeEnv = Map.Map Ident Type

-- | Function environment: map from top-level function names to their argument types and return type.
type FuncEnv = Map.Map Ident ([Type], Type)

-- | State for generating fresh type variables during inference.
data InferState = InferState { count :: Int }

-- | Inference monad combining state for fresh variables and error handling.
type Infer a    = ExceptT TypeError (State InferState) a

-- ======================================================
-- 4) Building signatures and function environment
-- ======================================================

-- | Build a signature from a list of function declarations.
-- Records arity for each function, ignoring duplicates.
buildSig :: [Decl] -> Sig
buildSig = foldr (\(FunDecl f ps _) acc ->
                    Map.insertWith (const id) f (length ps) acc)
                 Map.empty

-- | Create an initial function environment using fresh type variables for arguments and return.
buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = Map.fromList . map (\(FunDecl f args _) ->
  let tvs = replicate (length args) (TVar "_")
      tr  = TVar ("r_" ++ f)
  in (f, (tvs, tr)))

-- ======================================================
-- 5) Semantic checking
-- ======================================================

-- | Perform semantic checks (undefined variables, arity, duplicates) on a program.
semanticCheck :: Program -> [Error]
semanticCheck prog =
  let Program ds = desugarProgram prog
      sig0  = buildSig ds
      dupFs = [ SemErr (DuplicateFunc f) | (f,n) <- Map.toList sig0, n < 0 ]
      errs  = concatMap (checkDecl sig0) ds
  in dupFs ++ map SemErr errs

-- | Check a single function declaration for semantic errors.
checkDecl :: Sig -> Decl -> [SemanticError]
checkDecl sig (FunDecl _ ps b) =
  let env0      = Set.fromList ps
      dupParams = [ DuplicateParam x | x <- ps, length (filter (==x) ps) > 1 ]
  in dupParams ++ checkExpr sig env0 b

-- | Recursively check an expression for semantic errors given current signature and environment.
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
  If c t e    -> concatMap (checkExpr sig env) [c,t,e]
  Case s alts -> checkExpr sig env s ++ concatMap (checkAlt sig env) alts
  Let ds e    ->
    let sig' = Map.union (buildSig ds) sig
        errsD = concatMap (\(FunDecl _ ps bd) ->
                  checkExpr sig' (Set.union env (Set.fromList ps)) bd
                ) ds
        env'  = Set.union env (Set.fromList (concatMap (\(FunDecl _ ps _) -> ps) ds))
    in errsD ++ checkExpr sig' env' e
  App{}       ->
    let (fn, args) = flattenApp expr
        e1 = checkExpr sig env fn
        e2 = concatMap (checkExpr sig env) args
        ar = case fn of
          Var f | Just ar <- Map.lookup f sig, ar /= length args
                -> [ArityMismatch f ar (length args)]
          _ -> []
    in e1 ++ e2 ++ ar
  BinOp _ l r  -> checkExpr sig env l ++ checkExpr sig env r
  UnOp _ x     -> checkExpr sig env x
  List xs      -> concatMap (checkExpr sig env) xs
  Tuple xs     -> concatMap (checkExpr sig env) xs

-- | Check a case alternative for duplicate pattern variables and nested errors.
checkAlt :: Sig -> Env -> (Pattern, Expr) -> [SemanticError]
checkAlt sig env (pat, bd) =
  let vs   = patVars pat
      dupV = [ DuplicatePatternVar v | v <- vs, length (filter (==v) vs) > 1 ]
      env' = Set.union env (Set.fromList vs)
  in dupV ++ checkExpr sig env' bd

-- | Flatten nested applications into function and argument list.
flattenApp :: Expr -> (Expr, [Expr])
flattenApp (App f x) = let (fn,xs) = flattenApp f in (fn, xs ++ [x])
flattenApp e         = (e, [])

-- | Extract variables from a pattern.
patVars :: Pattern -> [Ident]
patVars = \case
  PWildcard   -> []
  PVar x      -> [x]
  PLit _      -> []
  PList ps    -> concatMap patVars ps
  PTuple ps   -> concatMap patVars ps

-- ======================================================
-- 6) Type checking and inference
-- ======================================================

-- | Perform both semantic and type checking on a program.
checkProgram :: Program -> [Error]
checkProgram prog =
  let Program ds = desugarProgram prog
      fenv        = buildFuncEnv ds
  in concatMap (runDecl fenv Map.empty) ds
  where
    -- | Infer and unify return type for a function declaration.
    runDecl fenv tenv (FunDecl f ps b) =
      let argTys = replicate (length ps) (TVar "_")
          retTy  = TVar ("r_" ++ f)
          env'   = Map.union (Map.fromList (zip ps argTys)) tenv
          act    = inferExpr fenv env' b >>= unifyReturn retTy
          st0    = InferState 0
      in case runState (runExceptT act) st0 of
           (Left te, _) -> [TypErr te]
           _            -> []

-- | Unify expected and actual return types, allowing type variables.
unifyReturn :: Type -> Type -> Infer Type
unifyReturn expected actual
  | TVar _ <- expected = return actual
  | expected == actual = return actual
  | otherwise          = throwError (Mismatch (Lit (LString "return")) expected actual)

-- | Infer the type of an expression.
inferExpr :: FuncEnv -> TypeEnv -> Expr -> Infer Type
inferExpr fenv tenv expr = case expr of
  Var x -> case Map.lookup x tenv of
             Just t  -> return t
             Nothing -> case Map.lookup x fenv of
                          Just (argTys, retT) -> return (TFun argTys retT)
                          Nothing             -> throwError (UnknownVar x)
  Lit l -> return $ literalType l

  Lambda ps bd -> do
    tys <- mapM (const freshTypeVar) ps
    let tenv' = Map.union (Map.fromList (zip ps tys)) tenv
    tr <- inferExpr fenv tenv' bd
    return (TFun tys tr)

  If c t e -> do
    _  <- inferExpr fenv tenv c >>= ensureBool c
    tc <- inferExpr fenv tenv t; te <- inferExpr fenv tenv e
    case (tc, te) of
      (a,b) | a==b       -> return a
      (TVar _, x)        -> return x
      (x, TVar _)        -> return x
      _                  -> throwError (BranchesTypeDiffer t e tc te)

  Case scr alts -> do
    scrT <- inferExpr fenv tenv scr
    rs   <- forM alts $ \(pat, bd') -> do
      (vs,pT) <- inferPattern pat
      when (not (isPoly scrT || isPoly pT) && pT /= scrT)
        $ throwError (Mismatch scr pT scrT)
      inferExpr fenv (Map.union (Map.fromList vs) tenv) bd'
    case rs of
      (r0:rs') -> do
        t <- foldM (\t1 t2 ->
                if t1==t2 then return t1
                else if isPoly t1 then return t2
                else if isPoly t2 then return t1
                else throwError (BranchesTypeDiffer scr scr t1 t2)
              ) r0 rs'
        return t
      [] -> throwError (Mismatch scr scrT scrT)

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
    let (fn,args) = flattenApp expr
    fty   <- inferExpr fenv tenv fn
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
    tl <- inferExpr fenv tenv l; tr <- inferExpr fenv tenv r
    case op of
      Add -> numBin  op tl tr; Sub -> numBin  op tl tr
      Mul -> numBin  op tl tr; Div -> numBin  op tl tr; Mod -> numBin  op tl tr
      Eq  -> compBin op tl tr; Neq -> compBin op tl tr
      Lt  -> compBin op tl tr; Le  -> compBin op tl tr
      Gt  -> compBin op tl tr; Ge  -> compBin op tl tr

  UnOp op x -> do
    tx <- inferExpr fenv tenv x
    case op of
      Neg -> if tx `elem` [TInt,TFloat] then return tx else throwError (UnOpTypeErr op tx)
      Not -> if tx == TBool            then return TBool else throwError (UnOpTypeErr op tx)

  List xs -> do
    ts <- mapM (inferExpr fenv tenv) xs
    case ts of
      []      -> freshTypeVar
      (t:ts') | all (==t) ts' -> return (TList t)
      _       -> throwError (Mismatch expr (TList (head ts)) (TList (last ts)))

  Tuple xs -> TTuple <$> mapM (inferExpr fenv tenv) xs

-- | Match two types, allowing type variables.
match :: Type -> Type -> Bool
match (TVar _) _ = True
match _ (TVar _) = True
match a b        = a == b

-- | Infer types for pattern variables and return pattern type.
inferPattern :: Pattern -> Infer ([(Ident,Type)],Type)
inferPattern = \case
  PVar x    -> do tv <- freshTypeVar; return ([(x,tv)],tv)
  PWildcard -> return ([], TVar "_")
  PLit l    -> return ([], literalType l)
  PList ps  -> do
    xs <- mapM inferPattern ps
    let (vs,ts)   = unzip xs
        nonWildTs = [ t | (p,t) <- zip ps ts, case p of PWildcard -> False; _ -> True ]
    case nonWildTs of
      (t:ts') | all (==t) ts' -> return (concat vs, TList t)
      []                      -> do tv <- freshTypeVar; return (concat vs, TList tv)
      (t:_)                   -> throwError (Mismatch (Lit (LString "pattern")) (TList t) (TList t))
  PTuple ps -> do
    xs <- mapM inferPattern ps
    let (vs,ts) = unzip xs
    return (concat vs, TTuple ts)

-- | Determine the type of a literal.
literalType :: Literal -> Type
literalType = \case
  LInt _    -> TInt
  LFloat _  -> TFloat
  LBool _   -> TBool
  LChar _   -> TChar
  LString _ -> TString

-- | Type-check numeric binary operators.
numBin, boolBin, compBin :: BinOperator -> Type -> Type -> Infer Type
numBin _ TInt   TInt   = return TInt
numBin _ TFloat TFloat = return TFloat
numBin op a b
  | isPoly a || isPoly b = return TInt
  | otherwise            = throwError (BinOpTypeErr op a b)

-- | Type-check boolean binary operators.
boolBin _ TBool TBool = return TBool
boolBin op a b
  | isPoly a || isPoly b = return TBool
  | otherwise            = throwError (BinOpTypeErr op a b)

-- | Type-check comparison binary operators.
compBin _ a b
  | a==b && a `elem` [TInt,TFloat,TChar,TString] = return TBool
compBin _ a b
  | isPoly a || isPoly b = return TBool
compBin op a b = throwError (BinOpTypeErr op a b)

-- | Ensure an expression has boolean type.
ensureBool :: Expr -> Type -> Infer ()
ensureBool _ TBool = return ()
ensureBool e t     = throwError (CondNotBool e t)

-- | Generate a fresh type variable.
freshTypeVar :: Infer Type
freshTypeVar = do
  s <- get
  let n = count s + 1
  put s { count = n }
  return (TVar ("t" ++ show n))

-- | Check if a type is polymorphic (a type variable).
isPoly :: Type -> Bool
isPoly (TVar _) = True
isPoly _        = False

-- ======================================================
-- 7) Export API
-- ======================================================

-- | Run full semantic and type checks on a program.
checkAll :: Program -> [Error]
checkAll p = semanticCheck p ++ checkProgram p
