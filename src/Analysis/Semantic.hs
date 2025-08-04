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

  -- 1) Variável ou função top-level
  Var x
    | Set.member x env
      || Map.member x sig
      -> []
    | otherwise
      -> [UndefinedVar x]

  -- 2) Literal nunca dá erro semântico
  Lit _ 
    -> []

  -- 3) Lambda: detecta parâmetros duplicados, depois checa o corpo
  Lambda ps body
    -> let
         -- parâmetros repetidos
         dupParams = [ p | p <- ps
                         , length (filter (==p) ps) > 1
                         ]
         -- ambiente com os parâmetros em escopo
         env'      = Set.union env (Set.fromList ps)
         -- erros do corpo
         errsBody  = checkExpr sig env' body
       in
         -- primeiro os DuplicateParam, depois os erros do corpo
         map DuplicateParam dupParams ++ errsBody

  -- 4) If: condição e ambos os ramos
  If c t e
    -> checkExpr sig env c
    ++ checkExpr sig env t
    ++ checkExpr sig env e

  -- 5) Cons: cabeça e cauda
  Cons x xs
    -> checkExpr sig env x
    ++ checkExpr sig env xs

  -- 6) Case: scrutinee e cada alternativa
  Case scr alts
    -> checkExpr sig env scr
    ++ concatMap (checkAlt sig env) alts

  -- 7) Let-in: inclui nomes e parâmetros locais no ambiente
  Let ds body
    -> let
         -- 7a) estende a assinatura com as local decls
         sig' = Map.union (buildSig ds) sig

         -- 7b) para cada FunDecl f ps fBody, cria ambiente local
         decls    = [ (f,ps,fBody) | FunDecl f ps fBody <- ds ]
         errsDecl = concat
           [ checkExpr sig' ( Set.union env (Set.fromList (f:ps)) )
                         fBody
           | (f,ps,fBody) <- decls
           ]

         -- 7c) ambiente do corpo let: todas as funções e parâmetros locais
         fnames = [ f  | (f,_,_) <- decls ]
         params = concat [ ps | (_,ps,_) <- decls ]
         env'   = Set.union env (Set.fromList (fnames ++ params))

       in
         errsDecl ++ checkExpr sig' env' body

  -- 8) Application: achata, checa fn e args, depois aridade
  App{}  
    -> let
         (fn,args) = flattenApp expr
         eFn       = checkExpr sig env fn
         eAs       = concatMap (checkExpr sig env) args
         arErr     = case fn of
           Var f
             | Just ar <- Map.lookup f sig
             , ar /= length args
               -> [ArityMismatch f ar (length args)]
           _ -> []
       in
         eFn ++ eAs ++ arErr

  -- 9) BinOp: esquerda e direita
  BinOp _ l r
    -> checkExpr sig env l ++ checkExpr sig env r

  -- 10) UnOp: operando
  UnOp _ x
    -> checkExpr sig env x

  -- 11) Lista literal: checa cada elemento
  List xs
    -> concatMap (checkExpr sig env) xs

  -- 12) Tupla literal: checa cada componente
  Tuple xs
    -> concatMap (checkExpr sig env) xs

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
  PCons p ps -> patVars p ++ patVars ps
  
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
  -- variável de tipo no declarado: aceita o inferido
  | TVar _    <- expected          = return actual
  -- variável de tipo no inferido: aceita o declarado
  | TVar _    <- actual            = return expected
  -- ambos literais/idênticos: ok
  | expected == actual             = return actual
  -- listas homônomas: unifica o elemento
  | TList e  <- expected
  , TList a  <- actual             = unifyReturn e a >> return actual
  -- tuplas do mesmo tamanho: unifica cada componente
  | TTuple es <- expected
  , TTuple as <- actual
  , length es == length as         = mapM_ (uncurry unifyReturn) (zip es as) >> return actual
  -- caso contrário é mismatch de retorno
  | otherwise                      = throwError (Mismatch (Var "<return>") expected actual)

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _        = False

-- | Infer the type of an expression.
inferExpr :: FuncEnv -> TypeEnv -> Expr -> Infer Type
inferExpr fenv tenv expr = case expr of
  Var x -> case Map.lookup x tenv of
             Just t  -> return t
             Nothing -> case Map.lookup x fenv of
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

  UnOp op e -> do
    te <- inferExpr fenv tenv e
    case op of
      Neg | te `elem` [TInt,TFloat] -> return te
          | isPoly te               -> return TInt
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

  where
    -- unifica dois tipos, descendo em listas e tuplas
    unifyTypes :: Expr -> Type -> Type -> Infer Type
    unifyTypes e t1 t2 = case (t1,t2) of
      (TVar _, t)            -> return t
      (t, TVar _)            -> return t

      (TList a, TList b)     -> TList <$> unifyTypes e a b

      (TTuple as, TTuple bs)
        | length as == length bs -> do
            cs <- zipWithM (unifyTypes e) as bs
            return (TTuple cs)

      _ | t1 == t2           -> return t1
      _                      -> throwError (Mismatch e t1 t2)
-- | Match two types, allowing type variables.
match :: Type -> Type -> Bool
match (TVar _) _ = True
match _ (TVar _) = True
match a b        = a == b

resolve :: Type -> Type -> Type
resolve (TVar _) t = t
resolve t       _  = t

-- | Infer types for pattern variables and return pattern type.
inferPattern :: Pattern -> Infer ([(Ident,Type)],Type)
inferPattern = \case
  PVar x    -> do tv <- freshTypeVar; return ([(x,tv)],tv)
  PWildcard -> return ([], TVar "_")
  PLit l    -> return ([], literalType l)
    -- padrão de lista
  PList ps -> do
    xs <- mapM inferPattern ps              -- xs :: [(vars,ty)]
    let (vs,ts) = unzip xs                  -- vs = variáveis, ts = tipos
    elemTy <- case ts of
                []      -> freshTypeVar     -- lista vazia → polimórfica
                (t:ts') -> foldM unify t ts'
    return (concat vs, TList elemTy)
    where
      -- une dois tipos, permitindo variáveis
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
ensureBool _ TBool    = return ()
ensureBool _ (TVar _) = return ()    -- aceita variável de tipo como “bool”
ensureBool e t        = throwError (CondNotBool e t)

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