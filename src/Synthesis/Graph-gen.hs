{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Gera grafo Dataflow (.dot) no estilo Trebuchet/TALM,
--   com constantes cacheadas, unária, lista única,
--   tuple- & list-pattern no case, inline de Lambdas e FunDecls não-recursivos,
--   e fallback dinâmico para funções recursivas ou higher-order.
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy   as T
import           Data.Char             (ord)
import           Control.Monad.State
import           Control.Monad         (forM, forM_, void, zipWithM_)
import           Data.List             (intercalate)
import           Data.Maybe            (mapMaybe)
import qualified Data.Map.Strict  as M
import           Syntax                ( Program(..)
                                      , Decl(..)
                                      , Expr(..)
                                      , Literal(..)
                                      , Ident
                                      , BinOperator(..)
                                      , UnOperator(..)
                                      , Pattern(..)
                                      )
import           Semantic              (desugarProgram)

-- | Estado do gerador
data CGCtx = CGCtx
  { counter  :: Int
  , nodes    :: [T.Text]
  , edges    :: [T.Text]
  , envVars  :: M.Map Ident String           -- variáveis ligadas a regs
  , defs     :: M.Map Ident ([Ident], Expr)  -- top-level FunDecls
  , cacheC   :: M.Map String String          -- constantes cacheadas
  }

type GenM = State CGCtx

-- | Gera ID único
freshId :: String -> GenM String
freshId base = do
  s <- get
  let i = counter s
  put s { counter = i + 1 }
  pure (base ++ show i)

-- | Cabeçalho do .dot
dotHeader :: [T.Text]
dotHeader =
  [ "digraph G {"
  , "  node [shape=box, style=rounded];"
  , "  node [shape=triangle, style=solid];"
  ]

-- | Emite nó arredondado
emitNode :: String -> String -> GenM String
emitNode base lbl = do
  nid <- freshId base
  let line = T.pack $ nid ++ " [label=\"" ++ lbl ++
               "\", shape=box, style=rounded];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Emite nó triangular de steer
emitSteer :: GenM String
emitSteer = do
  nid <- freshId "steer"
  let line = T.pack $ nid ++ " [label=\"T   F\", shape=triangle, style=solid];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Emite constante cacheada (int, float, char/bool→ord)
emitConst :: String -> GenM String
emitConst lbl = do
  cmap <- gets cacheC
  case M.lookup lbl cmap of
    Just nid -> pure nid
    Nothing  -> do
      nid <- freshId "const"
      let line = T.pack $ nid ++ " [label=\"" ++ lbl ++
                   "\", shape=box, style=rounded];"
      modify $ \s -> s
        { nodes  = nodes s ++ [line]
        , cacheC = M.insert lbl nid (cacheC s)
        }
      pure nid

-- | Emite aresta com tail/head ports
emitEdge :: String -> String -> [(String,String)] -> GenM ()
emitEdge from to attrs = do
  let a    = intercalate ", " [k ++ "=" ++ v | (k,v) <- attrs]
      line = T.pack $ from ++ " -> " ++ to ++ " [" ++ a ++ "];"
  modify $ \s -> s { edges = edges s ++ [line] }

-- | Liga variável a reg
bindVar :: Ident -> String -> GenM ()
bindVar x r = modify $ \s ->
  s { envVars = M.insert x r (envVars s) }

-- | Procura variável; se não achar, mas for FunDecl top-level, gera node de função
lookupVar :: Ident -> GenM String
lookupVar x = do
  ev <- gets envVars
  case M.lookup x ev of
    Just r  -> pure r
    Nothing -> do
      m <- lookupDef x
      case m of
        Just _ -> do
          -- gera um nó 'lambda' para a função e o liga
          r <- emitNode "lambda" x
          bindVar x r
          pure r
        Nothing ->
          error $ "variável não ligada: " ++ show x

-- | Registra FunDecl para inline/recursão
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s ->
  s { defs = M.insert f (ps,e) (defs s) }

-- | Procura definição top-level
lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = gets (M.lookup f . defs)

-- | Entrada: gera .dot inteiro
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState (mapM_ recordDecl decls >> mapM_ genDecl decls) initial
  in T.unlines (dotHeader ++ ns ++ es ++ ["}"])

-- | Registra todos os FunDecls top-level
recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b
recordDecl _                = pure ()

-- | Gera corpo de FunDecls sem parâmetros
genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] b) = void (genExpr [] b)
genDecl _                = pure ()

-- | Geração recursiva de expressõe
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  -- Variável ou função: busca em envVars (pré-vincula FunDecls a um reg “lambda”)
  Var x ->
    lookupVar x

  -- Literais
  Lit lit -> case lit of
    LInt i     -> emitConst ("const#" ++ show i)
    LBool b    -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c    -> emitConst ("const#" ++ show (ord c))
    LFloat f   -> emitConst ("fconst#" ++ show f)
    LString s  -> genExpr stack (List (map (Lit . LChar) s))

  -- Operadores unários
  UnOp op e -> do
    r <- genExpr stack e
    case op of
      Neg -> do
        n <- emitNode "neg" "-"
        emitEdge r n [("tailport","s"),("headport","nw")]
        pure n
      Not -> do
        n <- emitNode "not" "not"
        emitEdge r n [("tailport","s"),("headport","nw")]
        pure n

  -- Aplicação de função / inline / dynamic call
  App f x -> do
    -- desenrola aplicações curried em (hd, args)
    let flatten (App f' a) =
          let (g, xs) = flatten f' in (g, xs ++ [a])
        flatten e = (e, [])
        (hd, args) = flatten (App f x)

        dynCall funReg argRegs = do
          cg <- emitNode "callgroup" ("callgroup(" ++ funReg ++ ")")
          zipWithM_ (\i a -> do
              sndN <- emitNode "callsnd" ("callsnd(" ++ funReg ++ "," ++ show i ++ ")")
              emitEdge a    sndN [("tailport","s"),("headport","n")]
              emitEdge sndN cg   [("tailport","s"),("headport","nw")]
            ) [1..] argRegs
          retN <- emitNode "retsnd" ("retsnd(" ++ funReg ++ ")")
          emitEdge cg retN [("tailport","s"),("headport","n")]
          pure retN

    case hd of
      -- A) FunDecl top-level completamente aplicado → inline corpo
      Var fid -> do
        mdef <- lookupDef fid
        case mdef of
          Just (ps, body)
            | length ps == length args
           && fid `notElem` stack -> do
              regs   <- mapM (genExpr stack) args
              oldEnv <- gets envVars
              zipWithM_ bindVar ps regs
              res <- genExpr (fid:stack) body
              modify $ \s -> s { envVars = oldEnv }
              pure res
          _ -> do
            funReg  <- lookupVar fid
            argRegs <- mapM (genExpr stack) args
            dynCall funReg argRegs

      -- B) Lambda saturada → inline corpo
      Lambda ps body
        | length ps == length args -> do
            regs   <- mapM (genExpr stack) args
            oldEnv <- gets envVars
            zipWithM_ bindVar ps regs
            res    <- genExpr ("<lam>":stack) body
            modify $ \s -> s { envVars = oldEnv }
            pure res

      -- C) Caso geral → dynamic call
      _ -> do
        funReg  <- genExpr stack hd
        argRegs <- mapM (genExpr stack) args
        dynCall funReg argRegs

  -- Operador binário
  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let (nm,sym) = case op of
          Add -> ("add","+"); Sub -> ("sub","-")
          Mul -> ("mul","*"); Div -> ("div","/")
          Mod -> ("mod","%"); Eq  -> ("eq","==")
          Neq -> ("neq","!="); Lt -> ("lt","<")
          Le  -> ("le","<="); Gt -> ("gt",">")
          Ge  -> ("ge",">="); And -> ("and","&&")
          Or  -> ("or","||")
    n <- emitNode nm sym
    emitEdge ra n [("tailport","s"),("headport","nw")]
    emitEdge rb n [("tailport","s"),("headport","ne")]
    pure n

  -- If-then-else → steer
  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n  <- emitSteer
    emitEdge rc n [("tailport","s"),("headport","n")]
    emitEdge rt n [("tailport","se"),("headport","ne")]
    emitEdge re n [("tailport","sw"),("headport","nw")]
    pure n

  -- Let-binding inline de FunDecl aridade-0
  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) ->
      bindVar x =<< genExpr stack ex
    genExpr stack body

  -- Tupla literal
  Tuple es -> do
    rs  <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lista literal → nó único S1
  List es -> do
    rs  <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lambda isolada
  Lambda{} ->
    emitNode "lambda" "lambda"

  -- Case com tuple- & list-patterns (genérico para PList de qualquer tamanho)
  Case scrut alts -> do
    sid   <- genExpr stack scrut
    altRs <- forM alts $ \(pat, expr) ->
      case pat of
        -- Lista enumerada de qualquer tamanho
        PList pats -> do
          -- vincula cada variável PVar ao mesmo reg do scrutinee
          forM_ pats $ \p -> case p of
            PVar v -> bindVar v sid
            _      -> pure ()
          -- agora gera o corpo da alternativa
          genExpr stack expr

        -- Tuple-pattern
        PTuple ps | any isPVar ps -> do
          forM_ [v | PVar v <- ps] $ \v -> bindVar v sid
          genExpr stack expr

        -- Var-pattern simples
        PVar v -> bindVar v sid >> genExpr stack expr

        -- fallback
        _ -> genExpr stack expr

    n <- emitSteer
    emitEdge sid n [("tailport","s"),("headport","n")]
    case altRs of
      [r]      -> emitEdge r n [("tailport","s"),("headport","ne")]
      [rT,rF]  -> do
        emitEdge rT n [("tailport","se"),("headport","ne")]
        emitEdge rF n [("tailport","sw"),("headport","nw")]
      rs       -> zipWithM_ (\r p -> emitEdge r n [("tailport","s"),("headport",p)])
                            rs (cycle ["ne","nw","se","sw"])
    pure n

-- | Auxiliar para Pattern
isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False
