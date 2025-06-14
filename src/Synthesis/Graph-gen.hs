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
  , envVars  :: M.Map Ident String
  , defs     :: M.Map Ident ([Ident], Expr)
  , cacheC   :: M.Map String String
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

-- | Emite constante cacheada
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

-- | Procura variável; se for FunDecl top-level, gera nodo lambda
lookupVar :: Ident -> GenM String
lookupVar x = do
  ev <- gets envVars
  case M.lookup x ev of
    Just r  -> pure r
    Nothing -> do
      m <- lookupDef x
      case m of
        Just _ -> do
          r <- emitNode "lambda" x
          bindVar x r
          pure r
        Nothing ->
          error $ "variável não ligada: " ++ show x

-- | Registra FunDecls para inline/recursão
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s ->
  s { defs = M.insert f (ps,e) (defs s) }

-- | Procura definição top-level
lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = gets (M.lookup f . defs)

-- | Gera .dot completo
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial       = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState (mapM_ recordDecl decls >> mapM_ genDecl decls) initial
  in T.unlines (dotHeader ++ ns ++ es ++ ["}"])

recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b
recordDecl _                = pure ()

genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] b) = void (genExpr [] b)
genDecl _                = pure ()

-- | Auxiliar para patterns
isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False

-- | Geração de expressões com suporte a applyN e case exaustivo
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  Var x ->
    lookupVar x

  Lit lit -> case lit of
    LInt i     -> emitConst ("const#" ++ show i)
    LBool b    -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c    -> emitConst ("const#" ++ show (ord c))
    LFloat f   -> emitConst ("fconst#" ++ show f)
    LString s  ->
      genExpr stack (List (map (Lit . LChar) s))

  UnOp op e -> do
    r <- genExpr stack e
    let mkUn sym = do
          n <- emitNode sym sym
          emitEdge r n [("tailport","s"),("headport","nw")]
          pure n
    case op of
      Neg -> mkUn "-"
      Not -> mkUn "not"

  App f x -> do
    let flatten (App f' a) =
          let (g,xs) = flatten f' in (g, xs ++ [a])
        flatten e = (e, [])
        (hd, args) = flatten (App f x)

        fallback :: GenM String
        fallback = do
          funReg  <- genExpr stack hd
          argRegs <- mapM (genExpr stack) args
          let instr = "apply" ++ show (length argRegs)
          node <- emitNode instr instr
          emitEdge funReg node    [("tailport","s"),("headport","nw")]
          forM_ argRegs $ \a ->
            emitEdge a node       [("tailport","s"),("headport","ne")]
          pure node

    case hd of
      Var fid -> do
        lookupDef fid >>= \case
          Just (ps, body)
            | length ps == length args
           && fid `notElem` stack -> do
              regs   <- mapM (genExpr stack) args
              oldEnv <- gets envVars
              zipWithM_ bindVar ps regs
              res    <- genExpr (fid:stack) body
              modify $ \s -> s { envVars = oldEnv }
              pure res
          _ -> fallback

      Lambda ps body
        | length ps == length args -> do
            regs   <- mapM (genExpr stack) args
            oldEnv <- gets envVars
            zipWithM_ bindVar ps regs
            res    <- genExpr ("<lam>":stack) body
            modify $ \s -> s { envVars = oldEnv }
            pure res

      _ -> fallback

  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let (nm,sym) = case op of
          Add -> ("add","+"); Sub -> ("sub","-")
          Mul -> ("mul","*"); Div -> ("div","/")
          Mod -> ("mod","%"); Eq  -> ("eq","==")
          Neq -> ("neq","!="); Lt -> ("lt","<")
          Le  -> ("le","<="); Gt  -> ("gt",">")
          Ge  -> ("ge",">="); And -> ("and","&&")
          Or  -> ("or","||")
    n <- emitNode nm sym
    emitEdge ra n [("tailport","s"),("headport","nw")]
    emitEdge rb n [("tailport","s"),("headport","ne")]
    pure n

  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n  <- emitSteer
    emitEdge rc n [("tailport","s"),("headport","n")]
    emitEdge rt n [("tailport","se"),("headport","ne")]
    emitEdge re n [("tailport","sw"),("headport","nw")]
    pure n

  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) ->
      bindVar x =<< genExpr stack ex
    genExpr stack body

  Tuple es -> do
    rs <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  List es -> do
    rs  <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  Lambda{} ->
    emitNode "lambda" "lambda"

  Case scrut alts -> do
    -- detecta exaustividade (último padrão '_' ou PWildcard)
    let exhaustive = case alts of
          [] -> False
          xs -> case fst (last xs) of PWildcard -> True; _ -> False

    sid <- genExpr stack scrut

    -- default: corpo do último caso se exaustivo, erro caso não
    defaultReg <- if exhaustive
      then let (_, exprDef) = last alts in genExpr stack exprDef
      else error "Case não-exaustivo sem padrão catch-all"

    -- padrões a testar (todos menos o último se exaustivo)
    let tests = if exhaustive then init alts else alts

    -- encadeia switch + steer para cada pattern
    let go [] falReg = pure falReg
        go ((pat, expr):rest) falReg = do
          -- gera guardReg e lista de variáveis para bind
          (guardReg, binds) <- case pat of
            PList ps -> do
              let vars = [v | PVar v <- ps]
              sw <- emitNode ("switchList" ++ show (length ps))
                             ("switchList" ++ show (length ps))
              emitEdge sid sw [("tailport","s"),("headport","n")]
              pure (sw, vars)

            PTuple ps | any isPVar ps -> do
              let vars = [v | PVar v <- ps]
              sw <- emitNode ("matchtuple" ++ show (length ps))
                             ("matchtuple" ++ show (length ps))
              emitEdge sid sw [("tailport","s"),("headport","n")]
              pure (sw, vars)

            PVar v -> do
              c <- emitConst "const#1"
              pure (c, [v])

            PWildcard -> do
              c <- emitConst "const#1"
              pure (c, [])

            _ ->
              error $ "Pattern não suportado: " ++ show pat

          -- steer
          st <- emitSteer
          emitEdge guardReg st [("tailport","s"),("headport","n")]

          -- projeções + bind
          projRegs <- forM (zip binds [0..]) $ \(_,i) -> do
            pr <- emitNode ("proj" ++ show i) ("proj" ++ show i)
            let port = if i == 0 then "nw" else "ne"
            emitEdge sid pr [("tailport","s"),("headport",port)]
            pure pr
          forM_ (zip binds projRegs) $ \(v,r) -> bindVar v r

          -- corpo
          br <- genExpr stack expr
          -- <–––– TRUE: conectar à esquerda (nw)
          emitEdge br st [("tailport","sw"),("headport","nw")]
          -- <–––– FALSE: conectar à direita (ne)
          emitEdge falReg st [("tailport","se"),("headport","ne")]

          go rest st

    go tests defaultReg
