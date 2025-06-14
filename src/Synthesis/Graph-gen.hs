{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Gera grafo Dataflow (.dot) no estilo Trebuchet/TALM,
--   com constantes inline, lista única, tuple-pattern no case sem nó extra.
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy   as T
import           Data.Char             (ord, toUpper)
import           Control.Monad.State
import           Data.List             (intercalate)
import qualified Data.Map.Strict  as M
import           Syntax                ( Program(..)
                                      , Decl(..)
                                      , Expr(..)
                                      , Literal(..)
                                      , Ident
                                      , BinOperator(..)
                                      , Pattern(..)
                                      )
import           Semantic              (desugarProgram)

-- Estado do gerador
data CGCtx = CGCtx
  { counter  :: Int
  , nodes    :: [Text]
  , edges    :: [Text]
  , envVars  :: M.Map Ident String
  , defs     :: M.Map Ident ([Ident], Expr)
  , cacheC   :: M.Map String String
  }

type GenM = State CGCtx

-- | Identificador único
freshId :: String -> GenM String
freshId base = do
  s <- get
  let i = counter s
  put s { counter = i + 1 }
  pure (base ++ show i)

-- | Cabeçalho .dot
dotHeader :: [Text]
dotHeader =
  [ "digraph G {"
  , "  node [shape=box, style=rounded];"
  , "  node [shape=triangle, style=solid];"
  ]

-- | Nó arredondado genérico
emitNode :: String -> String -> GenM String
emitNode base lbl = do
  nid <- freshId base
  let line = T.pack $
        nid
        ++ " [label=\"" ++ lbl ++ "\","
        ++ " shape=box,"
        ++ " style=rounded];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Nó triangular de steer
emitSteer :: GenM String
emitSteer = do
  nid <- freshId "steer"
  let line = T.pack $
        nid
        ++ " [label=\"T   F\","
        ++ " shape=triangle,"
        ++ " style=solid];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Constantes cacheadas (int, float, char/bool → ord)
emitConst :: String -> GenM String
emitConst lbl = do
  cmap <- gets cacheC
  case M.lookup lbl cmap of
    Just nid -> pure nid
    Nothing  -> do
      nid <- freshId "const"
      let line = T.pack $
            nid
            ++ " [label=\"" ++ lbl ++ "\","
            ++ " shape=box,"
            ++ " style=rounded];"
      modify $ \s -> s
        { nodes  = nodes s ++ [line]
        , cacheC = M.insert lbl nid (cacheC s)
        }
      pure nid

-- | Aresta com tail/head ports
emitEdge :: String -> String -> [(String,String)] -> GenM ()
emitEdge f t attrs = do
  let attrsTxt = intercalate ", " [k ++ "=" ++ v | (k,v) <- attrs]
      line = T.pack $ f ++ " -> " ++ t ++ " [" ++ attrsTxt ++ "];"
  modify $ \s -> s { edges = edges s ++ [line] }

-- | Bind e lookup de variáveis
bindVar :: Ident -> String -> GenM ()
bindVar x nid = modify $ \s ->
  s { envVars = M.insert x nid (envVars s) }

lookupVar :: Ident -> GenM String
lookupVar x = do
  m <- gets envVars
  case M.lookup x m of
    Just nid -> pure nid
    Nothing  -> error $ "variável não ligada: " ++ show x

-- | Definições de função para inline/recursão
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s ->
  s { defs = M.insert f (ps,e) (defs s) }

lookupDef :: Ident -> GenM (Maybe ([Ident],Expr))
lookupDef f = gets (M.lookup f . defs)

-- | Gera todo o .dot (sem nó de retorno)
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial       = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState (mapM_ recordDecl decls >> mapM_ genDecl decls) initial
  in T.unlines (dotHeader ++ ns ++ es ++ ["}"])

-- | Registra todas as funções
recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b
recordDecl _                = pure ()

-- | Gera só o corpo de funções sem parâmetros
genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] b) = void (genExpr [] b)
genDecl _                = pure ()

-- | Geração recursiva de expressões
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  -- variável: uso direto do id ligado
  Var x -> lookupVar x

  -- literais
  Lit lit -> case lit of
    LInt i   -> emitConst ("const#" ++ show i)
    LBool b  -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c  -> emitConst ("const#" ++ show (ord c))
    LFloat f -> emitConst ("fconst#" ++ show f)
    LString s ->
      genExpr stack (List (map (Lit . LChar) s))

  -- aplicação / inline
  App f x -> do
    -- flatten do App em hd + args
    let flatten :: Expr -> (Expr, [Expr])
        flatten (App f' x') =
          let (g, xs) = flatten f'
          in (g, xs ++ [x'])
        flatten e = (e, [])
        (hd, args) = flatten (App f x)
    case hd of
      Var fid -> do
        md <- lookupDef fid
        case md of
          Just (ps, body)
            | fid `notElem` stack -> do
                regs   <- mapM (genExpr stack) args
                oldEnv <- gets envVars
                zipWithM_ bindVar ps regs
                res <- genExpr (fid:stack) body
                modify $ \s -> s { envVars = oldEnv }
                pure res
          _ -> fallbackCall fid args stack
      _ -> fallbackCall "anon" (hd:args) stack

  -- binop com símbolo real
  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let (nm,sym) = case op of
          Add -> ("add","+"); Sub -> ("sub","-")
          Mul -> ("mul","*"); Div -> ("div","/")
          Mod -> ("mod","%"); Eq  -> ("eq","==")
          Neq -> ("neq","!="); Lt  -> ("lt","<")
          Le  -> ("le","<="); Gt  -> ("gt",">")
          Ge  -> ("ge",">="); And -> ("and","&&")
          Or  -> ("or","||")
    n <- emitNode nm sym
    emitEdge ra n [("tailport","s"),("headport","nw")]
    emitEdge rb n [("tailport","s"),("headport","ne")]
    pure n

  -- if → steer
  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n  <- emitSteer
    emitEdge rc n [("tailport","s"),("headport","n")]
    emitEdge rt n [("tailport","se"),("headport","ne")]
    emitEdge re n [("tailport","sw"),("headport","nw")]
    pure n

  -- let
  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) -> do
      r <- genExpr stack ex
      bindVar x r
    genExpr stack body

  -- tupla
  Tuple es -> do
    rs  <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- lista literal → nó único S1
  List es -> do
    rs  <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- lambda raro no topo
  Lambda{} -> emitNode "lambda" "lambda"

  -- case com tuple-pattern ou outros
  Case scrut alts -> do
    sid   <- genExpr stack scrut
    altRs <- forM alts $ \(pat, expr) -> case pat of
      PTuple pats | any isPVar pats ->
        let xs = [x | PVar x <- pats]
        in mapM_ (`bindVar` sid) xs >> pure sid
      PVar x ->
        bindVar x sid >> pure sid
      _ ->
        genExpr stack expr
    n <- emitSteer
    emitEdge sid n [("tailport","s"),("headport","n")]
    case altRs of
      [r]      -> emitEdge r n [("tailport","s"),("headport","ne")]
      [rT,rF]  -> do
        emitEdge rT n [("tailport","se"),("headport","ne")]
        emitEdge rF n [("tailport","sw"),("headport","nw")]
      rs       -> forM_ rs $ \r -> emitEdge r n [("tailport","s"),("headport","nw")]
    pure n

-- | fallback para chamadas externas
fallbackCall :: Ident -> [Expr] -> [Ident] -> GenM String
fallbackCall fn args stack = do
  regs <- mapM (genExpr stack) args
  cg   <- emitNode "callgroup" ("callgroup(" ++ fn ++ ")")
  zipWithM_ (\i r -> do
      sn <- emitNode "callsnd" ("callsnd(" ++ fn ++ "," ++ show i ++ ")")
      emitEdge r  sn [("tailport","s"),("headport","n")]
      emitEdge sn cg [("tailport","s"),("headport","nw")]
    ) [1..] regs
  retn <- emitNode "retsnd" ("retsnd(" ++ fn ++ ")")
  emitEdge cg retn [("tailport","s"),("headport","n")]
  pure retn

-- | Detecta PVar em Pattern
isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False
