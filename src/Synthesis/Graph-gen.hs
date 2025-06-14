{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Gera grafo Dataflow (.dot) no estilo Trebuchet/TALM
--   - constantes int: const#<n>
--   - constantes float: fconst#<f>
--   - char e bool → int (usando ord True=1,False=0; Char via ord)
--   - string → lista de Char
--   - lista literal → um único nó "S1"
--   - evita nós duplicados de constantes
--   - sem nó de retorno
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy as T
import           Data.Char             (ord, toUpper)
import           Syntax                ( Program(..), Decl(..), Expr(..)
                                      , Literal(..), Ident, BinOperator(..)
                                      , Pattern(..) )
import           Semantic              (desugarProgram)
import           Control.Monad.State
import           Control.Monad         (forM, forM_, void, zipWithM_)
import           Data.List             (intercalate)
import qualified Data.Map.Strict as M

-- | Estado do gerador
data CGCtx = CGCtx
  { counter  :: Int
  , nodes    :: [Text]
  , edges    :: [Text]
  , envVars  :: M.Map Ident String           -- ^ variáveis ligadas
  , defs     :: M.Map Ident ([Ident], Expr)  -- ^ defs de função
  , constMap :: M.Map String String          -- ^ labelConst → idConst
  }

type GenM = State CGCtx

-- | freshId genérico
freshId :: String -> GenM String
freshId base = do
  s <- get
  let n = counter s
      idx = show n
  put s { counter = n + 1 }
  pure (base ++ idx)

-- | header do .dot
dotHeader :: [Text]
dotHeader =
  [ "digraph G {"
  , "  node [shape=box, style=rounded];"
  , "  node [shape=triangle, style=solid];"
  ]

-- | emite um nó qualquer de caixa arredondada
emitNode :: String -> String -> GenM String
emitNode base lbl = do
  nid <- freshId base
  let line = T.pack $ nid
            ++ " [label=\"" ++ lbl ++ "\","
            ++ " shape=box,"
            ++ " style=rounded];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | nó steer
emitSteer :: GenM String
emitSteer = do
  nid <- freshId "steer"
  let line = T.pack $ nid
            ++ " [label=\"T   F\","
            ++ " shape=triangle,"
            ++ " style=solid];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | emite constante (int ou float), cacheada
emitConst :: String   -- ^ label sem aspas: e.g. "const#42" ou "fconst#3.14"
          -> GenM String
emitConst lbl = do
  mp <- gets constMap
  case M.lookup lbl mp of
    Just nid -> pure nid
    Nothing  -> do
      nid <- freshId "const"
      let line = T.pack $ nid
                ++ " [label=\"" ++ lbl ++ "\","
                ++ " shape=box,"
                ++ " style=rounded];"
      modify $ \s -> s
        { nodes    = nodes s ++ [line]
        , constMap = M.insert lbl nid (constMap s)
        }
      pure nid

-- | emite aresta com tail/head ports
emitEdge :: String -> String -> [(String,String)] -> GenM ()
emitEdge from to attrs = do
  let attrsTxt = intercalate ", " [k ++ "=" ++ v | (k,v) <- attrs]
      line = T.pack $ from ++ " -> " ++ to ++ " [" ++ attrsTxt ++ "];"
  modify $ \s -> s { edges = edges s ++ [line] }

-- | vincula var → id
bindVar :: Ident -> String -> GenM ()
bindVar x nid = modify $ \s -> s { envVars = M.insert x nid (envVars s) }

lookupVar :: Ident -> GenM String
lookupVar x = do
  mp <- gets envVars
  case M.lookup x mp of
    Just nid -> pure nid
    Nothing  -> error $ "variável não ligada: " ++ show x

-- | defs de função para inline/recursão
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps b = modify $ \s ->
  s { defs = M.insert f (ps,b) (defs s) }

lookupDef :: Ident -> GenM (Maybe ([Ident],Expr))
lookupDef f = gets (M.lookup f . defs)

-- | gera o .dot completo
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState
          (mapM_ recordDecl decls >> mapM_ genDecl decls)
          initial
  in T.unlines (dotHeader ++ ns ++ es ++ ["}"])

-- | registra todas as funções
recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b
recordDecl _                = pure ()

-- | gera só o corpo (sem nó de retorno)
genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] body) = void (genExpr [] body)
genDecl _                   = pure ()

-- | gera cada expressão
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  Var x ->
    lookupVar x

  Lit lit -> case lit of
    LInt i ->
      emitConst ("const#" ++ show i)

    LBool b ->
      emitConst ("const#" ++ show (if b then 1 else 0))

    LChar c ->
      emitConst ("const#" ++ show (ord c))

    LFloat f ->
      emitConst ("fconst#" ++ show f)

    LString s ->
      -- string é lista de char
      genExpr stack (List (map (Lit . LChar) s))

  App f x -> do
    let flatten (App f' x') = let (g, xs) = flatten f' in (g, xs++[x'])
        flatten e           = (e, [])
        (hd, args) = flatten (App f x)
    case hd of
      Var fid -> do
        m <- lookupDef fid
        case m of
          Just (ps,b)
            | fid `notElem` stack -> do
                regs   <- mapM (genExpr stack) args
                oldEnv <- gets envVars
                zipWithM_ bindVar ps regs
                res <- genExpr (fid:stack) b
                modify $ \s -> s { envVars = oldEnv }
                pure res
          _ -> fallbackCall fid args stack
      _ ->
        fallbackCall "anon" (hd:args) stack

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

  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n  <- emitSteer
    emitEdge rc n [("tailport","s"), ("headport","n")]
    emitEdge rt n [("tailport","se"),("headport","ne")]
    emitEdge re n [("tailport","sw"),("headport","nw")]
    pure n

  Let decls bd -> do
    forM_ decls $ \(FunDecl x [] ex) -> do
      r <- genExpr stack ex
      bindVar x r
    genExpr stack bd

  Tuple es -> do
    rs <- mapM (genExpr stack) es
    -- super de tupla com label S<n>
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  List es -> do
    rs <- mapM (genExpr stack) es
    -- sempre um único nó de lista: S1
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  Lambda{} ->
    emitNode "lambda" "lambda"

  Case e alts -> do
    scr   <- genExpr stack e
    altRs <- forM alts $ \(pat,ex) -> case pat of
      PVar x -> bindVar x scr >> genExpr stack ex
      _      -> genExpr stack ex
    n <- emitSteer
    emitEdge scr n [("tailport","s"),("headport","n")]
    case altRs of
      [rT,rF] -> do
        emitEdge rT n [("tailport","se"),("headport","ne")]
        emitEdge rF n [("tailport","sw"),("headport","nw")]
      _ -> forM_ altRs $ \r -> emitEdge r n [("tailport","s"),("headport","nw")]
    pure n

-- | fallback para chamadas externas
fallbackCall :: Ident -> [Expr] -> [Ident] -> GenM String
fallbackCall fn args stack = do
  regs <- mapM (genExpr stack) args
  cg   <- emitNode "callgroup" ("callgroup(" ++ fn ++ ")")
  zipWithM_ (\i r -> do
      sndN <- emitNode "callsnd" ("callsnd(" ++ fn ++ "," ++ show i ++ ")")
      emitEdge r    sndN [("tailport","s"),("headport","n")]
      emitEdge sndN cg   [("tailport","s"),("headport","nw")]
    ) [1..] regs
  retN <- emitNode "retsnd" ("retsnd(" ++ fn ++ ")")
  emitEdge cg retN [("tailport","s"),("headport","n")]
  pure retN
