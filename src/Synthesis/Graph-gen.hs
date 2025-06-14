{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Gera grafo Dataflow (.dot) no estilo Trebuchet/TALM,
--   com inline de funções nomeadas e lambdas saturadas, unária,
--   constantes cacheadas, lista única, tuple- e list-pattern,
--   e sem nós extras de retorno ou var.
module GraphGen (programToDataflowDot) where

import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy   as T
import           Data.Char             (ord)
import           Control.Monad.State
import           Control.Monad         (forM, forM_, zipWithM, void)
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

-- | Gera um ID único
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

-- | Emite um nó arredondado
emitNode :: String -> String -> GenM String
emitNode base lbl = do
  nid  <- freshId base
  let line = T.pack $ nid ++ " [label=\"" ++ lbl ++ "\", shape=box, style=rounded];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Emite um nó triangular de steer
emitSteer :: GenM String
emitSteer = do
  nid  <- freshId "steer"
  let line = T.pack $ nid ++ " [label=\"T   F\", shape=triangle, style=solid];"
  modify $ \s -> s { nodes = nodes s ++ [line] }
  pure nid

-- | Emite constante cacheada (int, float, char/bool via ord)
emitConst :: String -> GenM String
emitConst lbl = do
  cmap <- gets cacheC
  case M.lookup lbl cmap of
    Just nid -> pure nid
    Nothing  -> do
      nid  <- freshId "const"
      let line = T.pack $ nid ++ " [label=\"" ++ lbl ++ "\", shape=box, style=rounded];"
      modify $ \s -> s
        { nodes  = nodes s ++ [line]
        , cacheC = M.insert lbl nid (cacheC s)
        }
      pure nid

-- | Emite uma aresta com atributos tail/head ports
emitEdge :: String -> String -> [(String,String)] -> GenM ()
emitEdge f t attrs = do
  let a    = intercalate ", " [k ++ "=" ++ v | (k,v) <- attrs]
      line = T.pack $ f ++ " -> " ++ t ++ " [" ++ a ++ "];"
  modify $ \s -> s { edges = edges s ++ [line] }

-- | Liga variável a nó no ambiente
bindVar :: Ident -> String -> GenM ()
bindVar x r = modify $ \s -> s { envVars = M.insert x r (envVars s) }

lookupVar :: Ident -> GenM String
lookupVar x = do
  m <- gets envVars
  case M.lookup x m of
    Just r  -> pure r
    Nothing -> error $ "variável não ligada: " ++ show x

-- | Registra defs de função para inline/recursão
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s -> s { defs = M.insert f (ps,e) (defs s) }

lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = gets (M.lookup f . defs)

-- | Ponto de entrada: gera o .dot completo
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial       = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState (mapM_ recordDecl decls >> mapM_ genDecl decls) initial
  in T.unlines (dotHeader ++ ns ++ es ++ ["}"])

-- | Grava todas as funções para uso em inline
recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b
recordDecl _                = pure ()

-- | Gera apenas o corpo de funções sem parâmetros (main, etc.)
genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] b) = void (genExpr [] b)
genDecl _                = pure ()

-- | Geração recursiva de expressões, com inline "inteligente" de FunDecl e Lambda
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  Var x ->
    lookupVar x

  Lit lit -> case lit of
    LInt i     -> emitConst ("const#" ++ show i)
    LBool b    -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c    -> emitConst ("const#" ++ show (ord c))
    LFloat f   -> emitConst ("fconst#" ++ show f)
    LString s  -> genExpr stack (List (map (Lit . LChar) s))

  -- Negação unária
  UnOp Neg e -> do
    r <- genExpr stack e
    n <- emitNode "neg" "-"
    emitEdge r n [("tailport","s"),("headport","nw")]
    pure n

  -- Aplicação: inline de FunDecl saturadas e de Lambdas, fallbackCall caso geral
  App f x -> do
    let flatten (App f' x') = let (g,xs) = flatten f' in (g, xs ++ [x'])
        flatten e           = (e, [])
        (hd, args)         = flatten (App f x)

    case hd of
      -- 1) Função nomeada totalmente aplicada → inline
      Var fid -> do
        md <- lookupDef fid
        case md of
          Just (ps, body)
            | length ps == length args
           && fid `notElem` stack -> do
              -- gera regs para cada arg; se Var de FunDecl, inline desse FunDecl
              regs <- zipWithM (\param argExp ->
                        case argExp of
                          Var afid -> do
                            ma <- lookupDef afid
                            case ma of
                              Just (aps, ab) -> genExpr stack (Lambda aps ab)
                              Nothing        -> genExpr stack argExp
                          _ -> genExpr stack argExp
                      ) ps args
              oldEnv <- gets envVars
              zipWithM_ bindVar ps regs
              res <- genExpr (fid:stack) body
              modify $ \s -> s { envVars = oldEnv }
              pure res
          _ ->
            fallbackCall fid args stack

      -- 2) Lambda literal totalmente aplicada → inline
      Lambda ps body
        | length ps == length args -> do
            regs   <- mapM (genExpr stack) args
            oldEnv <- gets envVars
            zipWithM_ bindVar ps regs
            res    <- genExpr ("<lam>":stack) body
            modify $ \s -> s { envVars = oldEnv }
            pure res

      -- 3) Caso geral → callgroup/callsnd/retsnd
      _ ->
        fallbackCall "anon" (hd:args) stack

  -- Operador binário com símbolo real
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

  -- If → steer
  If c t e -> do
    rc <- genExpr stack c
    rt <- genExpr stack t
    re <- genExpr stack e
    n  <- emitSteer
    emitEdge rc n [("tailport","s"),("headport","n")]
    emitEdge rt n [("tailport","se"),("headport","ne")]
    emitEdge re n [("tailport","sw"),("headport","nw")]
    pure n

  -- Let inline de FunDecl de aridade zero
  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) -> bindVar x =<< genExpr stack ex
    genExpr stack body

  -- Tupla literal
  Tuple es -> do
    rs <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs (\r -> emitEdge r sup [("tailport","s"),("headport","nw")])
    pure sup

  -- Lista literal → nó único S1
  List es -> do
    rs <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs (\r -> emitEdge r sup [("tailport","s"),("headport","nw")])
    pure sup

  -- Lambda não aplicada → nó genérico
  Lambda{} ->
    emitNode "lambda" "lambda"

  -- Case com tuple- e list-patterns
  Case scrut alts -> do
    sid   <- genExpr stack scrut
    altRs <- forM alts $ \(pat, expr) -> case pat of

      -- lista enumerada
      PList pats -> case pats of
        []                  -> emitConst "const#0"
        [PVar x]            -> bindVar x sid >> pure sid
        [PVar x,PWildcard]  -> bindVar x sid >> pure sid
        _                   -> genExpr stack expr

      -- tuple-pattern
      PTuple ps | any isPVar ps ->
        let xs = [x | PVar x <- ps] in mapM_ (`bindVar` sid) xs >> pure sid

      -- var-pattern simples
      PVar x ->
        bindVar x sid >> pure sid

      -- outros
      _ ->
        genExpr stack expr

    n <- emitSteer
    emitEdge sid n [("tailport","s"),("headport","n")]
    case altRs of
      [r]      -> emitEdge r n [("tailport","s"),("headport","ne")]
      [rT,rF]  -> do
        emitEdge rT n [("tailport","se"),("headport","ne")]
        emitEdge rF n [("tailport","sw"),("headport","nw")]
      rs       ->
        zipWithM_ (\r p -> emitEdge r n [("tailport","s"),("headport",p)])
                  rs (cycle ["ne","nw","se","sw"])
    pure n

-- | Fallback para chamadas externas de FunDecl ou lambdas não saturadas
fallbackCall :: Ident -> [Expr] -> [Ident] -> GenM String
fallbackCall fn args _ = do
  regs <- mapM (genExpr []) args
  cg   <- emitNode "callgroup" ("callgroup(" ++ fn ++ ")")
  zipWithM_ (\i r -> do
      sndN <- emitNode "callsnd" ("callsnd(" ++ fn ++ "," ++ show i ++ ")")
      emitEdge r    sndN [("tailport","s"),("headport","n")]
      emitEdge sndN cg   [("tailport","s"),("headport","nw")]
    ) [1..] regs
  retN <- emitNode "retsnd" ("retsnd(" ++ fn ++ ")")
  emitEdge cg retN [("tailport","s"),("headport","n")]
  pure retN

-- | Auxiliar para detectar PVar em Pattern
isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False
