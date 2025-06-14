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
import           Data.Maybe            (mapMaybe, catMaybes)
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
genExpr :: [Ident] -> Expr -> GenM String
genExpr stack = \case

  -- Variável ou função
  Var x ->
    lookupVar x

  -- Literais
  Lit lit -> case lit of
    LInt i     -> emitConst ("const#" ++ show i)
    LBool b    -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c    -> emitConst ("const#" ++ show (ord c))
    LFloat f   -> emitConst ("fconst#" ++ show f)
    LString s  -> genExpr stack (List (map (Lit . LChar) s))

  -- Unários
  UnOp op e -> do
    r <- genExpr stack e
    let mkUn sym = do
          n <- emitNode sym sym
          emitEdge r n [("tailport","s"),("headport","nw")]
          pure n
    case op of
      Neg -> mkUn "-"
      Not -> mkUn "not"

  -- Aplicação / inline / dynamic call
  App f x -> do
    let flatten (App f' a) = let (g,xs) = flatten f' in (g, xs ++ [a])
        flatten e          = (e, [])
        (hd, args)         = flatten (App f x)

        dynCall funReg argRegs = do
          cg <- emitNode "callgroup" ("callgroup(" ++ funReg ++ ")")
          zipWithM_ (\i a -> do
              sn <- emitNode "callsnd" ("callsnd(" ++ funReg ++ "," ++ show i ++ ")")
              emitEdge a    sn [("tailport","s"),("headport","n")]
              emitEdge sn cg [("tailport","s"),("headport","nw")]
            ) [1..] argRegs
          rt <- emitNode "retsnd" ("retsnd(" ++ funReg ++ ")")
          emitEdge cg rt [("tailport","s"),("headport","n")]
          pure rt

    case hd of
      -- FunDecl top-level saturada
      Var fid -> do
        mdef <- lookupDef fid
        case mdef of
          Just (ps, body)
            | length ps == length args
           && fid `notElem` stack -> do
              regs   <- mapM (genExpr stack) args
              oldEnv <- gets envVars
              zipWithM_ bindVar ps regs
              res    <- genExpr (fid:stack) body
              modify $ \s -> s { envVars = oldEnv }
              pure res
          _ -> do
            funReg  <- lookupVar fid
            argRegs <- mapM (genExpr stack) args
            dynCall funReg argRegs

      -- Lambda saturada
      Lambda ps body
        | length ps == length args -> do
            regs   <- mapM (genExpr stack) args
            oldEnv <- gets envVars
            zipWithM_ bindVar ps regs
            res    <- genExpr ("<lam>":stack) body
            modify $ \s -> s { envVars = oldEnv }
            pure res

      -- fallback dinâmico
      _ -> do
        funReg  <- genExpr stack hd
        argRegs <- mapM (genExpr stack) args
        dynCall funReg argRegs

  -- BinOp
  BinOp op a b -> do
    ra <- genExpr stack a
    rb <- genExpr stack b
    let (nm,sym) = case op of
          Add -> ("add","+"); Sub -> ("sub","-")
          Mul -> ("mul","*"); Div -> ("div","/")
          Mod -> ("mod","%"); Eq -> ("eq","==")
          Neq -> ("neq","!="); Lt -> ("lt","<")
          Le  -> ("le","<="); Gt -> ("gt",">")
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

  -- Let-binding aridade-0
  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) ->
      bindVar x =<< genExpr stack ex
    genExpr stack body

  -- Tuple literal
  Tuple es -> do
    rs <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lista literal
  List es -> do
    rs <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lambda isolada
  Lambda{} ->
    emitNode "lambda" "lambda"

  -- Case geral → usa switchList para as alternaivas PList
  Case scrut alts -> do
    sid <- genExpr stack scrut

    -- para cada alternativa de lista, emite um switchList
    let makeMatch (pat, expr) = case pat of

          PList ps -> do
            sw <- emitNode "switchList" "switchList"
            emitEdge sid sw [("tailport","s"),("headport","n")]
            -- bind das variáveis de ps ao mesmo registrador do switch
            forM_ ps $ \p -> case p of
              PVar v -> bindVar v sw
              _      -> pure ()
            -- corpo da alternativa
            br <- genExpr stack expr
            pure (Just (sw, br))

          -- para tuplas e var-patterns, mantemos como antes
          PTuple ps | any isPVar ps -> do
            n <- emitNode ("matchtuple" ++ show (length ps))
                          ("matchtuple" ++ show (length ps))
            emitEdge sid n [("tailport","s"),("headport","n")]
            forM_ (zip ps [0..]) $ \(p,i) -> do
              proj <- emitNode ("tproj"++show i) ("tproj"++show i)
              let port = ["nw","ne","sw","se"] !! (i `mod` 4)
              emitEdge n proj [("tailport","s"),("headport",port)]
              case p of PVar v -> bindVar v proj; _ -> pure ()
            br <- genExpr stack expr
            pure (Just (n, br))

          PVar v -> do
            bindVar v sid
            br <- genExpr stack expr
            pure (Just (sid, br))

          _ ->
            pure Nothing

    pairs <- fmap catMaybes (mapM makeMatch alts)

    -- encadeia steers normalmente
    let buildChain []           = error "Case sem alternativa válida"
        buildChain [(g,b)]      = do
          s <- emitSteer
          emitEdge g s [("tailport","s"),("headport","n")]
          emitEdge b s [("tailport","se"),("headport","ne")]
          pure s
        buildChain ((g,b):rest) = do
          nxt <- buildChain rest
          s   <- emitSteer
          emitEdge g s    [("tailport","s"),("headport","n")]
          emitEdge b s    [("tailport","se"),("headport","ne")]
          emitEdge nxt s  [("tailport","s"),("headport","sw")]
          pure s

    buildChain pairs

-- | Auxiliar para Pattern
isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False
