{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Gera grafo Dataflow (.dot) no estilo Trebuchet/TALM,
--   com constantes cacheadas, unária, lista única,
--   tuple- & list-pattern no case, inline de Lambdas e FunDecls não-recursivos,
--   e fallback dinâmico para funções recursivas ou higher-order
--   usando callsnd / callgroup / retsnd.
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
  , nodes    :: [Text]
  , edges    :: [Text]
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

-- | Cabeçalho fixo do .dot
dotHeader :: [Text]
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

-- | Emite aresta com atributos
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

-- | Registra FunDecls
bindDef :: Ident -> [Ident] -> Expr -> GenM ()
bindDef f ps e = modify $ \s ->
  s { defs = M.insert f (ps,e) (defs s) }

lookupDef :: Ident -> GenM (Maybe ([Ident], Expr))
lookupDef f = gets (M.lookup f . defs)

-- | Chamada dinâmica: callsnd / callgroup / retsnd
fallbackCall :: String -> [Expr] -> [Ident] -> GenM String
fallbackCall fn args stack = do
  argRegs <- mapM (genExpr stack) args
  cg      <- emitNode "callgroup" ("callgroup(" ++ fn ++ ")")
  forM_ (zip [1..] argRegs) $ \(i, r) -> do
    sndN <- emitNode "callsnd" ("callsnd(" ++ fn ++ "," ++ show i ++ ")")
    emitEdge r    sndN [("tailport","s"),("headport","n")]
    emitEdge sndN cg   [("tailport","s"),("headport","nw")]
  retn <- emitNode "retsnd" ("retsnd(" ++ fn ++ ")")
  emitEdge cg retn [("tailport","s"),("headport","n")]
  pure retn

-- | Gera o .dot completo com formatação padronizada
programToDataflowDot :: Program -> Text
programToDataflowDot prog =
  let Program decls = desugarProgram prog
      initial       = CGCtx 0 [] [] M.empty M.empty M.empty
      CGCtx _ ns es _ _ _ =
        execState (mapM_ recordDecl decls >> mapM_ genDecl decls) initial

      -- indentação de duas espaços
      indent t = "  " <> t

      headerLines = dotHeader
      nodeLines   = map indent ns
      edgeLines   = map indent es

  in T.unlines $
       headerLines
    ++ nodeLines
    ++ [ "" ]     -- linha em branco
    ++ edgeLines
    ++ [ "}" ]

recordDecl :: Decl -> GenM ()
recordDecl (FunDecl n ps b) = bindDef n ps b

genDecl :: Decl -> GenM ()
genDecl (FunDecl _ [] b) = void (genExpr [] b)
genDecl _                = pure ()

isPVar :: Pattern -> Bool
isPVar (PVar _) = True
isPVar _        = False

genExpr :: [Ident] -> Expr -> GenM String
genExpr stack expr =  case expr of

  -- Variável ou função
  Var x ->
    lookupVar x

  -- Literais
  Lit lit -> case lit of
    LInt i    -> emitConst ("const#" ++ show i)
    LBool b   -> emitConst ("const#" ++ show (if b then 1 else 0))
    LChar c   -> emitConst ("const#" ++ show (ord c))
    LFloat f  -> emitConst ("fconst#" ++ show f)
    LString s -> genExpr stack (List (map (Lit . LChar) s))

  -- UnOp (Neg e Not)
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
    -- “Desenrola” Apps curried em (hd, args)
    let flatten (App f' a) =
          let (g,xs) = flatten f' in (g, xs ++ [a])
        flatten e = (e, [])
        (hd, args) = flatten (App f x)

    case hd of
      -- 1) FunDecl não-recursivo (applyTwice ou inc) totalmente aplicado → inline
      Var fid -> do
        mdef <- lookupDef fid
        case mdef of
          Just (ps, body)
            | length ps == length args
           && fid `notElem` stack -> do
              -- gera regs dos args
              regs   <- mapM (genExpr stack) args
              -- vincula formal→reg
              oldEnv <- gets envVars
              zipWithM_ bindVar ps regs
              -- inline do corpo
              res    <- genExpr (fid:stack) body
              -- restaura ambiente
              modify $ \s -> s { envVars = oldEnv }
              pure res

          -- 2) Caso de parâmetro-função (f em applyTwice) ou recursão → dynamic call
          _ -> fallbackCall fid args stack

      -- 3) Lambda saturada → inline
      Lambda ps body
        | length ps == length args -> do
            regs   <- mapM (genExpr stack) args
            oldEnv <- gets envVars
            zipWithM_ bindVar ps regs
            res    <- genExpr ("<lam>":stack) body
            modify $ \s -> s { envVars = oldEnv }
            pure res

      -- 4) Qualquer outro → dynamic call
      _ -> do
        funReg <- genExpr stack hd
        fallbackCall funReg args stack

  -- BinOp com símbolo real
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

  -- Let-binding de FunDecl aridade-0 inline
  Let decls body -> do
    forM_ decls $ \(FunDecl x [] ex) ->
      bindVar x =<< genExpr stack ex
    genExpr stack body

  -- Tupla literal
  Tuple es -> do
    rs <- mapM (genExpr stack) es
    let lbl = "S" ++ show (length rs)
    sup <- emitNode "super" lbl
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lista literal
  List es -> do
    rs  <- mapM (genExpr stack) es
    sup <- emitNode "super" "S1"
    forM_ rs $ \r -> emitEdge r sup [("tailport","s"),("headport","nw")]
    pure sup

  -- Lambda solto
  Lambda{} ->
    emitNode "lambda" "lambda"

  Case scrut alts -> do
    -- 1) avalia a expressão que será casada
    scrutReg <- genExpr stack scrut

    -- 2) serializa cada Pattern em um reg:
    --    • PVar/PWildcard → emitConst
    --    • PLit → emitConst do valor
    --    • PList → makeListPat dos sub-padrões
    --    • PTuple → makeTuplePat dos sub-padrões
    patRegs <- forM (map fst alts) $ \pat -> case pat of

      -- padrões atômicos
      PVar _        -> emitNode "pat" "PVar"
      PWildcard     -> emitNode "pat" "PWildcard"

      -- literais
      PLit (LInt i) -> emitConst ("PConstInt" ++ show i)
      PLit (LBool b)
        -> emitConst (if b then "PConstBoolTrue" else "PConstBoolFalse")

      -- padrão de lista [p1,p2,…]
      PList ps -> do
        subRegs <- forM ps $ \p -> case p of
          PVar _        -> emitNode "pat" "PVar"
          PWildcard     -> emitNode "pat" "PWildcard"
          PLit (LInt i) -> emitConst ("PConstInt" ++ show i)
          PLit (LBool b)
            -> emitConst (if b then "PConstBoolTrue" else "PConstBoolFalse")
          _             -> error "sub-pattern de lista não suportado"
        listPat <- emitNode "makeListPat"  "makeListPat"
        forM_ subRegs $ \r ->
          emitEdge r listPat [("tailport","s"),("headport","nw")]
        pure listPat

      -- padrão de tupla (p1,p2,…)
      PTuple ps -> do
        subRegs <- forM ps $ \p -> case p of
          PVar _        -> emitNode "pat" "PVar"
          PWildcard     -> emitNode "pat" "PWildcard"
          PLit (LInt i) -> emitConst ("PConstInt" ++ show i)
          PLit (LBool b)
            -> emitConst (if b then "PConstBoolTrue" else "PConstBoolFalse")
          _             -> error "sub-pattern de tupla não suportado"
        tupPat <- emitNode "makeTuplePat"  "makeTuplePat"
        forM_ subRegs $ \r ->
          emitEdge r tupPat [("tailport","s"),("headport","nw")]
        pure tupPat

      -- qualquer outro
      _ -> error "Pattern não suportado pelo matchSeq"

    -- 3) agrupa TODOS os patRegs num único makeListAll
    patternsList <- emitNode "makeListAll" "makeListAll"
    forM_ patRegs $ \pr ->
      emitEdge pr patternsList [("tailport","s"),("headport","nw")]

    -- 4) chama matchSeq via callsnd/callgroup/retsnd
    cgM   <- emitNode "callgroup" "callgroup(matchSeq)"
    sndP  <- emitNode "callsnd"   "callsnd(matchSeq,1)"
    emitEdge patternsList sndP [("tailport","s"),("headport","n")]
    emitEdge sndP         cgM    [("tailport","s"),("headport","nw")]

    sndS  <- emitNode "callsnd"   "callsnd(matchSeq,2)"
    emitEdge scrutReg      sndS    [("tailport","s"),("headport","n")]
    emitEdge sndS          cgM     [("tailport","s"),("headport","nw")]

    retsM <- emitNode "retsnd"   "retsnd(matchSeq)"
    emitEdge cgM            retsM [("tailport","s"),("headport","n")]

    let matchReg = retsM

    -- 5) steer do resultado do match
    st <- emitSteer
    emitEdge matchReg st [("tailport","s"),("headport","n")]

    -- 6) projeta e bindVar das variáveis do primeiro padrão
    let (firstPat, thenExpr) = head alts
        bindProj (PVar v, i) = do
          prj <- emitNode ("proj" ++ show i) ("proj" ++ show i)
          let port = if i == 0 then "nw" else "ne"
          emitEdge scrutReg prj [("tailport","s"),("headport",port)]
          bindVar v prj
        bindProj _ = pure ()
    case firstPat of
      PList ps  -> mapM_ bindProj (zip ps [0..])
      PTuple ps -> mapM_ bindProj (zip ps [0..])
      _         -> pure ()

    -- 7) then-branch: corpo da primeira alternativa
    thenReg <- genExpr stack thenExpr
    emitEdge thenReg st [("tailport","sw"),("headport","nw")]

    -- 8) else-branch: recursão sobre demais alts ou default
    let restAlts = tail alts
    elseReg <- if null restAlts
      then genExpr stack scrut
      else genExpr stack (Case scrut restAlts)
    emitEdge elseReg st [("tailport","se"),("headport","ne")]

    pure st
