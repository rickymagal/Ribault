{-# LANGUAGE LambdaCase #-}

-- | Codegen.hs – Gera TALM assembly a partir do AST desugarizado.
module Codegen where

import Semantic (desugarProgram)
import Syntax
  ( Program(..)
  , Decl(..)
  , Expr(..)
  , Literal(..)
  , Ident
  , BinOperator(..)
  , UnOperator(..)
  )
import Data.List (intercalate)

-- | Contexto para mapear nomes de variáveis e gerar nomes temporários
data Ctx = Ctx
  { varEnv  :: [(Ident,String)]
  , counter :: Int
  }

emptyCtx :: Ctx
emptyCtx = Ctx [] 0

bindVar :: Ident -> String -> Ctx -> Ctx
bindVar v op (Ctx env n) = Ctx ((v,op):env) n

freshVar :: Ctx -> (String,Ctx)
freshVar (Ctx env n) = ("t" ++ show n, Ctx env (n+1))

lookupVar :: Ctx -> Ident -> String
lookupVar (Ctx env _) v = case lookup v env of
  Just x  -> x
  Nothing -> error $ "Unbound variable: " ++ v

-- | Gera TALM a partir do AST desugarizado
programToTALM :: Program -> String
programToTALM prog =
  let Program decls = desugarProgram prog
      initCtx      = foldr (\(FunDecl n _ _) c -> bindVar n n c) emptyCtx decls
      (_, blocks)  = foldl genDecl (initCtx,[]) decls
  in unlines ("// TALM Assembly Generated" : blocks)

-- | Gera código para cada declaração top-level
genDecl :: (Ctx,[String]) -> Decl -> (Ctx,[String])
genDecl (ctx,acc) = \case
  FunDecl name args body ->
    let ctxArgs         = foldr (\a c -> bindVar a a c) ctx args
        (code,out,ctx') = genExpr ctxArgs body
        header = if null args
                 then "// Top-level binding " ++ name
                 else "// Function " ++ name ++ " params: " ++ show args
        footer = if null args
                 then "// Result in: " ++ out
                 else "ret " ++ name ++ ", " ++ out ++ ", []"
        block  = unlines [header, code, footer]
        ctx''  = bindVar name out ctx'
    in (ctx'', acc ++ [block])
  _ -> (ctx,acc)

-- | Gera código para expressões: (código, dest, novoCtx)
genExpr :: Ctx -> Expr -> (String,String,Ctx)
genExpr ctx = \case
  Var v ->
    ("", lookupVar ctx v, ctx)

  Lit l ->
    genLiteral l ctx

  Lambda ps body ->
    genExpr (foldr (\a c -> bindVar a a c) ctx ps) body

  App f x ->
    let (cf,rf,c1) = genExpr ctx f
        (cx,rx,c2) = genExpr c1 x
        (dst,c3)   = freshVar c2
        instr      = "// call " ++ rf ++ " " ++ rx ++ "\n"
    in (cf ++ cx ++ instr, dst, c3)

  BinOp op l r ->
    let (c1,r1,cx) = genExpr ctx l
        (c2,r2,cy) = genExpr cx r
        (dst,cz)   = freshVar cy
        instr      = binOpInstr op dst r1 r2
    in (c1 ++ c2 ++ instr, dst, cz)

  UnOp op e ->
    let (c,r,c1) = genExpr ctx e
        (dst,c2)  = freshVar c1
        instr     = case op of
          Not -> "not  " ++ dst ++ ", " ++ r ++ "\n"
          Neg -> "subi " ++ dst ++ ", 0, " ++ r ++ "\n"
    in (c ++ instr, dst, c2)

  If cond th el ->
    let (cc,rc,c1) = genExpr ctx cond
        (ct,rt,c2) = genExpr c1 th
        (ce,re,c3) = genExpr c2 el
        (dst,c4)   = freshVar c3
        instr      = "steer " ++ dst ++ ", " ++ rc ++ ", " ++ rt ++ ", " ++ re ++ "\n"
    in (cc ++ ct ++ ce ++ instr, dst, c4)

  List xs ->
    let
      folder (cs, ns, c0) e =
        let (c,n,c1) = genExpr c0 e
        in (cs ++ c, ns ++ [n], c1)
      (cs, ns, c1) = foldl folder ("", [], ctx) xs
      (dst, c2)    = freshVar c1
      -- opcode 1 = list, seguido do número de fontes e depois as fontes
      instr        = "super " ++ dst ++ ", 1, " ++ show (length ns)
                    ++ concatMap (", " ++) ns ++ "\n"
    in (cs ++ instr, dst, c2)

  Tuple es ->
    genExpr ctx (List es)

  Let decls body ->
    let
      (ctx1, binds) = foldl
        (\(c0, bs) d -> case d of
            FunDecl v ps bd ->
              let (cb, rv, c2) = genExpr c0 (Lambda ps bd)
              in (bindVar v rv c2, bs ++ ["// let " ++ v, cb])
            _ -> (c0, bs)
        ) (ctx, []) decls
      (ce, er, c3) = genExpr ctx1 body
    in (unlines binds ++ ce, er, c3)

  Case{} ->
    ("// Case not implemented\n", "", ctx)

-- | Gera instruções para literal
genLiteral :: Literal -> Ctx -> (String,String,Ctx)
genLiteral lit ctx = case lit of
  LInt n   -> mk "const" (show n) ctx
  LFloat f -> mk "fconst" (show f) ctx
  LBool b  -> mk "const" (if b then "1" else "0") ctx
  LChar c  -> mk "const" (show (fromEnum c)) ctx
  LString s ->
    let
      folder (cs, ns, c0) ch =
        let (l, n, c1) = mk "const" (show (fromEnum ch)) c0
        in (cs ++ l, ns ++ [n], c1)
      (cs, ns, c1) = foldl folder ("", [], ctx) s
      (dst, c2)   = freshVar c1
      instr       = "super " ++ dst ++ ", 1, " ++ show (length ns)
                   ++ concatMap (", " ++) ns ++ "\n"
    in (cs ++ instr, dst, c2)
 where
  mk opc val c0 =
    let (d, c1) = freshVar c0
    in (opc ++ " " ++ d ++ ", " ++ val ++ "\n", d, c1)

-- | Modelos para instruções binárias
binOpInstr :: BinOperator -> String -> String -> String -> String
binOpInstr op dst l r = case op of
  Add  -> "add "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Sub  -> "sub "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Mul  -> "mul "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Div  -> "div "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Mod  -> "mod "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Eq   -> "equal "     ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Neq  -> "not_equal " ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Lt   -> "lthan "     ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Le   -> "lthani "    ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Gt   -> "gthan "     ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Ge   -> "gthani "    ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  And  -> "and "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Or   -> "or "        ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
