{-# LANGUAGE LambdaCase #-}

module Codegen where

import Syntax
  ( Program(..)
  , Decl(..)
  , Expr(..)
  , Literal(..)
  , Ident
  , BinOperator(..)
  , UnOperator(..)
  , Pattern(..)
  )
import Semantic (desugarProgram)
import Data.List (intercalate)

data Ctx = Ctx { varEnv :: [(Ident, String)], counter :: Int }

emptyCtx :: Ctx
emptyCtx = Ctx [] 0

bindVar :: Ident -> String -> Ctx -> Ctx
bindVar v op (Ctx env n) = Ctx ((v, op) : env) n

freshVar :: Ctx -> (String, Ctx)
freshVar (Ctx env n) = ("t" ++ show n, Ctx env (n + 1))

lookupVar :: Ctx -> Ident -> String
lookupVar (Ctx env _) v = case lookup v env of
  Just x  -> x
  Nothing -> error $ "Unbound variable: " ++ v

programToTALM :: Program -> String
programToTALM prog =
  let Program decls = desugarProgram prog
      allNames = map (\(FunDecl name _ _) -> name) decls
      ctxWithNames = foldl (\c name -> bindVar name name c) emptyCtx allNames
      (_, blocks) = foldl genDecl (ctxWithNames, []) decls
  in unlines ("// TALM Assembly Generated" : blocks)
  

genDecl :: (Ctx, [String]) -> Decl -> (Ctx, [String])
genDecl (ctx, acc) (FunDecl name args body) =
  let ctxArgs = foldl (\c a -> bindVar a a c) ctx args
      (code, res, ctx') = genExpr ctxArgs body
      label = name ++ ":"
      retInst = "ret " ++ name ++ ", " ++ res ++ ", []"
      block = unlines [ "// Function " ++ name, label, code, retInst ]
   in (ctx { counter = counter ctx' }, acc ++ [block])


genExpr :: Ctx -> Expr -> (String, String, Ctx)
genExpr ctx = \case
  Var x -> ("", lookupVar ctx x, ctx)
  Lit l -> genLiteral ctx l
  Lambda ps e -> genExpr (foldl (\c p -> bindVar p p c) ctx ps) e
  App f x ->
    let (f', args) = flattenApp (App f x)
        (argsCode, argNames, ctx1) = genArgs ctx args
    in case f' of
      Var fname ->
        let (dst, ctx2) = freshVar ctx1
            code = argsCode ++ "callsnd " ++ dst ++ ", " ++ fname
                 ++ ", " ++ show (length argNames)
                 ++ concatMap (", " ++) argNames ++ "\n"
        in (code, dst, ctx2)
      Lambda ps body ->
        -- aplica diretamente: cria bindings e avalia o corpo
        let ctx' = foldl (\c (p, arg) -> bindVar p arg c) ctx1 (zip ps argNames)
        in genExpr ctx' body
      _ -> error "Função aplicada não é identificador nem lambda"

  BinOp op l r ->
    let (c1, a1, ctx1) = genExpr ctx l
        (c2, a2, ctx2) = genExpr ctx1 r
        (dst, ctx3) = freshVar ctx2
    in (c1 ++ c2 ++ binOpInstr op dst a1 a2, dst, ctx3)
  UnOp op e ->
    let (c, a, ctx1) = genExpr ctx e
        (dst, ctx2) = freshVar ctx1
        instr = case op of
          Not -> "not " ++ dst ++ ", " ++ a ++ "\n"
          Neg -> "subi " ++ dst ++ ", 0, " ++ a ++ "\n"
    in (c ++ instr, dst, ctx2)
  If c t e ->
    let (cc, rc, ctx1) = genExpr ctx c
        (ct, rt, ctx2) = genExpr ctx1 t
        (ce, re, ctx3) = genExpr ctx2 e
        (dst, ctx4) = freshVar ctx3
        code = cc ++ ct ++ ce ++ "steer " ++ dst ++ ", " ++ rc ++ ", " ++ rt ++ ", " ++ re ++ "\n"
    in (code, dst, ctx4)
  Let decls body ->
    let (ctx1, blocks) = foldl
          (\(c0, acc) (FunDecl n ps b) ->
              let (c1, r1, c2) = genExpr (foldl (\c p -> bindVar p p c) c0 ps) b
                  c3 = bindVar n r1 c2
              in (c3, acc ++ ["// let " ++ n ++ "\n" ++ c1]))
          (ctx, []) decls
        (cb, rb, ctx2) = genExpr ctx1 body
    in (unlines blocks ++ cb, rb, ctx2)
  List es -> genListOrTuple ctx es
  Tuple es -> genListOrTuple ctx es
  Case e alts ->
    let (ce, re, ctx1) = genExpr ctx e
        (branches, ctx2) = foldl
          (\(bs, c) (pat, expr) ->
             case pat of
               PLit lit ->
                 let (clit, vlit, c1) = genLiteral c lit
                     (eq, c2) = freshVar c1
                     eqCode = clit ++ "equal " ++ eq ++ ", " ++ re ++ ", " ++ vlit ++ "\n"
                     (bc, br, c3) = genExpr c2 expr
                 in (bs ++ [(eq, eqCode ++ bc, br)], c3)
               PWildcard ->
                 let (bc, br, c1) = genExpr c expr
                 in (bs ++ [("1", bc, br)], c1)
               PVar v ->
                 let c1 = bindVar v re c
                     (bc, br, c2) = genExpr c1 expr
                 in (bs ++ [("1", bc, br)], c2)
               PList ps ->
                let (vars, c1) = foldl (\(vs, c0) _ -> let (v, c') = freshVar c0 in (vs ++ [v], c')) ([], c) ps
                    (lenReg, c2) = freshVar c1
                    (lenCode1, lenVal, c3) = mkConst "const" (show (length ps)) c2
                    lenCode = lenCode1 ++ "length " ++ lenReg ++ ", " ++ re ++ "\n" ++
                              "equal " ++ lenReg ++ ", " ++ lenReg ++ ", " ++ lenVal ++ "\n"
                    decomp = "splitn " ++ unwords vars ++ ", " ++ re ++ "\n"
                    (checks, binds, c4) = foldl (\(eqs, bnds, c0) (p, v) -> case p of
                                              PLit l ->
                                                let (cl, vl, c1') = genLiteral c0 l
                                                    (g, c2') = freshVar c1'
                                                    eq = cl ++ "equal " ++ g ++ ", " ++ v ++ ", " ++ vl ++ "\n"
                                                in (eqs ++ [(g, eq)], bnds, c2')
                                              PVar idn -> (eqs, bnds ++ [(idn, v)], c0)
                                              PWildcard -> (eqs, bnds, c0)
                                              _ -> error "Unsupported element in list pattern")
                                            ([], [], c3) (zip ps vars)
                    (guard, eqCode, c5) = case map fst checks of
                      []     -> ("1", "", c4)
                      [g1]   -> (g1, snd (head checks), c4)
                      gs     -> let (gfinal, c6) = freshVar c4
                                    eqs = concatMap snd checks
                                    ands = "and " ++ gfinal ++ ", " ++ intercalate ", " gs ++ "\n"
                                in (gfinal, eqs ++ ands, c6)
                    ctx' = foldl (\c0 (idn, val) -> bindVar idn val c0) c5 binds
                    (bc, br, c6) = genExpr ctx' expr
                    full = lenCode ++ decomp ++ eqCode ++ bc
                in (bs ++ [(guard, full, br)], c6)

               PTuple ps ->
                 let (vars, c1) = foldl (\(vs, c0) _ -> let (v, c') = freshVar c0 in (vs ++ [v], c')) ([], c) ps
                     splitCode = "splitn " ++ unwords vars ++ ", " ++ re ++ "\n"
                     (checks, binds, c2) = foldl
                       (\(eqs, bnds, c0) (p, v) -> case p of
                          PLit l ->
                            let (cl, vl, c1') = genLiteral c0 l
                                (g, c2') = freshVar c1'
                                eq = cl ++ "equal " ++ g ++ ", " ++ v ++ ", " ++ vl ++ "\n"
                            in (eqs ++ [(g, eq)], bnds, c2')
                          PVar idn ->
                            (eqs, bnds ++ [(idn, v)], c0)
                          PWildcard ->
                            (eqs, bnds, c0)
                          _ -> error "Unsupported pattern in tuple")
                       ([], [], c1) (zip ps vars)
                     (guard, eqCode, c3) = case map fst checks of
                       []     -> ("1", "", c2)
                       [g1]   -> (g1, snd (head checks), c2)
                       gs     -> let (gfinal, c4) = freshVar c2
                                     eqs = concatMap snd checks
                                     ands = "and " ++ gfinal ++ ", " ++ intercalate ", " gs ++ "\n"
                                 in (gfinal, eqs ++ ands, c4)
                     ctx' = foldl (\c0 (idn, val) -> bindVar idn val c0) c3 binds
                     (bc, br, c4) = genExpr ctx' expr
                     full = splitCode ++ eqCode ++ bc
                 in (bs ++ [(guard, full, br)], c4)
               _ -> error "Unsupported pattern in case expression")
          ([], ctx1) alts

        buildSteer [(cond, code, result)] dst = (code, result)
        buildSteer ((c1, b1, r1):(c2, b2, r2):rest) dst =
          let (g, ctx') = freshVar ctx2
              steer = "steer " ++ g ++ ", " ++ c1 ++ ", " ++ r1 ++ ", " ++ r2 ++ "\n"
              (codeRest, final) = buildSteer ((g, b1 ++ b2 ++ steer, g):rest) dst
          in (codeRest, final)
        buildSteer _ _ = error "Empty case branches"

        (codeBranches, finalRes) = buildSteer branches "UNUSED_DST"
    in (ce ++ codeBranches, finalRes, ctx2)



isPLit :: Pattern -> Bool
isPLit (PLit _) = True
isPLit _        = False

genLiteral :: Ctx -> Literal -> (String, String, Ctx)
genLiteral ctx lit = case lit of
  LInt n   -> mkConst "const" (show n) ctx
  LFloat f -> mkConst "fconst" (show f) ctx
  LBool b  -> mkConst "const" (if b then "1" else "0") ctx
  LChar c  -> mkConst "const" (show (fromEnum c)) ctx
  LString s ->
    let (codes, names, ctx1) = foldl
          (\(cs, ns, c0) ch -> let (c, n, c1) = mkConst "const" (show (fromEnum ch)) c0
                               in (cs ++ c, ns ++ [n], c1))
          ("", [], ctx) s
        (dst, ctx2) = freshVar ctx1
        code = codes ++ "super " ++ dst ++ ", 1, " ++ show (length names)
             ++ concatMap (", " ++) names ++ "\n"
    in (code, dst, ctx2)

mkConst :: String -> String -> Ctx -> (String, String, Ctx)
mkConst opc val c0 = let (d, c1) = freshVar c0 in (opc ++ " " ++ d ++ ", " ++ val ++ "\n", d, c1)

flattenApp :: Expr -> (Expr, [Expr])
flattenApp (App f x) = let (fn, args) = flattenApp f in (fn, args ++ [x])
flattenApp e = (e, [])

genArgs :: Ctx -> [Expr] -> (String, [String], Ctx)
genArgs ctx args = foldl
  (\(code, names, c0) e -> let (c, n, c1) = genExpr c0 e in (code ++ c, names ++ [n], c1))
  ("", [], ctx) args

genListOrTuple :: Ctx -> [Expr] -> (String, String, Ctx)
genListOrTuple ctx es =
  let (code, names, ctx1) = genArgs ctx es
      (dst, ctx2) = freshVar ctx1
      instr = "super " ++ dst ++ ", 1, " ++ show (length names) ++ concatMap (", " ++) names ++ "\n"
  in (code ++ instr, dst, ctx2)

binOpInstr :: BinOperator -> String -> String -> String -> String
binOpInstr op dst l r = case op of
  Add  -> "add "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Sub  -> "sub "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Mul  -> "mul "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Div  -> "div "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Mod  -> "mod "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Eq   -> "equal "    ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Neq  -> "not_equal "++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Lt   -> "lthan "    ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Le   -> "lthani "   ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Gt   -> "gthan "    ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Ge   -> "gthani "   ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  And  -> "and "      ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
  Or   -> "or "       ++ dst ++ ", " ++ l ++ ", " ++ r ++ "\n"
