# Module `Semantic`

**Overview.** Semantic checks + HM-style **type inference**, plus *desugaring* and **naming of `super` blocks**. Must succeed before graph building.

**Exports (selected).**
- `desugarDecl :: Decl -> Decl`
- `desugarProgram :: Program -> Program`
- `buildSig :: [Decl] -> Sig`
- `buildFuncEnv :: [Decl] -> FuncEnv`
- `semanticCheck :: Program -> [Error]`         — semantic only
- `checkProgram :: Program -> [Error]`          — semantic + types
- `checkAll :: Program -> [Error]`
- `assignSuperNames :: Program -> Program`
- `inferExpr :: FuncEnv -> TypeEnv -> Expr -> Infer Type`

**Core types (selected).**
- `data SemanticError = ...`
- `data TypeError = ...`
- `data Error = SemErr SemanticError | TypErr TypeError`
- `data Type = TInt | TFloat | TBool | TChar | TString | TList Type | TTuple [Type] | TVar String | TFun [Type] Type`
- `type Sig = Map Ident Int`
- `type Env = Set Ident`
- `type TypeEnv = Map Ident Type`
- `type FuncEnv = Map Ident ([Type], Type)`
- `data InferState = InferState { count :: Int }`
- `type Infer a = ExceptT TypeError (State InferState) a`

**Dependencies.** `Syntax` (+ `Data.Map`, `Data.Set`, `Control.Monad.*`).

**Function reference (selection).**
- **`checkAll :: Program -> [Error]`** — *Program → errors* (empty if OK).  
- **`assignSuperNames :: Program -> Program`** — canonical names (`s1`, `s2`, …) for `super` blocks.  
- **`desugarProgram / desugarDecl`** — normalize top-level forms.  
- **`buildSig / buildFuncEnv`** — construct arity/type environments.  
- **`inferExpr`** — HM-style inference (application, `Case`, `Let`, ops, literals).
