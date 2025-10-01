# Module `Syntax`

**Overview.** Defines the **AST** for the accepted Haskell subset: identifiers, programs, declarations, expressions, patterns, literals, and operators.

**Exports (main).**
- `type Ident = String`
- `data Program = Program [Decl]`
- `data Decl = FunDecl Ident [Ident] Expr`
- `data Expr = Var Ident | Lit Literal | Lambda [Ident] Expr | If Expr Expr Expr | Case Expr [(Pattern, Expr)] | Let [Decl] Expr | App Expr Expr | BinOp BinOperator Expr Expr | UnOp UnOperator Expr | List [Expr] | Tuple [Expr]`
- `data Pattern = ...` (wildcards, variables, literals, list/tuple patterns)
- `data Literal = LInt Int | LFloat Double | LChar Char | LString String | LBool Bool`
- `data BinOperator = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | And | Or`
- `data UnOperator = Neg | Not`

**Dependencies.** none (base AST types used across the project).
