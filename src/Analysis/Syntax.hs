{-# LANGUAGE LambdaCase, FlexibleContexts #-}

-- | Abstract Syntax Tree (AST) definitions for the simple functional language.
module Syntax where

-- | Identifier type, representing variable and function names.
type Ident = String

-- | A program is simply a list of declarations (functions).
data Program = Program [Decl]
  deriving (Show)

-- | A top-level declaration.
data Decl
  = FunDecl Ident [Ident] Expr
  deriving (Show)

-- | EXTRA: espécie de super‐bloco.
data SuperKind = SuperSingle | SuperParallel
  deriving (Show)

-- | Expressions in the language.
data Expr
  = Var Ident
  | Lit Literal
  | Lambda [Ident] Expr
  | If Expr Expr Expr
  | Case Expr [(Pattern, Expr)]
  | Let [Decl] Expr
  | App Expr Expr
  | BinOp BinOperator Expr Expr
  | UnOp  UnOperator  Expr
  | List  [Expr]
  | Tuple [Expr]
  | Cons  Expr Expr
  -- --------------- NOVO ----------------
  | Super SuperKind Ident Ident String   -- ^ super <kind> input(x) output(y) BODY
  ----------------------------------------
  deriving (Show)

-- | Patterns used in case alternatives.
data Pattern
  = PWildcard
  | PVar Ident
  | PLit Literal
  | PList  [Pattern]
  | PTuple [Pattern]
  | PCons  Pattern Pattern
  deriving (Show)

-- | Literal values supported by the language.
data Literal
  = LInt Int | LFloat Double | LChar Char | LString String | LBool Bool
  deriving (Show)

-- | Binary operators.
data BinOperator
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  deriving (Show)

-- | Unary operators.
data UnOperator = Neg | Not
  deriving (Show)
