{-# LANGUAGE LambdaCase, FlexibleContexts #-}

-- | Abstract Syntax Tree (AST) definitions for the simple functional language.
module Syntax where

-- | Identifier type, representing variable and function names.
type Ident = String

-- | A program is simply a list of declarations (functions).
data Program = Program [Decl]
  deriving (Show)

-- | A top-level declaration consists of a function name, its parameters, and its body expression.
data Decl = FunDecl Ident [Ident] Expr
  deriving (Show)

-- | Expressions in the language.
data Expr
  = Var Ident                -- ^ Variable reference
  | Lit Literal              -- ^ Literal constant
  | Lambda [Ident] Expr      -- ^ Lambda abstraction with parameters and body
  | If Expr Expr Expr        -- ^ Conditional: if <cond> then <then> else <else>
  | Case Expr [(Pattern, Expr)] -- ^ Pattern matching: case <expr> of <alternatives>
  | Let [Decl] Expr          -- ^ Local declarations and body expression
  | App Expr Expr            -- ^ Function application
  | BinOp BinOperator Expr Expr -- ^ Binary operator application
  | UnOp UnOperator Expr     -- ^ Unary operator application
  | List [Expr]              -- ^ List literal
  | Tuple [Expr]             -- ^ Tuple literal
  deriving (Show)

-- | Patterns used in case alternatives.
data Pattern
  = PWildcard                -- ^ Matches anything ("_" pattern)
  | PVar Ident               -- ^ Variable pattern, binds the identifier
  | PLit Literal             -- ^ Literal pattern
  | PList [Pattern]          -- ^ List pattern
  | PTuple [Pattern]         -- ^ Tuple pattern
  deriving (Show)

-- | Literal values supported by the language.
data Literal
  = LInt Int                 -- ^ Integer literal
  | LFloat Double            -- ^ Floating-point literal
  | LChar Char               -- ^ Character literal
  | LString String           -- ^ String literal
  | LBool Bool               -- ^ Boolean literal
  deriving (Show)

-- | Binary operators.
data BinOperator
  = Add  -- ^ Addition (+)
  | Sub  -- ^ Subtraction (-)
  | Mul  -- ^ Multiplication (*)
  | Div  -- ^ Division (/)
  | Mod  -- ^ Modulus (mod)
  | Eq   -- ^ Equality (==)
  | Neq  -- ^ Inequality (/=)
  | Lt   -- ^ Less than (<)
  | Le   -- ^ Less than or equal (<=)
  | Gt   -- ^ Greater than (>)
  | Ge   -- ^ Greater than or equal (>=)
  | And  -- ^ Logical and (&&)
  | Or   -- ^ Logical or (||)
  deriving (Show)

-- | Unary operators.
data UnOperator
  = Neg  -- ^ Numeric negation (\n -> -n)
  | Not  -- ^ Logical negation (not)
  deriving (Show)
