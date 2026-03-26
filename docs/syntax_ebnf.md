# Ribault Language Syntax (EBNF)

This document is a formal description of the surface syntax accepted by the
parser in `src/Analysis/Parser.y` and lexer in `src/Analysis/Lexer.x`.

Notation:
- `{ X }` means zero or more repetitions of `X`.
- `[ X ]` means optional `X`.
- `|` means alternatives.
- Terminals are in quotes.

## Lexical

```
letter      = "A".."Z" | "a".."z" ;
digit       = "0".."9" ;
idchar      = letter | digit | "_" | "'" ;
ident       = letter , { idchar } ;

int_lit     = digit , { digit } ;
float_lit   = digit , { digit } , "." , digit , { digit } ;

comment     = "--" , { ? any char except newline ? } ;
whitespace  = " " | "\t" | "\r" | "\n" ;
```

## Program structure

```
program     = decl_list ;

decl_list   = decl , { decl } ;

decl        = ident , params , "=" , expr ;
params      = { ident } ;
```

Note: the real parser uses **layout (indentation)** instead of semicolons.
The EBNF above omits layout rules; a new declaration starts when indentation
returns to the previous level.

## Expressions

```
expr        = lambda
            | if_expr
            | case_expr
            | let_expr
            | super_expr
            | expr_or
            | expr , ":" , expr ;

lambda      = "\" , lam_params , "->" , expr ;
lam_params  = params | "(" , ident_list , ")" ;
ident_list  = ident , { "," , ident } ;

if_expr     = "if" , expr , "then" , expr , "else" , expr ;

case_expr   = "case" , expr , "of" , alts ;
alts        = alt_list ;
alt_list    = alt , { alt } ;
alt         = pattern , "->" , expr ;

let_expr    = "let" , decl_list , "in" , expr ;

expr_or     = expr_and , { "||" , expr_and } ;
expr_and    = expr_eq  , { "&&" , expr_eq  } ;
expr_eq     = expr_rel , { eq_op , expr_rel } ;
eq_op       = "==" | "/=" ;

expr_rel    = expr_add , { rel_op , expr_add } ;
rel_op      = "<" | "<=" | ">" | ">=" ;

expr_add    = expr_mul , { add_op , expr_mul } ;
add_op      = "+" | "-" ;

expr_mul    = expr_unary , { mul_op , expr_unary } ;
mul_op      = "*" | "/" | "%" ;

expr_unary  = "not" , expr_unary
            | "-"   , expr_unary
            | expr_app ;

expr_app    = atom , { atom } ;
```

## Atoms, literals, lists, tuples

```
atom        = literal
            | ident
            | "(" , expr , ")"
            | list
            | tuple ;

literal     = int_lit | float_lit
            | "True" | "False" ;

list        = "[" , [ expr_list ] , "]" ;
expr_list   = expr , { "," , expr } ;

tuple       = "(" , expr , "," , expr , { "," , expr } , ")" ;
```

Note: only **pair tuples** `(a,b)` are represented correctly at runtime.
Larger tuples are parsed but not supported by the compiler backend.

## Super instructions

Super instructions embed opaque Haskell code that is compiled separately
by GHC. The body is captured by balanced parenthesis tracking in the lexer.

```
super_expr  = "super" , ident , super_args , "(" , super_body , ")"
            | "super" , super_kind ,
              "input"  , "(" , ident , ")" ,
              "output" , "(" , ident , ")" ,
              legacy_body ;

super_args  = { ident } ;
super_kind  = "single" | "parallel" ;
super_body  = ? balanced Haskell code (parens tracked by lexer) ? ;
legacy_body = "#BEGINSUPER" , legacy_text , "#ENDSUPER" ;
legacy_text = ? any characters including newlines ? ;
```

The first form is the **primary syntax**:

```haskell
myFunc input =
  super implName input (
    implName x = someHaskellExpression x
  )
```

Here `implName` is the function name defined inside the body, and the
remaining identifiers are the input arguments. The super instruction
reads its inputs from the dataflow graph and writes the function's
return value back.

**Important**: the body is delimited by balanced parentheses. Top-level
`(...)` inside the body will close it prematurely. Use `$` or `let`
bindings instead of parentheses:

```haskell
-- WRONG: the (do closes at the matching )
  super f x (
    f x = unsafePerformIO (do ...)    -- broken!
  )

-- RIGHT: use $ to avoid top-level parens
  super f x (
    f x = unsafePerformIO $ do
        ...
  )
```

The second form is the **legacy syntax** (backward compatible):

```haskell
myFunc input =
  super single input (input) output (out)
#BEGINSUPER
    out = someExpression
#ENDSUPER
```

## Built-in functions

### Print family

The following built-in print functions are available without requiring
a super instruction. Each prints the value and returns it (for chaining):

```
print x      — print integer (Int64)
prints xs    — print list of ASCII codes as string
printl xs    — print list of integers
printf x     — print float
printlf xs   — print list of floats
printmf xs   — print matrix (list of list of floats)
```

Examples:

```haskell
main = print (fib 10)                     -- 55
main = prints [72,101,108,108,111]         -- Hello
main = printl (mergeSort [3,1,2])          -- [1,2,3]
main = printf (1.5 * 2.0)                 -- 3.0
main = printlf [1.5, 2.25, 3.75]          -- [1.5,2.25,3.75]
main = printmf (mmult a b)                -- [[6.375,9.75],[13.125,20.4375]]
```

### List operations

Lists are built-in via super instructions s0-s3:

```
:    (cons)     — construct a pair/cons cell
head xs         — first element (via pattern matching)
tail xs         — rest of list (via pattern matching)
[]              — empty list (nil)
```

List operations are typically accessed through pattern matching syntax
rather than direct function calls:

```haskell
case xs of
  []     -> ...           -- nil check
  (x:xt) -> ...           -- head/tail destructuring
```

## Patterns

```
pattern     = "_"
            | ident
            | literal
            | pattern , ":" , pattern
            | list_pattern
            | tuple_pattern ;

list_pattern = "[" , [ pattern_list ] , "]" ;
pattern_list = pattern , { "," , pattern } ;

tuple_pattern = "(" , pattern , "," , pattern , { "," , pattern } , ")" ;
```

## Precedence and associativity

From lowest to highest:
- `||`
- `&&`
- `==`, `/=`
- `<`, `<=`, `>`, `>=`
- `+`, `-`
- `*`, `/`, `%`
- unary `not`, unary `-`
- application (left-associative)
- `:` (right-associative, cons)

This matches the precedence declarations in `Parser.y`.

## Reserved words

```
case    else    if      in      let     of      then
not     super   single  parallel  input   output
True    False
print   prints  printl  printf  printlf printmf
```

## File extension

Ribault source files use the `.hss` extension.
