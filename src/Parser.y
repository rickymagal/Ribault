{  
module Parser where

import Syntax
import Lexer (Token(..), PosnToken, AlexPosn)
}

%nonassoc   "->"
%left "||"
%left "&&"
%nonassoc "==" "/="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%right "not" "-"
%right ";"
%left APP   


%name parse
%tokentype { (AlexPosn, Token) }
%error { parseError }

%token
  ";"           { (_, TokenSemi) }
  "let"         { (_, TokenLet) }
  "in"          { (_, TokenIn) }
  "if"          { (_, TokenIf) }
  "then"        { (_, TokenThen) }
  "else"        { (_, TokenElse) }
  "case"        { (_, TokenCase) }
  "of"          { (_, TokenOf) }
  "not"         { (_, TokenNot) }
  "->"          { (_, TokenArrow) }
  "\\"          { (_, TokenBackslash) }
  "="           { (_, TokenEquals) }
  "_"           { (_, TokenUnderscore) }
  "("           { (_, TokenLParen) }
  ")"           { (_, TokenRParen) }
  "["           { (_, TokenLBracket) }
  "]"           { (_, TokenRBracket) }
  ","           { (_, TokenComma) }
  "+"           { (_, TokenPlus) }
  "-"           { (_, TokenMinus) }
  "*"           { (_, TokenTimes) }
  "/"           { (_, TokenDiv) }
  "%"           { (_, TokenMod) }
  "=="          { (_, TokenEq) }
  "/="          { (_, TokenNeq) }
  "<"           { (_, TokenLt) }
  "<="          { (_, TokenLe) }
  ">"           { (_, TokenGt) }
  ">="          { (_, TokenGe) }
  "&&"          { (_, TokenAnd) }
  "||"          { (_, TokenOr) }
  int           { (_, TokenInt $$) }
  float         { (_, TokenFloat $$) }
  char          { (_, TokenChar $$) }
  string        { (_, TokenString $$) }
  "True"        { (_, TokenBool True) }
  "False"       { (_, TokenBool False) }
  ident         { (_, TokenIdent $$) }

%%

Program :: { Program }
    : DeclList                      { Program (reverse $1) }

-- DeclList: declarações separadas por ';' (com trailing opcional)
DeclList :: { [Decl] }
    : Decl                           { [$1] }
    | Decl ";" DeclList             { $1 : $3 }

Decl :: { Decl }
    : ident Params "=" Expr         { FunDecl $1 (reverse $2) $4 }


Params :: { [Ident] }
    :                               { [] }
    | ident Params                  { $1 : $2 }

Expr :: { Expr }
    : Lambda                        { $1 }
    | IfExpr                        { $1 }
    | CaseExpr                      { $1 }
    | LetExpr                       { $1 }
    | ExprOr                        { $1 }

Lambda :: { Expr }
    : "\\" LamParams "->" Expr     { Lambda (reverse $2) $4 }

-- parâmetros de lambda: ou vários ident ou um (x1,x2,…)
LamParams :: { [Ident] }
    : Params                       { $1 }
    | "(" IdentList ")"           { $2 }

IdentList :: { [Ident] }
    : ident                        { [$1] }
    | ident "," IdentList         { $1 : $3 }

IfExpr :: { Expr }
    : "if" Expr "then" Expr "else" Expr
                                    { If $2 $4 $6 }

CaseExpr :: { Expr }
    -- case com branches separados por ';'
    : "case" Expr "of" Alts         { Case $2 $4 }

Alts :: { [(Pattern,Expr)] }
    : AltList                       { $1 }

AltList :: { [(Pattern,Expr)] }
    : Alt                           { [$1] }
    | AltList Alt                  { $1 ++ [ $2 ] }

Alt :: { (Pattern,Expr) }
    : Pattern "->" Expr ";"        { ($1,$3) }


LetExpr :: { Expr }
    : "let" DeclList "in" Expr      { Let (reverse $2) $4 }

-- expressões binárias, sem rec. à esquerda

ExprOr :: { Expr }
    : ExprAnd OrRest                { foldl (BinOp Or) $1 $2 }
OrRest :: { [Expr] }
    :                               { [] }
    | "||" ExprAnd OrRest           { $2 : $3 }

ExprAnd :: { Expr }
    : ExprEq AndRest                { foldl (BinOp And) $1 $2 }
AndRest :: { [Expr] }
    :                               { [] }
    | "&&" ExprEq AndRest           { $2 : $3 }

ExprEq :: { Expr }
    : ExprRel EqRest                { foldl (\e1 (op,e2) -> BinOp op e1 e2) $1 $2 }
EqRest :: { [(BinOperator,Expr)] }
    :                               { [] }
    | EqOp ExprRel EqRest           { ($1,$2) : $3 }
EqOp :: { BinOperator }
    : "=="                          { Eq }
    | "/="                          { Neq }

ExprRel :: { Expr }
    : ExprAdd RelRest               { foldl (\e1 (op,e2) -> BinOp op e1 e2) $1 $2 }
RelRest :: { [(BinOperator,Expr)] }
    :                               { [] }
    | RelOp ExprAdd RelRest         { ($1,$2) : $3 }
RelOp :: { BinOperator }
    : "<"                           { Lt }
    | "<="                          { Le }
    | ">"                           { Gt }
    | ">="                          { Ge }

ExprAdd :: { Expr }
    : ExprMul AddRest               { foldl (\e1 (op,e2) -> BinOp op e1 e2) $1 $2 }
AddRest :: { [(BinOperator,Expr)] }
    :                               { [] }
    | AddOp ExprMul AddRest         { ($1,$2) : $3 }
AddOp :: { BinOperator }
    : "+"                           { Add }
    | "-"                           { Sub }

ExprMul :: { Expr }
    : ExprUnary MulRest             { foldl (\e1 (op,e2) -> BinOp op e1 e2) $1 $2 }
MulRest :: { [(BinOperator,Expr)] }
    :                               { [] }
    | MulOp ExprUnary MulRest       { ($1,$2) : $3 }
MulOp :: { BinOperator }
    : "*"                           { Mul }
    | "/"                           { Div }
    | "%"                           { Mod }

ExprUnary :: { Expr }
    : "not" ExprUnary               { UnOp Not $2 }
    | "-" ExprUnary                 { UnOp Neg $2 }
    | ExprApp                       { $1 }

-- aplicação n-ária

ExprApp :: { Expr }
    : Atom AppTail  %prec APP       { foldl App $1 $2 }
AppTail :: { [Expr] }
    :                               { [] }
    | Atom AppTail                  { $1 : $2 }

Atom :: { Expr }
    : Literal                       { Lit $1 }
    | ident                         { Var $1 }
    | "(" Expr ")"                  { $2 }
    | List                          { $1 }
    | Tuple                         { $1 }

Literal :: { Literal }
    : int                           { LInt $1 }
    | float                         { LFloat $1 }
    | char                          { LChar $1 }
    | string                        { LString $1 }
    | "True"                        { LBool True }
    | "False"                       { LBool False }

List :: { Expr }
    : "[" ExprListOpt "]"           { List $2 }
ExprListOpt :: { [Expr] }
    :                               { [] }
    | ExprList                      { $1 }
ExprList :: { [Expr] }
    : Expr ExprListTail             { $1 : $2 }
ExprListTail :: { [Expr] }
    :                               { [] }
    | "," Expr ExprListTail         { $2 : $3 }

Tuple :: { Expr }
    : "(" Expr "," Expr TupleTail ")"  { Tuple ($2 : $4 : $5) }

TupleTail :: { [Expr] }
    :                               { [] }
    | "," Expr TupleTail           { $2 : $3 }

-- padrões

Pattern :: { Pattern }
    : "_"                           { PWildcard }
    | ident                         { PVar $1 }
    | Literal                       { PLit $1 }
    | ListPattern                   { $1 }
    | TuplePattern                  { $1 }

ListPattern :: { Pattern }
    : "[" PatternListOpt "]"        { PList $2 }
PatternListOpt :: { [Pattern] }
    :                               { [] }
    | PatternList                   { $1 }
PatternList :: { [Pattern] }
    : Pattern PatternListTail       { $1 : $2 }
PatternListTail :: { [Pattern] }
    :                               { [] }
    | "," Pattern PatternListTail   { $2 : $3 }

TuplePattern :: { Pattern }
    : "(" Pattern PatternTupleTail ")" { PTuple ($2 : $3) }
PatternTupleTail :: { [Pattern] }
    :                               { [] }
    | "," Pattern PatternTupleTail  { $2 : $3 }



{
parseError :: [PosnToken] -> a
parseError ((pos, tok):_) = error ("Erro de parse: token inesperado " ++ show tok ++ " em " ++ show pos)
parseError []             = error "Erro de parse: entrada inesperadamente vazia"
}
