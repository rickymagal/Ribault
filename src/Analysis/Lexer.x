{
module Lexer where
import Data.Maybe (fromMaybe)
}

%wrapper "posn"

$white   = [\ \t\r\n]
$digit   = 0-9
$alpha   = [A-Za-z]
$idchar  = [_A-Za-z0-9']

tokens :-

$white+                  ;
"--"[^\n]*               ;

"let"                    { \p _ -> (p, TokenLet) }
"in"                     { \p _ -> (p, TokenIn) }
"if"                     { \p _ -> (p, TokenIf) }
"then"                   { \p _ -> (p, TokenThen) }
"else"                   { \p _ -> (p, TokenElse) }
"case"                   { \p _ -> (p, TokenCase) }
"of"                     { \p _ -> (p, TokenOf) }
"not"                    { \p _ -> (p, TokenNot) }

"True"                   { \p _ -> (p, TokenBool True) }
"False"                  { \p _ -> (p, TokenBool False) }

"->"                     { \p _ -> (p, TokenArrow) }
"=="                     { \p _ -> (p, TokenEq) }
"/="                     { \p _ -> (p, TokenNeq) }
"<="                     { \p _ -> (p, TokenLe) }
">="                     { \p _ -> (p, TokenGe) }
"<"                      { \p _ -> (p, TokenLt) }
">"                      { \p _ -> (p, TokenGt) }
"&&"                     { \p _ -> (p, TokenAnd) }
"||"                     { \p _ -> (p, TokenOr) }

"+"                      { \p _ -> (p, TokenPlus) }
"-"                      { \p _ -> (p, TokenMinus) }
"*"                      { \p _ -> (p, TokenTimes) }
"/"                      { \p _ -> (p, TokenDiv) }
"%"                      { \p _ -> (p, TokenMod) }

"="                      { \p _ -> (p, TokenEquals) }
"\"                      { \p _ -> (p, TokenBackslash) }
"_"                      { \p _ -> (p, TokenUnderscore) }

"("                      { \p _ -> (p, TokenLParen) }
")"                      { \p _ -> (p, TokenRParen) }
"["                      { \p _ -> (p, TokenLBracket) }
"]"                      { \p _ -> (p, TokenRBracket) }
","                      { \p _ -> (p, TokenComma) }
";"                      { \p _ ->   (p, TokenSemi) }

$digit+ "." $digit+     { \p s -> (p, TokenFloat (read s)) }
$digit+                 { \p s -> (p, TokenInt (read s)) }

\'[^\\']\'              { \p s -> (p, TokenChar (read s)) }
\"([^\\\"]|\\.)*\"      { \p s -> (p, TokenString (read s)) }

$alpha $idchar*          { \p s -> (p, TokenIdent s) }
"-"                      { \p _ -> (p, TokenMinus) }
.                         { \p s -> error ("Erro léxico: caractere inesperado `" ++ s ++ "` em " ++ show p) }

{

data Token
  = TokenLet | TokenIn | TokenIf | TokenThen | TokenElse
  | TokenCase | TokenOf
  | TokenNot
  | TokenBool Bool

  -- Operadores
  | TokenArrow
  | TokenEq | TokenNeq | TokenLe | TokenGe | TokenLt | TokenGt
  | TokenAnd | TokenOr
  | TokenPlus | TokenMinus | TokenTimes | TokenDiv | TokenMod

  -- Símbolos
  | TokenEquals
  | TokenBackslash
  | TokenUnderscore
  | TokenLParen | TokenRParen
  | TokenLBracket | TokenRBracket
  | TokenComma | TokenSemi

  -- Literais e identificadores
  | TokenInt Int
  | TokenFloat Double
  | TokenChar Char
  | TokenString String
  | TokenIdent String
  deriving (Eq)

instance Show Token where
  show TokenSemi        = ";"
  show TokenLet         = "let"
  show TokenIn          = "in"
  show TokenIf          = "if"
  show TokenThen        = "then"
  show TokenElse        = "else"
  show TokenCase        = "case"
  show TokenOf          = "of"
  show TokenNot         = "not"
  show (TokenBool b)    = show b
  show TokenArrow       = "->"
  show TokenEq          = "=="
  show TokenNeq         = "/="
  show TokenLe          = "<="
  show TokenGe          = ">="
  show TokenLt          = "<"
  show TokenGt          = ">"
  show TokenAnd         = "&&"
  show TokenOr          = "||"
  show TokenPlus        = "+"
  show TokenMinus       = "-"
  show TokenTimes       = "*"
  show TokenDiv         = "/"
  show TokenMod         = "%"
  show TokenEquals      = "="
  show TokenBackslash   = "\\"
  show TokenUnderscore  = "_"
  show TokenLParen      = "("
  show TokenRParen      = ")"
  show TokenLBracket    = "["
  show TokenRBracket    = "]"
  show TokenComma       = ","
  show (TokenInt n)     = show n
  show (TokenFloat f)   = show f
  show (TokenChar c)    = show c
  show (TokenString s)  = show s
  show (TokenIdent s)   = s

type PosnToken = (AlexPosn, Token)
alexScanTokens :: String -> [PosnToken]

}
