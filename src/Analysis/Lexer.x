{
module Analysis.Lexer
  ( Token(..)
  , scanAll
  , alexMonadScan
  ) where

import Control.Monad (when)
}

%wrapper "monadUserState"

$white   = [\ \t\r]
$digit   = [0-9]
$alpha   = [A-Za-z]
$idchar  = [_A-Za-z0-9']

tokens :-

-- Normal mode: skip whitespace, newlines, comments
<0> $white+                         { skip }
<0> \n                              { skip }
<0> "--".*                          { skip }

-- Enter/leave super mode
<0>     "#BEGINSUPER"               { actBeginSuper }
<super> "#ENDSUPER"                 { actEndSuper }
<super> \n                          { actSuperAcc }
<super> .                           { actSuperAcc }

-- Keywords (normal mode only)
<0> "let"                           { actEmit TokenLet }
<0> "in"                            { actEmit TokenIn }
<0> "if"                            { actEmit TokenIf }
<0> "then"                          { actEmit TokenThen }
<0> "else"                          { actEmit TokenElse }
<0> "case"                          { actEmit TokenCase }
<0> "of"                            { actEmit TokenOf }
<0> "not"                           { actEmit TokenNot }

<0> "True"                          { actEmit (TokenBool True) }
<0> "False"                         { actEmit (TokenBool False) }

-- Operators (normal mode only)
<0> "->"                            { actEmit TokenArrow }
<0> "=="                            { actEmit TokenEq }
<0> "/="                            { actEmit TokenNeq }
<0> "<="                            { actEmit TokenLe }
<0> ">="                            { actEmit TokenGe }
<0> "<"                             { actEmit TokenLt }
<0> ">"                             { actEmit TokenGt }
<0> "&&"                            { actEmit TokenAnd }
<0> "||"                            { actEmit TokenOr }

<0> "\+"                            { actEmit TokenPlus }
<0> "-"                             { actEmit TokenMinus }
<0> "*"                             { actEmit TokenTimes }
<0> "/"                             { actEmit TokenDiv }
<0> "%"                             { actEmit TokenMod }

<0> "="                             { actEmit TokenEquals }
<0> [\\]                            { actEmit TokenBackslash }
<0> "_"                             { actEmit TokenUnderscore }

-- Delimiters (normal mode only)
<0> "("                             { actEmit TokenLParen }
<0> ")"                             { actEmit TokenRParen }
<0> "["                             { actEmit TokenLBracket }
<0> "]"                             { actEmit TokenRBracket }
<0> ","                             { actEmit TokenComma }
<0> ":"                             { actEmit TokenColon }
<0> ";"                             { actEmit TokenSemi }

-- Super headers (normal mode only)
<0> "super"                         { actEmit TokenSuper }
<0> "single"                        { actEmit TokenSingle }
<0> "parallel"                      { actEmit TokenParallel }
<0> "input"                         { actEmit TokenInput }
<0> "output"                        { actEmit TokenOutput }

-- Literals (normal mode only)
<0> $digit+ "." $digit+             { \i n -> actEmitLex (\s -> TokenFloat (read s)) i n }
<0> $digit+                         { \i n -> actEmitLex (\s -> TokenInt   (read s)) i n }

<0> \'[^\\\']\'                     { \i n -> actEmitLex (\s -> TokenChar   (read s)) i n }
<0> \"([^\\\"]|\\.)*\"              { \i n -> actEmitLex (\s -> TokenString (read s)) i n }

-- Identifiers (normal mode only)
<0> $alpha $idchar*                 { \i n -> actEmitLex TokenIdent i n }

-- Catch-all in normal mode: skip any remaining character
<0> .                               { skip }

{
-- ===== Tokens =====
data Token
  = TokenLet | TokenIn | TokenIf | TokenThen | TokenElse
  | TokenCase | TokenOf
  | TokenNot
  | TokenBool Bool
  | TokenSuper | TokenSingle | TokenParallel
  | TokenInput | TokenOutput
  | TokenSuperBody String
  | TokenArrow
  | TokenEq | TokenNeq | TokenLe | TokenGe | TokenLt | TokenGt
  | TokenAnd | TokenOr
  | TokenPlus | TokenMinus | TokenTimes | TokenDiv | TokenMod
  | TokenEquals
  | TokenBackslash
  | TokenUnderscore
  | TokenLParen | TokenRParen
  | TokenLBracket | TokenRBracket
  | TokenComma | TokenColon | TokenSemi
  | TokenInt Int | TokenFloat Double | TokenChar Char | TokenString String
  | TokenIdent String
  | TokenEOF
  deriving (Eq, Show)

-- ===== User state: only super bodies =====
data AlexUserState = AlexUserState
  { stSuper   :: [Char]
  , stInSuper :: !Bool
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { stSuper   = []
  , stInSuper = False
  }

-- ===== Emit helpers =====
actEmit :: Token -> AlexAction Token
actEmit t _ _ = pure t

actEmitLex :: (String -> Token) -> AlexAction Token
actEmitLex f (_,_,_,str) n = pure (f (take n str))

-- ===== super mode =====
actBeginSuper :: AlexAction Token
actBeginSuper _ _ = do
  alexSetStartCode super
  st <- alexGetUserState
  alexSetUserState st { stInSuper = True, stSuper = [] }
  alexMonadScan

actSuperAcc :: AlexAction Token
actSuperAcc _ n = do
  (_,_,_,str) <- alexGetInput
  st <- alexGetUserState
  alexSetUserState st { stSuper = stSuper st ++ take n str }
  alexMonadScan

actEndSuper :: AlexAction Token
actEndSuper _ _ = do
  alexSetStartCode 0
  st <- alexGetUserState
  alexSetUserState st { stInSuper = False }
  pure (TokenSuperBody (stSuper st))

-- ===== EOF / scanAll =====

alexEOF :: Alex Token
alexEOF = pure TokenEOF

scanAll :: String -> Either String [Token]
scanAll s = runAlex s (go [])
  where
    go :: [Token] -> Alex [Token]
    go acc = do
      t <- alexMonadScan
      case t of
        TokenEOF -> pure (reverse acc)
        _        -> go (t : acc)

}
