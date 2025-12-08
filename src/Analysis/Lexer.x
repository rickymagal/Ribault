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

$white+                              { skip }
\n                                   { skip }
"--".*                               { skip }

<0> "#BEGINSUPER"                    { actBeginSuper }
<super> "#ENDSUPER"                  { actEndSuper }
<super> \n                           { actSuperAcc }
<super> .                            { actSuperAcc }

"let"                                { actEmit TokenLet }
"in"                                 { actEmit TokenIn }
"if"                                 { actEmit TokenIf }
"then"                               { actEmit TokenThen }
"else"                               { actEmit TokenElse }
"case"                               { actEmit TokenCase }
"of"                                 { actEmit TokenOf }
"not"                                { actEmit TokenNot }

"True"                               { actEmit (TokenBool True) }
"False"                              { actEmit (TokenBool False) }

"->"                                 { actEmit TokenArrow }
"=="                                 { actEmit TokenEq }
"/="                                 { actEmit TokenNeq }
"<="                                 { actEmit TokenLe }
">="                                 { actEmit TokenGe }
"<"                                  { actEmit TokenLt }
">"                                  { actEmit TokenGt }
"&&"                                 { actEmit TokenAnd }
"||"                                 { actEmit TokenOr }

"\+"                                 { actEmit TokenPlus }
"-"                                  { actEmit TokenMinus }
"*"                                  { actEmit TokenTimes }
"/"                                  { actEmit TokenDiv }
"%"                                  { actEmit TokenMod }

"="                                  { actEmit TokenEquals }
[\\]                                 { actEmit TokenBackslash }
"_"                                  { actEmit TokenUnderscore }

"("                                  { actEmit TokenLParen }
")"                                  { actEmit TokenRParen }
"["                                  { actEmit TokenLBracket }
"]"                                  { actEmit TokenRBracket }
","                                  { actEmit TokenComma }
":"                                  { actEmit TokenColon }
";"                                  { actEmit TokenSemi }

"super"                              { actEmit TokenSuper }
"single"                             { actEmit TokenSingle }
"parallel"                           { actEmit TokenParallel }
"input"                              { actEmit TokenInput }
"output"                             { actEmit TokenOutput }

$digit+ "." $digit+                  { \i n -> actEmitLex (\s -> TokenFloat (read s)) i n }
$digit+                              { \i n -> actEmitLex (\s -> TokenInt   (read s)) i n }

\'[^\\\']\'                          { \i n -> actEmitLex (\s -> TokenChar   (read s)) i n }
\"([^\\\"]|\\.)*\"                   { \i n -> actEmitLex (\s -> TokenString (read s)) i n }

$alpha $idchar*                      { \i n -> actEmitLex TokenIdent i n }

-- catch-all: consume any remaining character (including weird spaces) and ignore
.                                     { skip }

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

-- ===== User state (only for super bodies) =====
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

-- Alex will call this when it hits real EOF; we map it to a sentinel token.
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
