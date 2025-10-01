{-# LANGUAGE ForeignFunctionInterface #-}

-- Emite o módulo Haskell com símbolos s# exportados via FFI (perfil B)
module Synthesis.SupersEmit
  ( emitSupersModule  -- :: FileBase -> [SuperSpec] -> String
  ) where

import Synthesis.SuperExtract
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

emitSupersModule :: String -> [SuperSpec] -> String
emitSupersModule baseName specs
  | null specs = ""  -- sem supers → sem arquivo
  | otherwise  = unlines $
      [ "{-# LANGUAGE ForeignFunctionInterface #-}"
      , "-- Gerado automaticamente para o programa: " ++ baseName
      , "module Supers where"
      , ""
      , "import Foreign.Ptr (Ptr)"
      , "import Foreign.Storable (peek, poke)"
      , "import Data.Int (Int64)"
      , ""
      , "-- Perfil B: sN :: Ptr Int64 -> Ptr Int64 -> IO ()"
      , "-- Contrato: lê in[0] e escreve em out[0]."
      , ""
      , "-- Helpers de codificação compatíveis com o Builder:"
      , "pairBase :: Int64"
      , "pairBase = 1000003"
      , ""
      , "nil :: Int64"
      , "nil = -1"
      , ""
      , "encPair :: Int64 -> Int64 -> Int64"
      , "encPair a b = a * pairBase + b"
      , ""
      , "fstDec :: Int64 -> Int64"
      , "fstDec p = p `div` pairBase"
      , ""
      , "sndDec :: Int64 -> Int64"
      , "sndDec p = p - (p `div` pairBase) * pairBase"
      , ""
      , "toList :: Int64 -> [Int64]"
      , "toList n | n == nil  = []"
      , "         | otherwise = let h = fstDec n; t = sndDec n in h : toList t"
      , ""
      , "fromList :: [Int64] -> Int64"
      , "fromList []     = nil"
      , "fromList (h:ts) = encPair h (fromList ts)"
      ]
      ++ concatMap emitOne specs

emitOne :: SuperSpec -> [String]
emitOne (SuperSpec nm _kind inp out bodyRaw) =
  let bodyCore = normalizeIndent (trimBlankEnds bodyRaw)  -- [String]
  in if null bodyCore
     then
       [ ""
       , "-- " ++ nm
       , "foreign export ccall \"" ++ nm ++ "\" " ++ nm ++ " :: Ptr Int64 -> Ptr Int64 -> IO ()"
       , nm ++ " :: Ptr Int64 -> Ptr Int64 -> IO ()"
       , nm ++ " pin pout = do"
       , "  x <- peek pin"
       , "  let r = " ++ nm ++ "_impl x"
       , "  poke pout r"
       , ""
       , nm ++ "_impl :: Int64 -> Int64"
       , nm ++ "_impl _x = 0"
       ]
     else
       [ ""
       , "-- " ++ nm
       , "foreign export ccall \"" ++ nm ++ "\" " ++ nm ++ " :: Ptr Int64 -> Ptr Int64 -> IO ()"
       , nm ++ " :: Ptr Int64 -> Ptr Int64 -> IO ()"
       , nm ++ " pin pout = do"
       , "  x <- peek pin"
       , "  let r = " ++ nm ++ "_impl x"
       , "  poke pout r"
       , ""
       , "-- Função pura interna:"
       , "-- - decodifica a entrada Int64 para lista em '" ++ inp ++ "'"
       , "-- - executa o corpo salvo na AST (declarações + definição de '" ++ out ++ "')"
       , "-- - codifica '" ++ out ++ "' de volta para Int64"
       , nm ++ "_impl :: Int64 -> Int64"
       , nm ++ "_impl x ="
       , "  let"
       , "    " ++ inp ++ " = toList x"
       ]
       ++ indent 4 bodyCore   -- <<< alinhar todas as bindings do let no mesmo nível
       ++ [ "  in fromList " ++ out ]

-- ===== helpers de formatação =====

trimBlankEnds :: String -> String
trimBlankEnds s =
  let ls   = lines s
      dropBE = dropWhile isBlank . dropWhileEnd isBlank
  in unlines (dropBE ls)
  where
    isBlank l = all isSpace l

-- remove o recuo mínimo comum das linhas não vazias
normalizeIndent :: String -> [String]
normalizeIndent s =
  let ls = lines s
      nonblank = filter (not . all isSpace) ls
      leadSpaces l = length (takeWhile isSpace l)
      base = case nonblank of
               [] -> 0
               _  -> minimum (map leadSpaces nonblank)
  in map (drop base) ls

indent :: Int -> [String] -> [String]
indent n ls =
  let pad = replicate n ' '
  in map (pad ++) ls
