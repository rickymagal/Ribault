{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Synthesis.SupersEmit
-- Description : Emit a Haskell module exposing s# symbols via FFI (profile B).
-- Maintainer  : ricardofilhoschool@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Generates the Haskell source for a module named @Supers@ that exports
-- super-instruction entry points via the FFI. Each exported symbol follows
-- “profile B”: @sN :: Ptr Int64 -> Ptr Int64 -> IO ()@, reading @in[0]@
-- and writing @out[0]@. Helpers for list encoding/decoding (compatible with
-- the Builder) are emitted alongside the supers.
module Synthesis.SupersEmit
  ( emitSupersModule  -- :: FileBase -> [SuperSpec] -> String
  ) where

import Synthesis.SuperExtract
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Generate a complete Supers.hs module for a given program and its supers.
emitSupersModule :: String -> [SuperSpec] -> String
emitSupersModule baseName specs
  | null specs = ""  -- no supers -> no file
  | otherwise  = unlines $
      [ "{-# LANGUAGE ForeignFunctionInterface #-}"
      , "-- Automatically generated for program: " ++ baseName
      , "module Supers where"
      , ""
      , "import Foreign.Ptr      (Ptr)"
      , "import Foreign.Storable (peek, poke)"
      , "import Data.Int         (Int64)"
      , ""
      , "-- Profile B: sN :: Ptr Int64 -> Ptr Int64 -> IO ()"
      , "-- Contract: reads in[0] and writes to out[0]."
      , ""
      , "-- Encoding helpers compatible with the Builder:"
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
      , "         | otherwise = let { h = fstDec n; t = sndDec n } in h : toList t"
      , ""
      , "fromList :: [Int64] -> Int64"
      , "fromList []     = nil"
      , "fromList (h:ts) = encPair h (fromList ts)"
      ]
      ++ concatMap emitOne specs

-- | Emit one super (its FFI wrapper + pure @_impl@).
emitOne :: SuperSpec -> [String]
emitOne (SuperSpec nm _kind inp out bodyRaw) =
  let bodyCore = normalizeIndent (trimBlankHash bodyRaw)
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
       , "-- Internal pure function:"
       , "-- - decodes the Int64 input into list '" ++ inp ++ "'"
       , "-- - executes the stored body (declarations + definition of '" ++ out ++ "')"
       , "-- - encodes '" ++ out ++ "' back to Int64"
       , nm ++ "_impl :: Int64 -> Int64"
       , nm ++ "_impl x ="
       , "  let"
       , "    " ++ inp ++ " = toList x"
       ]
       ++ indent 4 bodyCore
       ++ [ "  in fromList " ++ out ]

----------------------------------------------------------------
-- Formatting helpers
----------------------------------------------------------------

-- | Drop blank lines at the beginning/end and drop any line whose first
-- non-space character is '#'. This removes markers like #BEGINSUPER/#ENDSUPER
-- and avoids generating invalid Haskell.
trimBlankHash :: String -> String
trimBlankHash s =
  let ls0   = lines s
      isBlank l = all isSpace l
      isHash l  =
        case dropWhile isSpace l of
          ('#':_) -> True
          _       -> False
      isIgn l   = isBlank l || isHash l
      dropBE    = dropWhile isIgn . dropWhileEnd isIgn
      ls1       = dropBE ls0
      ls2       = filter (not . isHash) ls1
  in unlines ls2

-- | Remove the minimal common indentation from all non-blank lines.
normalizeIndent :: String -> [String]
normalizeIndent s =
  let ls       = lines s
      nonblank = filter (not . all isSpace) ls
      leadSpaces l = length (takeWhile isSpace l)
      base     = case nonblank of
                   [] -> 0
                   _  -> minimum (map leadSpaces nonblank)
  in map (drop base) ls

-- | Indent every line by @n@ spaces.
indent :: Int -> [String] -> [String]
indent n ls =
  let pad = replicate n ' '
  in map (pad ++) ls
