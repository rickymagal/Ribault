{-# LANGUAGE ForeignFunctionInterface #-}

-- | Implementação das super-instruções usadas pelo código gerado.
--   Compile com:   ghc -O2 -shared -dynamic -o libsupers.so Supers.hs
module Supers where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes)
import Control.Monad        (forM_)
import Data.Word            (Word32)

-------------------------------------------------------------------------------
-- super:1  ‒  buildList  -----------------------------------------------------
-- Constrói uma lista “caixa” contendo N ponteiros recebidos em @argv@.
-- A cabeça do objeto tem layout:  Word32 tag  |  Word32 len  |  [Ptr () ... ]
-------------------------------------------------------------------------------
foreign export ccall super1 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()

super1 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()
super1 argc argv out = do
  let n       = fromIntegral argc :: Int
      hdrSz   = sizeOf (undefined :: Word32) * 2   -- tag + len
      elemSz  = sizeOf (undefined :: Ptr ())
      boxSize = hdrSz + n * elemSz

  box <- mallocBytes boxSize

  -- header: tag = 1, len = n
  poke               (castPtr box)           (1 :: Word32)
  pokeByteOff box 4  (fromIntegral n :: Word32)

  -- payload
  forM_ [0 .. n-1] $ \i -> do
    ptr <- peekElemOff argv i
    pokeElemOff (castPtr box `plusPtr` 8) i ptr

  -- único token de saída
  poke out box


-------------------------------------------------------------------------------
-- super:2  ‒  phi2  ----------------------------------------------------------
-- Recebe dois ponteiros e devolve o primeiro que não seja NULL.
-------------------------------------------------------------------------------
foreign export ccall super2 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()

super2 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()
super2 _ argv out = do
  a <- peekElemOff argv 0
  b <- peekElemOff argv 1
  poke out (if a /= nullPtr then a else b)


-------------------------------------------------------------------------------
-- super:3  ‒  splitHead2  ----------------------------------------------------
-- Divide uma lista caixa (construída por super1) e devolve os dois
-- primeiros elementos como tokens independentes.
-------------------------------------------------------------------------------
foreign export ccall super3 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()

super3 :: CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ()
super3 _ argv out = do
  boxPtr <- peekElemOff argv 0
  len    <- (peekByteOff boxPtr 4 :: IO Word32)  -- <- anotação no RHS

  if len < 2
     then error "super3: list length < 2"
     else do
       a <- peekElemOff (castPtr boxPtr `plusPtr` 8) 0
       b <- peekElemOff (castPtr boxPtr `plusPtr` 8) 1
       pokeElemOff out 0 a
       pokeElemOff out 1 b
