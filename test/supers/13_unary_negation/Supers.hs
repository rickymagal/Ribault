{-# LANGUAGE ForeignFunctionInterface #-}
module Supers where
-- Automatically generated for program: 13_unary_negation

import Foreign.Ptr      (Ptr, IntPtr, ptrToIntPtr, intPtrToPtr)
import Foreign.Storable (peek, poke, peekElemOff)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr
                        , castStablePtrToPtr, castPtrToStablePtr)
import Data.Int         (Int64)
import Data.Word        (Word32)
import Data.Bits        ((.&.))
import GHC.Conc (par, pseq)
import GHC.Float (castWord32ToFloat, castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

-- Profile B: sN :: Ptr Int64 -> Ptr Int64 -> IO ()
-- Contract: reads in[0] and writes to out[0].

-- Encoding helpers compatible with the Builder:
pairBase :: Int64
pairBase = 0
handleNil :: Int64
handleNil = 0

nil :: Int64
nil = handleNil

handleFromStable :: StablePtr (Int64, Int64) -> Int64
handleFromStable sp = fromIntegral (ptrToIntPtr (castStablePtrToPtr sp))

stableFromHandle :: Int64 -> StablePtr (Int64, Int64)
stableFromHandle h = castPtrToStablePtr (intPtrToPtr (fromIntegral h))

readPairIO :: Int64 -> IO (Int64, Int64)
readPairIO h = deRefStablePtr (stableFromHandle h)

mkPairIO :: Int64 -> Int64 -> IO Int64
mkPairIO a b = do sp <- newStablePtr (a, b); pure (handleFromStable sp)

toListIO :: Int64 -> IO [Int64]
toListIO h
  | h == handleNil = pure []
  | otherwise = do
      (a, b) <- readPairIO h
      xs <- toListIO b
      pure (a : xs)

fromListIO :: [Int64] -> IO Int64
fromListIO [] = pure handleNil
fromListIO (a:xs) = do
  b <- fromListIO xs
  mkPairIO a b

toList :: Int64 -> [Int64]
{-# NOINLINE toList #-}
toList h = unsafePerformIO (toListIO h)

fromList :: [Int64] -> Int64
{-# NOINLINE fromList #-}
fromList xs = unsafePerformIO (fromListIO xs)

-- Float helpers (list elements may carry float bits in low 32 bits):
toFloat :: Int64 -> Float
{-# NOINLINE toFloat #-}
toFloat x = castWord32ToFloat (fromIntegral (x .&. 0xffffffff))

fromFloat :: Float -> Int64
{-# NOINLINE fromFloat #-}
fromFloat f = fromIntegral (castFloatToWord32 f)

toListF :: Int64 -> [Float]
{-# NOINLINE toListF #-}
toListF h = map toFloat (toList h)

fromListF :: [Float] -> Int64
{-# NOINLINE fromListF #-}
fromListF xs = fromList (map fromFloat xs)

-- Compatibility helpers for older supers bodies:
encPair :: Int64 -> Int64 -> Int64
{-# NOINLINE encPair #-}
encPair a b = unsafePerformIO (mkPairIO a b)

fstDec :: Int64 -> Int64
{-# NOINLINE fstDec #-}
fstDec h = unsafePerformIO (if h == handleNil then pure 0 else fst <$> readPairIO h)

sndDec :: Int64 -> Int64
{-# NOINLINE sndDec #-}
sndDec h = unsafePerformIO (if h == handleNil then pure 0 else snd <$> readPairIO h)

-- Builtins: s0..s3 reserved for list ops
foreign export ccall "s0" s0 :: Ptr Int64 -> Ptr Int64 -> IO ()
s0 :: Ptr Int64 -> Ptr Int64 -> IO ()
s0 pin pout = do
  a <- peekElemOff pin 0
  b <- peekElemOff pin 1
  h <- mkPairIO a b
  poke pout h

foreign export ccall "s1" s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 pin pout = do
  h <- peek pin
  if h == handleNil
    then poke pout 0
    else do (a, _) <- readPairIO h; poke pout a

foreign export ccall "s2" s2 :: Ptr Int64 -> Ptr Int64 -> IO ()
s2 :: Ptr Int64 -> Ptr Int64 -> IO ()
s2 pin pout = do
  h <- peek pin
  if h == handleNil
    then poke pout 0
    else do (_, b) <- readPairIO h; poke pout b

foreign export ccall "s3" s3 :: Ptr Int64 -> Ptr Int64 -> IO ()
s3 :: Ptr Int64 -> Ptr Int64 -> IO ()
s3 pin pout = do
  h <- peek pin
  if h == handleNil then poke pout 1 else poke pout 0

foreign export ccall "supers_io_init" supers_io_init :: IO ()
supers_io_init :: IO ()
supers_io_init = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
