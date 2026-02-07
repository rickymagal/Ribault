{-# LANGUAGE ForeignFunctionInterface #-}
module Supers where
-- Automatically generated for program: ms_coarse_v3

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

-- s4
foreign export ccall "s4" s4 :: Ptr Int64 -> Ptr Int64 -> IO ()
s4 :: Ptr Int64 -> Ptr Int64 -> IO ()
s4 pin pout = do
  x <- peek pin
  let r = s4_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'lst'
-- - executes the stored body (declarations + definition of 'sorted')
-- - encodes 'sorted' back to Int64
s4_impl :: Int64 -> Int64
s4_impl x =
  let
    lst = toList x
    sorted =
      let
        splitLocal []         = ([], [])
        splitLocal [x]        = ([x], [])
        splitLocal (x:y:rest) =
          let (xs, ys) = splitLocal rest
          in (x:xs, y:ys)
    
        mergeLocal [] ys        = ys
        mergeLocal xs []        = xs
        mergeLocal (x:xt) (y:yt)
          | x <= y              = x : mergeLocal xt (y:yt)
          | otherwise           = y : mergeLocal (x:xt) yt
    
        ms []  = []
        ms [x] = [x]
        ms xs  =
          let (l, r) = splitLocal xs
          in mergeLocal (ms l) (ms r)
      in
        ms lst
  in fromList sorted

-- s5
foreign export ccall "s5" s5 :: Ptr Int64 -> Ptr Int64 -> IO ()
s5 :: Ptr Int64 -> Ptr Int64 -> IO ()
s5 pin pout = do
  x <- peek pin
  let r = s5_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'pair'
-- - executes the stored body (declarations + definition of 'result')
-- - encodes 'result' back to Int64
s5_impl :: Int64 -> Int64
s5_impl x =
  let
    pair = toList x
    result =
      let
        l  = toList (head pair)
        r  = toList (head (tail pair))
        mg [] ys        = ys
        mg xs []        = xs
        mg (x:xt) (y:yt)
          | x <= y     = x : mg xt (y:yt)
          | otherwise  = y : mg (x:xt) yt
      in mg l r
  in fromList result

-- s6
foreign export ccall "s6" s6 :: Ptr Int64 -> Ptr Int64 -> IO ()
s6 :: Ptr Int64 -> Ptr Int64 -> IO ()
s6 pin pout = do
  x <- peek pin
  let r = s6_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'lst'
-- - executes the stored body (declarations + definition of 'result')
-- - encodes 'result' back to Int64
s6_impl :: Int64 -> Int64
s6_impl x =
  let
    lst = toList x
    result =
      let
        sp []         = ([], [])
        sp [x]        = ([x], [])
        sp (x:y:rest) = let (xs, ys) = sp rest in (x:xs, y:ys)
        (l, r) = sp lst
      in [fromList l, fromList r]
  in fromList result

-- s7
foreign export ccall "s7" s7 :: Ptr Int64 -> Ptr Int64 -> IO ()
s7 :: Ptr Int64 -> Ptr Int64 -> IO ()
s7 pin pout = do
  x <- peek pin
  let r = s7_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'inp'
-- - executes the stored body (declarations + definition of 'result')
-- - encodes 'result' back to Int64
s7_impl :: Int64 -> Int64
s7_impl x =
  let
    inp = toList x
    result = let n = head inp in [n, n-1 .. 1]
  in fromList result

foreign export ccall "supers_io_init" supers_io_init :: IO ()
supers_io_init :: IO ()
supers_io_init = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
