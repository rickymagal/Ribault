{-# LANGUAGE ForeignFunctionInterface #-}
-- Automatically generated for program: 00_hello_world
module Supers where

import Foreign.Ptr      (Ptr)
import Foreign.Storable (peek, poke)
import Data.Int         (Int64)

-- Profile B: sN :: Ptr Int64 -> Ptr Int64 -> IO ()
-- Contract: reads in[0] and writes to out[0].

-- Encoding helpers compatible with the Builder:
pairBase :: Int64
pairBase = 1000003

nil :: Int64
nil = -1

encPair :: Int64 -> Int64 -> Int64
encPair a b = a * pairBase + b

fstDec :: Int64 -> Int64
fstDec p = p `div` pairBase

sndDec :: Int64 -> Int64
sndDec p = p - (p `div` pairBase) * pairBase

toList :: Int64 -> [Int64]
toList n | n == nil  = []
         | otherwise = let h = fstDec n
                            t = sndDec n
                        in h : toList t

fromList :: [Int64] -> Int64
fromList []     = nil
fromList (h:ts) = encPair h (fromList ts)

-- s1
foreign export ccall "s1" s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 :: Ptr Int64 -> Ptr Int64 -> IO ()
s1 pin pout = do
  x <- peek pin
  let r = s1_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'cs'
-- - executes the stored body (declarations + definition of 'out')
-- - encodes 'out' back to Int64
s1_impl :: Int64 -> Int64
s1_impl x =
  let
    cs = toList x
    hello = print "Hello World!"
    shown = print cs
    out   = [0]
  in fromList out

-- s2
foreign export ccall "s2" s2 :: Ptr Int64 -> Ptr Int64 -> IO ()
s2 :: Ptr Int64 -> Ptr Int64 -> IO ()
s2 pin pout = do
  x <- peek pin
  let r = s2_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'pairs'
-- - executes the stored body (declarations + definition of 'cs')
-- - encodes 'cs' back to Int64
s2_impl :: Int64 -> Int64
s2_impl x =
  let
    pairs = toList x
    cs = map (\p -> fstDec p + sndDec p) pairs
  in fromList cs

-- s3
foreign export ccall "s3" s3 :: Ptr Int64 -> Ptr Int64 -> IO ()
s3 :: Ptr Int64 -> Ptr Int64 -> IO ()
s3 pin pout = do
  x <- peek pin
  let r = s3_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'xs'
-- - executes the stored body (declarations + definition of 'pairs')
-- - encodes 'pairs' back to Int64
s3_impl :: Int64 -> Int64
s3_impl x =
  let
    xs = toList x
    pairs = map (\x -> encPair x (x + 1)) xs
  in fromList pairs

-- s4
foreign export ccall "s4" s4 :: Ptr Int64 -> Ptr Int64 -> IO ()
s4 :: Ptr Int64 -> Ptr Int64 -> IO ()
s4 pin pout = do
  x <- peek pin
  let r = s4_impl x
  poke pout r

-- Internal pure function:
-- - decodes the Int64 input into list 'dummy'
-- - executes the stored body (declarations + definition of 'xs')
-- - encodes 'xs' back to Int64
s4_impl :: Int64 -> Int64
s4_impl x =
  let
    dummy = toList x
    xs = [0 .. 99]
  in fromList xs
