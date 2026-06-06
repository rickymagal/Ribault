{-# LANGUAGE BangPatterns #-}
module Main where

-- CYK parsing baseline -- sequential Haskell reference.
--
-- Uses Data.Vector.Unboxed.Mutable IOVector Word64 for the d table
-- (essentially a raw array via newForeignPtr_).  This matches what the
-- parallel variants need (shared mutable storage across sparks/supers).
-- Same -A256m RTS option as the parallel variants for GC fairness.
--
-- Algorithm mirrors cyk_seq.c byte-for-byte: span = 1..N-1, i = 0..N-1-span,
-- k = i..j-1, accumulate `produce_bin[B*64+C]` for each B in d[i][k] and
-- C in d[k+1][j].

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Data.Word (Word8, Word64)
import Data.Bits ((.&.), (.|.), countTrailingZeros)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Monad (forM_)


data Cfg = Cfg { cfgN :: !Int, cfgNNT :: !Int, cfgNSigma :: !Int }


parseCfg :: String -> Cfg
parseCfg s =
  let pairs = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
      look k = maybe 0 id (lookup k pairs)
  in Cfg (look "N") (look "N_NT") (look "N_SIGMA")


readBytes :: FilePath -> Int -> IO (Ptr Word8)
readBytes path n = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes n :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src n
  return p


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _ -> hPutStrLn stderr "usage: cyk_seq DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  cfg <- parseCfg <$> readFile (dir ++ "/config.txt")
  let !n = cfgN cfg
      !nnt = cfgNNT cfg
      !nsigma = cfgNSigma cfg

  -- input.bin
  sP <- readBytes (dir ++ "/input.bin") n  -- Ptr Word8

  -- grammar.bin: 16-byte header, then NNT*NNT * 8 bytes produce_bin, then NSIGMA*8 produce_term
  gP <- readBytes (dir ++ "/grammar.bin") (16 + nnt*nnt*8 + nsigma*8) :: IO (Ptr Word8)
  let pbPtr   = gP `plusPtr` 16              :: Ptr Word64
      ptPtr   = gP `plusPtr` (16 + nnt*nnt*8) :: Ptr Word64

  -- Allocate d[N*N] zero-filled (note: only triangular upper is used)
  dPtr <- mallocBytes (n * n * 8) :: IO (Ptr Word64)
  forM_ [0 .. n*n - 1] $ \i -> pokeElemOff dPtr i (0 :: Word64)

  -- Base diagonal
  forM_ [0 .. n - 1] $ \i -> do
    !sigma <- peekElemOff sP i
    !mask <- peekElemOff ptPtr (fromIntegral sigma)
    pokeElemOff dPtr (i * n + i) mask

  -- Time the main wavefront
  t0 <- getCurrentTime
  forM_ [1 .. n - 1] $ \span_ -> do
    forM_ [0 .. n - 1 - span_] $ \i -> do
      let !j = i + span_
      !acc <- accumulate dPtr pbPtr nnt n i j
      pokeElemOff dPtr (i * n + j) acc
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  top <- peekElemOff dPtr (0 * n + (n - 1))
  putStrLn ("CHECKSUM=" ++ wordToHex top)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout


-- Compute one cell d[i][j]: OR over k=i..j-1 of OR over (B in d[i][k], C in d[k+1][j]) of produce_bin[B*64+C].
{-# INLINE accumulate #-}
accumulate :: Ptr Word64 -> Ptr Word64 -> Int -> Int -> Int -> Int -> IO Word64
accumulate !dPtr !pbPtr !nnt !n !i !j = goK i 0
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !left  <- peekElemOff dPtr (i * n + k)
          !right <- peekElemOff dPtr ((k + 1) * n + j)
          if left == 0 || right == 0
            then goK (k + 1) acc
            else do
              !acc' <- accBits left right acc
              goK (k + 1) acc'
    accBits !lb !right !acc
      | lb == 0 = return acc
      | otherwise = do
          let !b = countTrailingZeros lb
              !lb' = lb .&. (lb - 1)
          !acc'' <- accBits2 right b acc
          accBits lb' right acc''
    accBits2 !rb !b !acc
      | rb == 0 = return acc
      | otherwise = do
          let !c = countTrailingZeros rb
              !rb' = rb .&. (rb - 1)
          !m <- peekElemOff pbPtr (b * nnt + c)
          accBits2 rb' b (acc .|. m)


wordToHex :: Word64 -> String
wordToHex w = pad (showHexUpper w "")
  where
    pad s = replicate (16 - length s) '0' ++ s

showHexUpper :: Word64 -> String -> String
showHexUpper 0 acc = if null acc then "0" else acc
showHexUpper n acc =
  let (q, r) = n `divMod` 16
      d = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                    else toEnum (fromEnum 'A' + fromIntegral r - 10)
  in showHexUpper q (d : acc)
