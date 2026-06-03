{-# LANGUAGE BangPatterns #-}
module Main where

-- Mergesort sequential baseline in Haskell — monolithic top-down
-- recursive merge sort on Data.Vector.Unboxed.Mutable Int32. Below
-- CUTOFF: insertion sort. Above: split, recurse, merge into scratch,
-- copy back. Same algorithm as ms_seq.c and ms_seq.rs.

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed.Mutable as MV
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

readConfig :: FilePath -> IO (Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [ (head ws, read (ws !! 1) :: Int)
           | l <- lines s, let ws = words l, length ws >= 2 ]
  let n      = maybe 0 id (lookup "N" kv)
      cutoff = maybe 64 id (lookup "CUTOFF" kv)
  return (n, cutoff)

readInput :: FilePath -> Int -> IO (MV.IOVector Int32)
readInput path n = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  v <- MV.replicate n 0
  withForeignPtr fp $ \src ->
    forM_ [0..n-1] $ \i -> do
      !w <- peek (src `plusPtr` (i*4) :: Ptr Int32)
      MV.write v i w
  return v

{-# INLINE insertionSort #-}
insertionSort :: MV.IOVector Int32 -> Int -> Int -> IO ()
insertionSort !a !lo !hi = go (lo + 1)
  where
    go !i
      | i >= hi   = return ()
      | otherwise = do
          !x <- MV.read a i
          let bubble !j
                | j <= lo   = MV.write a j x
                | otherwise = do
                    !y <- MV.read a (j - 1)
                    if y > x
                      then do MV.write a j y
                              bubble (j - 1)
                      else MV.write a j x
          bubble i
          go (i + 1)

{-# INLINE mergeTo #-}
mergeTo :: MV.IOVector Int32 -> Int -> Int -> Int -> MV.IOVector Int32 -> IO ()
mergeTo !a !lo !mid !hi !t = do
  let loop !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do !y <- MV.read a j; MV.write t k y; loop i (j+1) (k+1)
        | j >= hi  = do !x <- MV.read a i; MV.write t k x; loop (i+1) j (k+1)
        | otherwise = do
            !x <- MV.read a i
            !y <- MV.read a j
            if x <= y then do MV.write t k x; loop (i+1) j     (k+1)
                      else do MV.write t k y; loop i     (j+1) (k+1)
  loop lo mid lo
  -- copy back t[lo..hi) into a[lo..hi)
  let cp !p
        | p >= hi   = return ()
        | otherwise = do !v <- MV.read t p; MV.write a p v; cp (p + 1)
  cp lo

msSort :: Int -> MV.IOVector Int32 -> Int -> Int -> MV.IOVector Int32 -> IO ()
msSort !cutoff !a !lo !hi !t
  | hi - lo <= cutoff = insertionSort a lo hi
  | otherwise = do
      let !mid = lo + (hi - lo) `div` 2
      msSort cutoff a lo  mid t
      msSort cutoff a mid hi  t
      mergeTo  a lo  mid hi  t

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> do hPutStrLn stderr "usage: ms_seq_hs DATA_DIR"; return ()

run :: FilePath -> IO ()
run dir = do
  (n, cutoff) <- readConfig (dir ++ "/config.txt")
  arr <- readInput (dir ++ "/input.bin") n
  tmp <- MV.replicate n 0

  -- Pre-sort checksum (sum mod 2^32)
  let presumGo !i !acc
        | i >= n    = return (acc :: Word64)
        | otherwise = do
            !x <- MV.read arr i
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            presumGo (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !pre <- presumGo 0 0

  t0 <- getCurrentTime
  msSort cutoff arr 0 n tmp
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  -- Verify sorted + post-sort checksum
  let verifyGo !i !acc
        | i >= n    = return (acc :: Word64)
        | otherwise = do
            !x <- MV.read arr i
            when (i > 0) $ do
              !y <- MV.read arr (i - 1)
              when (x < y) $ do
                hPutStrLn stderr ("FATAL: not sorted at i=" ++ show i)
                error "not sorted"
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            verifyGo (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- verifyGo 0 0
  when (cs /= pre) $ do
    hPutStrLn stderr ("FATAL: checksum changed pre=" ++ show pre ++ " post=" ++ show cs)
    error "checksum mismatch"

  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
