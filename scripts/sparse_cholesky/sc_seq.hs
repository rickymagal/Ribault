{-# LANGUAGE BangPatterns #-}
module Main where

-- Dense block Cholesky sequential baseline in Haskell — raw Ptr Double,
-- peekElemOff / pokeElemOff in inner kernels. Same algorithm as sc_seq.c.
-- Block (i, j) at element offset blockIdx(i,j) * B*B into the contiguous
-- Ptr Double; element (r, c) of that block at +r*B+c.

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, castPtr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)

readConfig :: FilePath -> IO (Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [ (head ws, read (ws !! 1) :: Int)
           | l <- lines s, let ws = words l, length ws >= 2 ]
  return (maybe 0 id (lookup "NB" kv), maybe 64 id (lookup "B" kv))

{-# INLINE blockIdx #-}
blockIdx :: Int -> Int -> Int
blockIdx i j = i * (i + 1) `div` 2 + j

readBin :: FilePath -> Int -> IO (Ptr Double)
readBin path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return (castPtr p)

-- POTRF on block at base offset `tgt` (in elements), block size B.
{-# INLINE potrfBlock #-}
potrfBlock :: Ptr Double -> Int -> Int -> IO ()
potrfBlock !arr !tgt !b = do
  forM_ [0..b-1] $ \j -> do
    !s0 <- peekElemOff arr (tgt + j*b + j)
    let go !kk !acc
          | kk >= j   = return acc
          | otherwise = do
              !x <- peekElemOff arr (tgt + j*b + kk)
              go (kk + 1) (acc - x*x)
    !s <- go 0 s0
    let !sq = sqrt s
    pokeElemOff arr (tgt + j*b + j) sq
    let !inv = 1.0 / sq
    forM_ [j+1..b-1] $ \i -> do
      !t0 <- peekElemOff arr (tgt + i*b + j)
      let go2 !kk !acc
            | kk >= j   = return acc
            | otherwise = do
                !a <- peekElemOff arr (tgt + i*b + kk)
                !c <- peekElemOff arr (tgt + j*b + kk)
                go2 (kk + 1) (acc - a*c)
      !t <- go2 0 t0
      pokeElemOff arr (tgt + i*b + j) (t * inv)
  forM_ [0..b-1] $ \i ->
    forM_ [i+1..b-1] $ \j -> pokeElemOff arr (tgt + i*b + j) 0.0

{-# INLINE trsmBlock #-}
trsmBlock :: Ptr Double -> Int -> Int -> Int -> IO ()
trsmBlock !arr !x !l !b =
  forM_ [0..b-1] $ \i ->
    forM_ [0..b-1] $ \j -> do
      !s0 <- peekElemOff arr (x + i*b + j)
      let go !kk !acc
            | kk >= j   = return acc
            | otherwise = do
                !a <- peekElemOff arr (x + i*b + kk)
                !c <- peekElemOff arr (l + j*b + kk)
                go (kk + 1) (acc - a*c)
      !s <- go 0 s0
      !lj <- peekElemOff arr (l + j*b + j)
      pokeElemOff arr (x + i*b + j) (s / lj)

{-# INLINE syrkBlock #-}
syrkBlock :: Ptr Double -> Int -> Int -> Int -> IO ()
syrkBlock !arr !c !a !b =
  forM_ [0..b-1] $ \i ->
    forM_ [0..i] $ \j -> do
      let go !kk !acc
            | kk >= b   = return acc
            | otherwise = do
                !x <- peekElemOff arr (a + i*b + kk)
                !y <- peekElemOff arr (a + j*b + kk)
                go (kk + 1) (acc + x*y)
      !s <- go 0 0.0
      !c0 <- peekElemOff arr (c + i*b + j)
      pokeElemOff arr (c + i*b + j) (c0 - s)

{-# INLINE gemmBlock #-}
gemmBlock :: Ptr Double -> Int -> Int -> Int -> Int -> IO ()
gemmBlock !arr !c !a !b_ !b =
  forM_ [0..b-1] $ \i ->
    forM_ [0..b-1] $ \j -> do
      let go !kk !acc
            | kk >= b   = return acc
            | otherwise = do
                !x <- peekElemOff arr (a + i*b + kk)
                !y <- peekElemOff arr (b_ + j*b + kk)
                go (kk + 1) (acc + x*y)
      !s <- go 0 0.0
      !c0 <- peekElemOff arr (c + i*b + j)
      pokeElemOff arr (c + i*b + j) (c0 - s)

-- (kind, ti, tj, s1i, s1j, s2i, s2j)
readDag :: FilePath -> IO [(Int, Int, Int, Int, Int, Int, Int)]
readDag path = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  withForeignPtr fp $ \src8 -> do
    let src = castPtr src8 :: Ptr Int32
    !n <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src 0
    let readOp !off = do
          !k  <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src off
          !ti <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 1)
          !tj <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 2)
          !s1i<- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 3)
          !s1j<- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 4)
          !s2i<- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 5)
          !s2j<- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 6)
          !nd <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 8)
          return ((k, ti, tj, s1i, s1j, s2i, s2j), off + 9 + nd)
    let collect !off !acc !left
          | left == 0 = return (reverse acc)
          | otherwise = do
              (op, off') <- readOp off
              collect off' (op : acc) (left - 1)
    collect 1 [] n

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: sc_seq_hs DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  (nb, b) <- readConfig (dir ++ "/config.txt")
  let !nBlocks = nb * (nb + 1) `div` 2
      !blockSize = b * b
      !nbytes = nBlocks * blockSize * 8
      blockOff i j = blockIdx i j * blockSize  -- element offset
  arr <- readBin (dir ++ "/A.bin") nbytes
  ops <- readDag (dir ++ "/dag.bin")

  t0 <- getCurrentTime
  forM_ ops $ \(kind, ti, tj, s1i, s1j, s2i, s2j) ->
    case kind of
      0 -> potrfBlock arr (blockOff ti tj) b
      1 -> trsmBlock  arr (blockOff ti tj) (blockOff s1i s1j) b
      2 -> syrkBlock  arr (blockOff ti tj) (blockOff s1i s1j) b
      3 -> gemmBlock  arr (blockOff ti tj) (blockOff s1i s1j) (blockOff s2i s2j) b
      _ -> error "unknown op"
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  -- Checksum
  let totalElems = nBlocks * blockSize
      go !i !acc
        | i >= totalElems = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !v <- peekElemOff arr i
            let !fixed = truncate (v * 1e6) :: Int   -- toward zero, matches C (int64_t) cast
                !w = fromIntegral (fromIntegral fixed :: Word32) :: Word64
            go (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- go 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
