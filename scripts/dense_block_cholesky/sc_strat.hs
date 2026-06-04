{-# LANGUAGE BangPatterns #-}
module Main where

-- Dense Block Cholesky STRAT variant — same DAG as Ribault-Hs (loaded from
-- dag.bin), parallelism via parList rseq with per-level barrier.
-- Inner kernels: raw Ptr Double, matches sc_seq.hs byte-for-byte.

import Control.Monad (forM_, when)
import Control.Parallel.Strategies (using, parList, rseq)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as UM
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

readConfig :: FilePath -> IO (Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [(head ws, read (ws!!1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
  return (maybe 0 id (lookup "NB" kv), maybe 64 id (lookup "B" kv))

{-# INLINE blockIdx #-}
blockIdx :: Int -> Int -> Int
blockIdx i j = i*(i+1) `div` 2 + j

readBin :: FilePath -> Int -> IO (Ptr Double)
readBin path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return (castPtr p)

-- Each op: (kind, ti, tj, s1i, s1j, s2i, s2j). The dag.bin format also
-- contains a pre-computed `level` field at offset+7 — we IGNORE it here
-- and compute our own level assignment from `deps` inside the timed
-- region, symmetric with the work Ribault's runtime does. Returned
-- alongside the ops is the per-op dependency list.
readDag :: FilePath -> IO (V.Vector (Int, Int, Int, Int, Int, Int, Int), V.Vector [Int])
readDag path = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  withForeignPtr fp $ \src8 -> do
    let src = castPtr src8 :: Ptr Int32
    !n <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src 0
    let readOp !off = do
          !k  <- fromIntegral <$> peekElemOff src off
          !ti <- fromIntegral <$> peekElemOff src (off + 1)
          !tj <- fromIntegral <$> peekElemOff src (off + 2)
          !s1i<- fromIntegral <$> peekElemOff src (off + 3)
          !s1j<- fromIntegral <$> peekElemOff src (off + 4)
          !s2i<- fromIntegral <$> peekElemOff src (off + 5)
          !s2j<- fromIntegral <$> peekElemOff src (off + 6)
          -- offset+7 is the pre-computed level — intentionally NOT read.
          !nd <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 8)
          ds <- mapM (\j -> (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 9 + j)) [0..nd-1]
          return ((k :: Int, ti :: Int, tj :: Int, s1i :: Int, s1j :: Int, s2i :: Int, s2j :: Int), ds, off + 9 + nd)
    let collect !off !acc !accDeps !left
          | left == 0 = return (V.fromList (reverse acc), V.fromList (reverse accDeps))
          | otherwise = do
              (op, ds, off') <- readOp off
              collect off' (op : acc) (ds : accDeps) (left - 1)
    collect 1 [] [] n

{-# INLINE potrfBlock #-}
potrfBlock :: Ptr Double -> Int -> Int -> IO ()
potrfBlock !arr !tgt !b = do
  forM_ [0..b-1] $ \j -> do
    !s0 <- peekElemOff arr (tgt + j*b + j)
    let go !kk !acc | kk >= j = return acc
                    | otherwise = do !x <- peekElemOff arr (tgt + j*b + kk); go (kk+1) (acc - x*x)
    !s <- go 0 s0
    let !sq = sqrt s
    pokeElemOff arr (tgt + j*b + j) sq
    let !inv = 1.0 / sq
    forM_ [j+1..b-1] $ \i -> do
      !t0 <- peekElemOff arr (tgt + i*b + j)
      let go2 !kk !acc | kk >= j = return acc
                       | otherwise = do
                           !a <- peekElemOff arr (tgt + i*b + kk)
                           !c <- peekElemOff arr (tgt + j*b + kk)
                           go2 (kk+1) (acc - a*c)
      !t <- go2 0 t0
      pokeElemOff arr (tgt + i*b + j) (t * inv)
  forM_ [0..b-1] $ \i -> forM_ [i+1..b-1] $ \j -> pokeElemOff arr (tgt + i*b + j) 0.0

{-# INLINE trsmBlock #-}
trsmBlock :: Ptr Double -> Int -> Int -> Int -> IO ()
trsmBlock !arr !x !l !b =
  forM_ [0..b-1] $ \i -> forM_ [0..b-1] $ \j -> do
    !s0 <- peekElemOff arr (x + i*b + j)
    let go !kk !acc | kk >= j = return acc
                    | otherwise = do
                        !a <- peekElemOff arr (x + i*b + kk)
                        !c <- peekElemOff arr (l + j*b + kk)
                        go (kk+1) (acc - a*c)
    !s <- go 0 s0
    !lj <- peekElemOff arr (l + j*b + j)
    pokeElemOff arr (x + i*b + j) (s / lj)

{-# INLINE syrkBlock #-}
syrkBlock :: Ptr Double -> Int -> Int -> Int -> IO ()
syrkBlock !arr !c !a !b =
  forM_ [0..b-1] $ \i -> forM_ [0..i] $ \j -> do
    let go !kk !acc | kk >= b = return acc
                    | otherwise = do
                        !x <- peekElemOff arr (a + i*b + kk)
                        !y <- peekElemOff arr (a + j*b + kk)
                        go (kk+1) (acc + x*y)
    !s <- go 0 0.0
    !c0 <- peekElemOff arr (c + i*b + j)
    pokeElemOff arr (c + i*b + j) (c0 - s)

{-# INLINE gemmBlock #-}
gemmBlock :: Ptr Double -> Int -> Int -> Int -> Int -> IO ()
gemmBlock !arr !c !a !b_ !b =
  forM_ [0..b-1] $ \i -> forM_ [0..b-1] $ \j -> do
    let go !kk !acc | kk >= b = return acc
                    | otherwise = do
                        !x <- peekElemOff arr (a + i*b + kk)
                        !y <- peekElemOff arr (b_ + j*b + kk)
                        go (kk+1) (acc + x*y)
    !s <- go 0 0.0
    !c0 <- peekElemOff arr (c + i*b + j)
    pokeElemOff arr (c + i*b + j) (c0 - s)

{-# NOINLINE g_arr #-}
g_arr :: IORef (Ptr Double)
g_arr = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_b #-}
g_b :: IORef Int
g_b = unsafePerformIO (newIORef 0)
{-# NOINLINE g_ops #-}
g_ops :: IORef (V.Vector (Int, Int, Int, Int, Int, Int, Int))
g_ops = unsafePerformIO (newIORef V.empty)

{-# NOINLINE runOp #-}
runOp :: Int -> ()
runOp !idx = unsafePerformIO $ do
  arr <- readIORef g_arr
  b   <- readIORef g_b
  ops <- readIORef g_ops
  let (k, ti, tj, s1i, s1j, s2i, s2j) = ops V.! idx
      blockOff i j = blockIdx i j * b * b
  case k of
    0 -> potrfBlock arr (blockOff ti tj) b
    1 -> trsmBlock  arr (blockOff ti tj) (blockOff s1i s1j) b
    2 -> syrkBlock  arr (blockOff ti tj) (blockOff s1i s1j) b
    3 -> gemmBlock  arr (blockOff ti tj) (blockOff s1i s1j) (blockOff s2i s2j) b
    _ -> error "unknown op"

-- Compute level[i] = 1 + max(level[d] for d in deps[i]). Ops are stored
-- in topological order so a single left-to-right mutable pass is enough.
-- O(n + sum |deps[i]|). This runs INSIDE the timed region — it is the
-- equivalent of the topological work Ribault's runtime does at dispatch
-- time.
computeLevels :: V.Vector [Int] -> IO (UM.IOVector Int, Int)
computeLevels deps = do
  let n = V.length deps
  arr <- UM.replicate n (0 :: Int)
  let go !i !mx
        | i >= n = return mx
        | otherwise = do
            let ds = deps V.! i
            lv <- if null ds
                    then return 1
                    else do ls <- mapM (UM.read arr) ds; return (1 + maximum ls)
            UM.write arr i lv
            go (i + 1) (max mx lv)
  mx <- go 0 0
  return (arr, mx)

groupByLevel :: UM.IOVector Int -> Int -> IO [[Int]]
groupByLevel levels maxL = do
  let n = UM.length levels
  pairs <- mapM (\i -> do !lv <- UM.read levels i; return (lv, i)) [0..n-1]
  let buckets = M.fromListWith (++) [(lv, [i]) | (lv, i) <- pairs]
  return [maybe [] reverse (M.lookup k buckets) | k <- [1..maxL]]

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: sc_strat DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  (nb, b) <- readConfig (dir ++ "/config.txt")
  let !nBlocks = nb*(nb+1) `div` 2
      !nbytes  = nBlocks * b * b * 8
  arr <- readBin (dir ++ "/A.bin") nbytes
  (ops, deps) <- readDag (dir ++ "/dag.bin")
  writeIORef g_arr arr
  writeIORef g_b b
  writeIORef g_ops ops

  t0 <- getCurrentTime
  -- Level computation is part of STRAT's job, inside the timed region.
  (levels, maxL) <- computeLevels deps
  levelGroups <- groupByLevel levels maxL
  forM_ levelGroups $ \opIdxs -> do
    let !rs = map runOp opIdxs `using` parList rseq
    foldl' seq () rs `seq` return ()
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  let totalElems = nBlocks * b * b
      goCs !i !acc
        | i >= totalElems = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !v <- peekElemOff arr i
            let !fixed = truncate (v * 1e6) :: Int
                !w = fromIntegral (fromIntegral fixed :: Word32) :: Word64
            goCs (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
