{-# LANGUAGE BangPatterns #-}
module Main where

-- Mergesort STRAT variant — same binary tree topology as Ribault-Hs
-- (loaded from data_dir/tree.bin), parallelism via parList rseq over
-- each tree LEVEL with a strict barrier between levels. This is the
-- canonical Haskell parallel-numerics idiom for DAGs with fan-in.
--
-- The level field of each merge is used to group merges into per-level
-- batches; each batch is a single `using parList rseq` spark wave,
-- forced strictly before the next level begins.
--
-- Inner kernels: raw `Ptr Int32` with peekElemOff / pokeElemOff —
-- identical to the ribault_hs raw-pointer kernels, so the per-block
-- compute is byte-identical; only the parallel scheduler differs.

import Control.Monad (forM_, when)
import Control.Parallel.Strategies (using, parList, rseq)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as M
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Algorithms.Intro as VAI
import Foreign.ForeignPtr (newForeignPtr_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, poke, peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

readConfig :: FilePath -> IO (Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [ (head ws, read (ws !! 1) :: Int)
           | l <- lines s, let ws = words l, length ws >= 2 ]
  let n      = maybe 0 id (lookup "N" kv)
      cutoff = maybe 64 id (lookup "CUTOFF" kv)
  return (n, cutoff)

-- Load tree.bin: header (n_leaves, n_merges), leaves [(lo,hi)], merges [(lo,mid,hi,level)].
readTree :: FilePath -> IO (V.Vector (Int, Int), V.Vector (Int, Int, Int, Int))
readTree path = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  withForeignPtr fp $ \src8 -> do
    let src = castPtr src8 :: Ptr Int32
    !nl <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src 0
    !nm <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src 1
    leavesV <- V.generateM nl $ \i -> do
      !lo <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (2 + i*2)
      !hi <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (2 + i*2 + 1)
      return (lo, hi)
    let mergeBase = 2 + 2 * nl
    mergesV <- V.generateM nm $ \i -> do
      !lo  <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mergeBase + i*4)
      !mid <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mergeBase + i*4 + 1)
      !hi  <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mergeBase + i*4 + 2)
      !lev <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mergeBase + i*4 + 3)
      return (lo, mid, hi, lev)
    return (leavesV, mergesV)

msReadInput :: FilePath -> Int -> IO (Ptr Int32)
msReadInput path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes (count * 4) :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src (count * 4)
  return (castPtr p)

msAllocI32 :: Int -> IO (Ptr Int32)
msAllocI32 count = do
  p <- mallocBytes (count * 4) :: IO (Ptr Int32)
  forM_ [0..count-1] $ \i -> pokeElemOff p i (0 :: Int32)
  return p

-- Leaf sort: introsort on a Storable.Mutable view of arr[lo..hi).
{-# INLINE leafSort #-}
leafSort :: Ptr Int32 -> Int -> Int -> IO ()
leafSort !a !lo !hi = do
  fp <- newForeignPtr_ (a `plusPtr` (lo * 4))
  let mv = SMV.unsafeFromForeignPtr0 fp (hi - lo) :: SMV.IOVector Int32
  VAI.sort mv

{-# INLINE mergeOp #-}
mergeOp :: Ptr Int32 -> Ptr Int32 -> Int -> Int -> Int -> IO ()
mergeOp !arr !tmp !lo !mid !hi = do
  let loop !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do !y <- peekElemOff arr j; pokeElemOff tmp k y; loop i (j+1) (k+1)
        | j >= hi  = do !x <- peekElemOff arr i; pokeElemOff tmp k x; loop (i+1) j (k+1)
        | otherwise = do
            !x <- peekElemOff arr i
            !y <- peekElemOff arr j
            if x <= y then do pokeElemOff tmp k x; loop (i+1) j     (k+1)
                      else do pokeElemOff tmp k y; loop i     (j+1) (k+1)
  loop lo mid lo
  let cp !p
        | p >= hi   = return ()
        | otherwise = do !v <- peekElemOff tmp p; pokeElemOff arr p v; cp (p + 1)
  cp lo

-- Global state for sparked actions (set in main, read by runLeaf/runMerge).
{-# NOINLINE g_arr #-}
g_arr :: IORef (Ptr Int32)
g_arr = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_tmp #-}
g_tmp :: IORef (Ptr Int32)
g_tmp = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_leaves #-}
g_leaves :: IORef (V.Vector (Int, Int))
g_leaves = unsafePerformIO (newIORef V.empty)
{-# NOINLINE g_merges #-}
g_merges :: IORef (V.Vector (Int, Int, Int, Int))
g_merges = unsafePerformIO (newIORef V.empty)

{-# NOINLINE runLeaf #-}
runLeaf :: Int -> ()
runLeaf !idx = unsafePerformIO $ do
  arr <- readIORef g_arr
  lvs <- readIORef g_leaves
  let !(lo, hi) = lvs V.! idx
  leafSort arr lo hi

{-# NOINLINE runMerge #-}
runMerge :: Int -> ()
runMerge !idx = unsafePerformIO $ do
  arr <- readIORef g_arr
  tmp <- readIORef g_tmp
  mrgs <- readIORef g_merges
  let !(lo, mid, hi, _) = mrgs V.! idx
  mergeOp arr tmp lo mid hi

-- Group merge indices by level (1..max_lev). Returns list of [Int] indexed
-- by level - 1 (so levelGroups !! 0 is the level-1 merges).
groupByLevel :: V.Vector (Int, Int, Int, Int) -> [[Int]]
groupByLevel mrgs =
  let pairs = [(lev, i) | (i, (_, _, _, lev)) <- zip [0..] (V.toList mrgs)]
      maxL  = if V.null mrgs then 0 else maximum [lev | (_, _, _, lev) <- V.toList mrgs]
      buckets = M.fromListWith (++) [(lev, [i]) | (lev, i) <- pairs]
  in [maybe [] reverse (M.lookup k buckets) | k <- [1..maxL]]

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> do hPutStrLn stderr "usage: ms_strat DATA_DIR"; return ()

run :: FilePath -> IO ()
run dir = do
  (n, _cutoff) <- readConfig (dir ++ "/config.txt")
  (lvs, mrgs)  <- readTree (dir ++ "/tree.bin")

  arr <- msReadInput (dir ++ "/input.bin") n
  tmp <- msAllocI32 n
  writeIORef g_arr arr
  writeIORef g_tmp tmp
  writeIORef g_leaves lvs
  writeIORef g_merges mrgs

  let levelGroups = groupByLevel mrgs

  -- Pre-sort checksum
  let presum !i !acc
        | i >= n    = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !x <- peekElemOff arr i
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            presum (i+1) ((acc + w) .&. 0xFFFFFFFF)
  !pre <- presum 0 0

  t0 <- getCurrentTime

  -- Level 0: sort all leaves in parallel (parList rseq spark wave, strict barrier)
  let !rL = map runLeaf [0..V.length lvs - 1] `using` parList rseq
  foldl' seq () rL `seq` return ()

  -- Levels 1..max: per-level barrier
  forM_ levelGroups $ \mergeIdxs -> do
    let !rM = map runMerge mergeIdxs `using` parList rseq
    foldl' seq () rM `seq` return ()

  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  let verifyGo !i !acc
        | i >= n    = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !x <- peekElemOff arr i
            when (i > 0) $ do
              !y <- peekElemOff arr (i - 1)
              when (x < y) $ error ("not sorted at i=" ++ show i)
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            verifyGo (i+1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- verifyGo 0 0
  when (cs /= pre) $ error ("checksum changed pre=" ++ show pre ++ " post=" ++ show cs)

  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
