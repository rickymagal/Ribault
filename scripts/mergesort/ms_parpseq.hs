{-# LANGUAGE BangPatterns #-}
module Main where

-- Mergesort par/pseq variant — same binary tree topology as Ribault-Hs
-- and STRAT (loaded from data_dir/tree.bin), parallelism via raw
-- Control.Parallel (par, pseq) without the Strategies library. Sparks
-- all leaves at level 0, then all merges at each level 1..max, with a
-- strict barrier between levels (the pseq chain forces all sparks).
--
-- Compute kernels identical to ms_strat.hs (and to ribault_hs raw-pointer
-- supers) — the only thing that varies vs STRAT is the spark mechanism:
-- STRAT uses parList rseq (Strategies combinator); parpseq uses manual
-- par/pseq pairs (lower-level idiom).

import Control.Monad (forM_, when)
import Control.Parallel (par, pseq)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
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
    let mb = 2 + 2 * nl
    mergesV <- V.generateM nm $ \i -> do
      !lo  <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mb + i*4)
      !mid <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mb + i*4 + 1)
      !hi  <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mb + i*4 + 2)
      !lev <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (mb + i*4 + 3)
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

{-# INLINE insertionSort #-}
insertionSort :: Ptr Int32 -> Int -> Int -> IO ()
insertionSort !a !lo !hi = go (lo + 1)
  where
    go !i
      | i >= hi   = return ()
      | otherwise = do
          !x <- peekElemOff a i
          let bubble !j
                | j <= lo   = pokeElemOff a j x
                | otherwise = do
                    !y <- peekElemOff a (j - 1)
                    if y > x
                      then do pokeElemOff a j y
                              bubble (j - 1)
                      else pokeElemOff a j x
          bubble i
          go (i + 1)

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

-- Pure-value wrappers (NOINLINE so GHC doesn't share). Each returns ().
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

{-# NOINLINE evalLeaf #-}
evalLeaf :: Int -> ()
evalLeaf !idx = unsafePerformIO $ do
  arr <- readIORef g_arr
  lvs <- readIORef g_leaves
  let !(lo, hi) = lvs V.! idx
  insertionSort arr lo hi

{-# NOINLINE evalMerge #-}
evalMerge :: Int -> ()
evalMerge !idx = unsafePerformIO $ do
  arr <- readIORef g_arr
  tmp <- readIORef g_tmp
  mrgs <- readIORef g_merges
  let !(lo, mid, hi, _) = mrgs V.! idx
  mergeOp arr tmp lo mid hi

-- Spark all indices on a level via recursive par/pseq pairs:
--   x `par` xs `pseq` x `pseq` ()
-- This forces every spark (whole-level barrier).
sparkLeaves :: [Int] -> ()
sparkLeaves []     = ()
sparkLeaves [i]    = evalLeaf i
sparkLeaves (i:rs) =
  let !x  = evalLeaf i
      !xs = sparkLeaves rs
  in x `par` (xs `pseq` x `pseq` ())

sparkMerges :: [Int] -> ()
sparkMerges []     = ()
sparkMerges [i]    = evalMerge i
sparkMerges (i:rs) =
  let !x  = evalMerge i
      !xs = sparkMerges rs
  in x `par` (xs `pseq` x `pseq` ())

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
    _       -> do hPutStrLn stderr "usage: ms_parpseq DATA_DIR"; return ()

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

  let presum !i !acc
        | i >= n    = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !x <- peekElemOff arr i
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            presum (i+1) ((acc + w) .&. 0xFFFFFFFF)
  !pre <- presum 0 0

  t0 <- getCurrentTime

  -- Level 0: all leaves in parallel via par/pseq
  let !rL = sparkLeaves [0..V.length lvs - 1]
  rL `pseq` return ()

  -- Levels 1..max: per-level spark wave with par/pseq barrier
  forM_ levelGroups $ \mergeIdxs -> do
    let !rM = sparkMerges mergeIdxs
    rM `pseq` return ()

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
