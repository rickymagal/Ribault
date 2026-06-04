{-# LANGUAGE BangPatterns #-}
module Main where

-- Cascading Inference Pipeline — par/pseq variant in Haskell.
-- Same structure as cip_strat.hs but with raw Control.Parallel sparks
-- (par, pseq) instead of the Strategies parList combinator.  Strict
-- barrier between stages — par/pseq has no native way to express
-- dependency-based pipelining either.

import Control.Monad (forM_, when)
import Control.Parallel (par, pseq)
import Data.Bits ((.&.), (.|.))
import Data.Int (Int16, Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

dimD, b2Slots, eDim, k3, hDim, cCls, k1 :: Int
dimD    = 256
b2Slots = 256
eDim    = 64
k3      = 8
hDim    = 128
cCls    = 16
k1      = 1024

acceptS1, rejectS2, acceptS3Base, classBase :: Int32
acceptS1     = 1
rejectS2     = 2
acceptS3Base = 0x40
classBase    = 0x80

-- Per-item state slot:
--   decisionPtr[i] = 0 if item i not yet decided; otherwise the int32 decision.
--   embPtr[i*E .. (i+1)*E] = embedding (only valid for items that survived to S3).

{-# NOINLINE gItems #-}
gItems :: IORef (Ptr Word8)
gItems = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE gDecisions #-}
gDecisions :: IORef (Ptr Int32)
gDecisions = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE gEmb #-}
gEmb :: IORef (Ptr Double)
gEmb = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE gN #-}
gN :: IORef Int
gN = unsafePerformIO (newIORef 0)
{-# NOINLINE gCfg #-}
gCfg :: IORef (Int32, Double)
gCfg = unsafePerformIO (newIORef (0, 0))
{-# NOINLINE gW #-}
gW :: IORef (Ptr Word32, Ptr Int16, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double)
gW = unsafePerformIO (newIORef (nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr))

readConfig :: FilePath -> IO (Int, Int32, Double, Int)
readConfig path = do
  s <- readFile path
  let kv = [(head ws, ws !! 1) | l <- lines s, let ws = words l, length ws >= 2]
  let lkup k = lookup k kv
      readD k def = maybe def read (lkup k) :: Double
      readI k def = maybe def read (lkup k) :: Int
  return (readI "N" 0, fromIntegral (readI "T2" 0) :: Int32, readD "T3" 0.0, readI "CHUNK_SIZE" 512)

readMalloc :: FilePath -> Int -> IO (Ptr Word8)
readMalloc path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return p

readWeights :: FilePath -> IO (Ptr Word32, Ptr Int16, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double)
readWeights path = do
  let sizeAccept = k1 * 4
      sizeReject = b2Slots * 2
      sizeRef    = k3 * eDim * 8
      sizeW1     = hDim * eDim * 8
      sizeB1     = hDim * 8
      sizeW2     = cCls * hDim * 8
      sizeB2     = cCls * 8
      sizeCos    = eDim * dimD * 8
      total = sizeAccept + sizeReject + sizeRef + sizeW1 + sizeB1 + sizeW2 + sizeB2 + sizeCos
  buf <- readMalloc path total
  let o1 = 0
      o2 = o1 + sizeAccept
      o3 = o2 + sizeReject
      o4 = o3 + sizeRef
      o5 = o4 + sizeW1
      o6 = o5 + sizeB1
      o7 = o6 + sizeW2
      o8 = o7 + sizeB2
  return ( castPtr (buf `plusPtr` o1)
         , castPtr (buf `plusPtr` o2)
         , castPtr (buf `plusPtr` o3)
         , castPtr (buf `plusPtr` o4)
         , castPtr (buf `plusPtr` o5)
         , castPtr (buf `plusPtr` o6)
         , castPtr (buf `plusPtr` o7)
         , castPtr (buf `plusPtr` o8)
         )


-- ---------- Stage kernels (same algo as cip_seq.hs) ----------

{-# INLINE stage1Decide #-}
stage1Decide :: Ptr Word8 -> Ptr Word32 -> IO Bool
stage1Decide !it !accept = do
  let goSig !i !acc
        | i >= dimD = return acc
        | otherwise = do !b <- peekElemOff it i; goSig (i + 1) (acc + fromIntegral b :: Word32)
  !sig0 <- goSig 0 0
  let !sig  = sig0 .&. 0xFFFF
      !slot = fromIntegral (sig .&. 0x3FF) :: Int
  !ts <- peekElemOff accept slot
  return (ts == sig)

{-# INLINE stage2Score #-}
stage2Score :: Ptr Word8 -> Ptr Int16 -> Ptr Int32 -> IO Int32
stage2Score !it !rejw !histBuf = do
  forM_ [0 .. b2Slots - 1] $ \k -> pokeElemOff histBuf k (0 :: Int32)
  let goBuild !i
        | i >= dimD - 1 = return ()
        | otherwise = do
            !x <- peekElemOff it i; !y <- peekElemOff it (i + 1)
            let !b = (fromIntegral x * 7 + fromIntegral y) .&. 0xFF :: Int
            !h <- peekElemOff histBuf b
            pokeElemOff histBuf b (h + 1); goBuild (i + 1)
  goBuild 0
  let goSum !k !acc
        | k >= b2Slots = return acc
        | otherwise = do !h <- peekElemOff histBuf k; !w <- peekElemOff rejw k
                         goSum (k + 1) (acc + h * fromIntegral w :: Int32)
  goSum 0 0

{-# INLINE stage3Embed #-}
stage3Embed :: Ptr Word8 -> Ptr Double -> Ptr Double -> IO ()
stage3Embed !it !cosT !emb = do
  forM_ [0 .. eDim - 1] $ \j -> do
    let goRow !i !acc
          | i >= dimD = return acc
          | otherwise = do
              !c <- peekElemOff cosT (j * dimD + i)
              !b <- peekElemOff it i
              goRow (i + 1) (acc + c * (fromIntegral b / 255.0))
    !s <- goRow 0 0
    pokeElemOff emb j s
  let goN2 !j !acc
        | j >= eDim = return acc
        | otherwise = do !v <- peekElemOff emb j; goN2 (j + 1) (acc + v * v)
  !n2 <- goN2 0 0
  when (n2 > 0.0) $ do
    let !inv = 1.0 / sqrt n2
    forM_ [0 .. eDim - 1] $ \j -> do
      !v <- peekElemOff emb j; pokeElemOff emb j (v * inv)

{-# INLINE stage3Best #-}
stage3Best :: Ptr Double -> Ptr Double -> IO (Int, Double)
stage3Best !emb !refV = do
  let goK !kk !bestI !bestS
        | kk >= k3 = return (bestI, bestS)
        | otherwise = do
            let goJ !j !acc
                  | j >= eDim = return acc
                  | otherwise = do
                      !e <- peekElemOff emb j; !r <- peekElemOff refV (kk * eDim + j)
                      goJ (j + 1) (acc + e * r)
            !s <- goJ 0 0
            if s > bestS then goK (kk + 1) kk s else goK (kk + 1) bestI bestS
  goK 0 0 (-1e300)

{-# INLINE stage4Classify #-}
stage4Classify :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO Int
stage4Classify !emb !w1 !b1 !w2 !b2 !hidden = do
  forM_ [0 .. hDim - 1] $ \h -> do
    !bh <- peekElemOff b1 h
    let goJ !j !acc
          | j >= eDim = return acc
          | otherwise = do
              !w <- peekElemOff w1 (h * eDim + j); !e <- peekElemOff emb j
              goJ (j + 1) (acc + w * e)
    !s <- goJ 0 bh
    pokeElemOff hidden h (if s > 0 then s else 0)
  let goC !c !bestI !bestS
        | c >= cCls = return bestI
        | otherwise = do
            !bc <- peekElemOff b2 c
            let goH !h !acc
                  | h >= hDim = return acc
                  | otherwise = do
                      !w <- peekElemOff w2 (c * hDim + h); !hv <- peekElemOff hidden h
                      goH (h + 1) (acc + w * hv)
            !s <- goH 0 bc
            if s > bestS then goC (c + 1) c s else goC (c + 1) bestI bestS
  goC 0 0 (-1e300)


-- ---------- Per-chunk per-stage workers ----------

-- runStage1 chunk_lo chunk_hi: write decision = ACCEPT_S1 for items that
-- match the stage-1 fingerprint; leave others' decision = 0 (sentinel
-- "not decided").
{-# NOINLINE runStage1 #-}
runStage1 :: Int -> Int -> ()
runStage1 !lo !hi = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  (accept, _, _, _, _, _, _, _) <- readIORef gW
  forM_ [lo .. hi - 1] $ \i -> do
    let !it = items `plusPtr` (i * dimD)
    ok <- stage1Decide it accept
    when ok $ pokeElemOff decis i acceptS1

{-# NOINLINE runStage2 #-}
runStage2 :: Int -> Int -> ()
runStage2 !lo !hi = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  (_, rejw, _, _, _, _, _, _) <- readIORef gW
  (t2, _) <- readIORef gCfg
  histBuf <- mallocBytes (b2Slots * 4) :: IO (Ptr Int32)
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !it = items `plusPtr` (i * dimD)
      !s <- stage2Score it rejw histBuf
      when (s > t2) $ pokeElemOff decis i rejectS2

{-# NOINLINE runStage3 #-}
runStage3 :: Int -> Int -> ()
runStage3 !lo !hi = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  embAll <- readIORef gEmb
  (_, _, refV, _, _, _, _, cosT) <- readIORef gW
  (_, t3) <- readIORef gCfg
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !it = items `plusPtr` (i * dimD)
          !emb = embAll `plusPtr` (i * eDim * 8)
      stage3Embed it cosT emb
      (best, bestSim) <- stage3Best emb refV
      when (bestSim > t3) $
        pokeElemOff decis i (acceptS3Base .|. fromIntegral best)

{-# NOINLINE runStage4 #-}
runStage4 :: Int -> Int -> ()
runStage4 !lo !hi = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  embAll <- readIORef gEmb
  (_, _, _, w1, b1, w2, b2, _) <- readIORef gW
  hidBuf <- mallocBytes (hDim * 8) :: IO (Ptr Double)
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !emb = embAll `plusPtr` (i * eDim * 8)
      !cls <- stage4Classify emb w1 b1 w2 b2 hidBuf
      pokeElemOff decis i (classBase .|. fromIntegral cls)


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: cip_strat DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  (n, t2, t3, chunkSize) <- readConfig (dir ++ "/config.txt")
  items <- readMalloc (dir ++ "/input.bin") (n * dimD)
  wblob <- readWeights (dir ++ "/weights.bin")
  decis <- mallocBytes (n * 4) :: IO (Ptr Int32)
  forM_ [0 .. n - 1] $ \i -> pokeElemOff decis i (0 :: Int32)
  embAll <- mallocBytes (n * eDim * 8) :: IO (Ptr Double)
  writeIORef gItems items
  writeIORef gDecisions decis
  writeIORef gEmb embAll
  writeIORef gN n
  writeIORef gCfg (t2, t3)
  writeIORef gW wblob

  let nChunks = (n + chunkSize - 1) `div` chunkSize
      chunkRanges = [ (k * chunkSize, min ((k + 1) * chunkSize) n) | k <- [0 .. nChunks - 1] ]

  t0 <- getCurrentTime
  -- Stage 1 barrier
  let sparkAll rs = let !forced  = foldl' seq () rs
                        !sparked = foldr par forced rs
                    in sparked `pseq` return ()
  -- Stage 1 barrier
  sparkAll [ runStage1 lo hi | (lo, hi) <- chunkRanges ]
  -- Stage 2 barrier
  sparkAll [ runStage2 lo hi | (lo, hi) <- chunkRanges ]
  -- Stage 3 barrier
  sparkAll [ runStage3 lo hi | (lo, hi) <- chunkRanges ]
  -- Stage 4 barrier
  sparkAll [ runStage4 lo hi | (lo, hi) <- chunkRanges ]
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  let goCs !i !acc
        | i >= n    = return ((acc :: Word32) .&. 0xFFFFFFFF)
        | otherwise = do
            !d <- peekElemOff decis i
            let !w = fromIntegral (fromIntegral d :: Word32) :: Word32
            goCs (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
