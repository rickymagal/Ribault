{-# LANGUAGE BangPatterns #-}
module Main where

-- Cascading Inference Pipeline — Strategies variant in Haskell.
--
-- Idiom: 4 stages, parList rseq per stage, strict barrier between
-- stages.  Each `withStrategy (parList rseq) (map (stageN_chunk k)
-- chunks)` forces all chunks to finish stage N before any chunk
-- starts stage N+1.  This is the natural Strategies-friendly shape
-- for a DAG with a level structure — Strategies has no native way
-- to express dependency-based per-chunk pipelining (which is what
-- Ribault's firing rule gives for free).  This asymmetry — Ribault
-- flows item-by-item through the cascade, Strategies barriers
-- between stages — is the central architectural argument we make
-- against Strategies in the paper.
--
-- Inner kernels: raw Ptr access (Word8 for items, Word32/Int16/Double
-- for weights), identical to cip_seq.hs.

import Control.Monad (forM_, when)
import Control.Parallel.Strategies (using, parList, rseq)
import Data.Bits ((.&.), (.|.), shiftR)
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

dimD, b2Slots, eDim, k3, hDim, cCls, acceptBitmapBytes, nClasses :: Int
dimD    = 256
b2Slots = 256
eDim    = 64
k3      = 8
hDim    = 128
cCls    = 16
acceptBitmapBytes = 8192
nClasses = 4

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
{-# NOINLINE gT2 #-}
gT2 :: IORef [Int32]
gT2 = unsafePerformIO (newIORef [])
{-# NOINLINE gT3 #-}
gT3 :: IORef [Double]
gT3 = unsafePerformIO (newIORef [])
{-# NOINLINE gChunkClass #-}
gChunkClass :: IORef (Ptr Int32)
gChunkClass = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE gW #-}
gW :: IORef ([Ptr Word8], Ptr Int16, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double)
gW = unsafePerformIO (newIORef ([], nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr))

readConfig :: FilePath -> IO (Int, [Int32], [Double], Int)
readConfig path = do
  s <- readFile path
  let kv = [(head ws, ws !! 1) | l <- lines s, let ws = words l, length ws >= 2]
  let lkup k = lookup k kv
      readI k def = maybe def read (lkup k) :: Int
      t2s = [ fromIntegral (readI ("T2_CLASS_" ++ show c) 0) :: Int32 | c <- [0 .. nClasses - 1] ]
      t3s = [ (read $ maybe "0.0" id (lkup ("T3_CLASS_" ++ show c))) :: Double | c <- [0 .. nClasses - 1] ]
  return (readI "N" 0, t2s, t3s, readI "CHUNK_SIZE" 32)

readMalloc :: FilePath -> Int -> IO (Ptr Word8)
readMalloc path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return p

readWeights :: FilePath -> IO ([Ptr Word8], Ptr Int16, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double)
readWeights path = do
  let sizeAcceptAll = nClasses * acceptBitmapBytes
      sizeReject = b2Slots * 2
      sizeRef    = k3 * eDim * 8
      sizeW1     = hDim * eDim * 8
      sizeB1     = hDim * 8
      sizeW2     = cCls * hDim * 8
      sizeB2     = cCls * 8
      sizeCos    = eDim * dimD * 8
      total = sizeAcceptAll + sizeReject + sizeRef + sizeW1 + sizeB1 + sizeW2 + sizeB2 + sizeCos
  buf <- readMalloc path total
  let bitmaps = [ castPtr (buf `plusPtr` (c * acceptBitmapBytes)) | c <- [0 .. nClasses - 1] ]
      o2 = sizeAcceptAll
      o3 = o2 + sizeReject
      o4 = o3 + sizeRef
      o5 = o4 + sizeW1
      o6 = o5 + sizeB1
      o7 = o6 + sizeW2
      o8 = o7 + sizeB2
  return ( bitmaps
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
stage1Decide :: Ptr Word8 -> Ptr Word8 -> IO Bool
stage1Decide !it !accept = do
  let goSig !i !acc
        | i >= dimD = return acc
        | otherwise = do !b <- peekElemOff it i; goSig (i + 1) (acc + fromIntegral b :: Word32)
  !sig0 <- goSig 0 0
  let !sig    = sig0 .&. 0xFFFF
      !byteI  = fromIntegral (sig `shiftR` 3) :: Int
      !bitOff = fromIntegral (sig .&. 7)      :: Int
  !byte <- peekElemOff accept byteI
  return (((fromIntegral byte :: Word32) `shiftR` bitOff) .&. 1 /= 0)

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
-- Per-chunk stage workers: take (lo, hi, classId).  classId selects the
-- per-source-class bitmap + T2 + T3.
{-# NOINLINE runStage1 #-}
runStage1 :: Int -> Int -> Int -> ()
runStage1 !lo !hi !classId = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  (bitmaps, _, _, _, _, _, _, _) <- readIORef gW
  let !accept = bitmaps !! classId
  forM_ [lo .. hi - 1] $ \i -> do
    let !it = items `plusPtr` (i * dimD)
    ok <- stage1Decide it accept
    when ok $ pokeElemOff decis i acceptS1

{-# NOINLINE runStage2 #-}
runStage2 :: Int -> Int -> Int -> ()
runStage2 !lo !hi !classId = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  (_, rejw, _, _, _, _, _, _) <- readIORef gW
  t2s <- readIORef gT2
  let !t2 = t2s !! classId
  histBuf <- mallocBytes (b2Slots * 4) :: IO (Ptr Int32)
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !it = items `plusPtr` (i * dimD)
      !s <- stage2Score it rejw histBuf
      when (s > t2) $ pokeElemOff decis i rejectS2

{-# NOINLINE runStage3 #-}
runStage3 :: Int -> Int -> Int -> ()
runStage3 !lo !hi !classId = unsafePerformIO $ do
  items <- readIORef gItems
  decis <- readIORef gDecisions
  embAll <- readIORef gEmb
  (_, _, refV, _, _, _, _, cosT) <- readIORef gW
  t3s <- readIORef gT3
  let !t3 = t3s !! classId
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
runStage4 :: Int -> Int -> Int -> ()
runStage4 !lo !hi !_classId = unsafePerformIO $ do
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
  (n, t2s, t3s, chunkSize) <- readConfig (dir ++ "/config.txt")
  items <- readMalloc (dir ++ "/input.bin") (n * dimD)
  wblob <- readWeights (dir ++ "/weights.bin")
  let nChunks = (n + chunkSize - 1) `div` chunkSize
  ccBuf <- readMalloc (dir ++ "/chunk_class.bin") (nChunks * 4)
  let ccPtr = castPtr ccBuf :: Ptr Int32
  decis <- mallocBytes (n * 4) :: IO (Ptr Int32)
  forM_ [0 .. n - 1] $ \i -> pokeElemOff decis i (0 :: Int32)
  embAll <- mallocBytes (n * eDim * 8) :: IO (Ptr Double)
  writeIORef gItems items
  writeIORef gDecisions decis
  writeIORef gEmb embAll
  writeIORef gN n
  writeIORef gT2 t2s
  writeIORef gT3 t3s
  writeIORef gChunkClass ccPtr
  writeIORef gW wblob

  -- Pre-read chunk_class into Haskell list of (lo, hi, classId).
  ccList <- mapM (\k -> do c <- peekElemOff ccPtr k; return (fromIntegral c :: Int)) [0 .. nChunks - 1]
  let chunkInfo = zipWith (\k cid -> (k * chunkSize, min ((k + 1) * chunkSize) n, cid)) [0 ..] ccList

  t0 <- getCurrentTime
  -- Stage 1 barrier
  let !r1 = map (\(lo, hi, cid) -> runStage1 lo hi cid) chunkInfo `using` parList rseq
  foldl' seq () r1 `seq` return ()
  let !r2 = map (\(lo, hi, cid) -> runStage2 lo hi cid) chunkInfo `using` parList rseq
  foldl' seq () r2 `seq` return ()
  let !r3 = map (\(lo, hi, cid) -> runStage3 lo hi cid) chunkInfo `using` parList rseq
  foldl' seq () r3 `seq` return ()
  let !r4 = map (\(lo, hi, cid) -> runStage4 lo hi cid) chunkInfo `using` parList rseq
  foldl' seq () r4 `seq` return ()
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
