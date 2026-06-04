{-# LANGUAGE BangPatterns #-}
module Main where

-- Cascading Inference Pipeline sequential baseline in Haskell.
-- Mirrors cip_seq.c byte-for-byte algorithm.  Uses raw `Ptr Word8` for
-- input items and `Ptr` accessors via peekElemOff/pokeElemOff on every
-- inner loop — the same idiom used by the parallel Haskell variants
-- (cip_strat.hs, cip_parpseq.hs, and the Ribault-Hs supers emitted by
-- gen_cip_hs.py).  Same fairness principle as ms_seq.hs / sc_seq.hs.

import Control.Monad (forM_, when)
import Data.Bits ((.&.), shiftR, (.|.))
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)

dimD, b2Slots, eDim, k3, hDim, cCls, acceptBitmapBytes :: Int
dimD              = 256
b2Slots           = 256
eDim              = 64
k3                = 8
hDim              = 128
cCls              = 16
acceptBitmapBytes = 8192

acceptS1, rejectS2, acceptS3Base, classBase :: Int32
acceptS1     = 1
rejectS2     = 2
acceptS3Base = 0x40
classBase    = 0x80


-- ---------- Config ----------

data Config = Config { cfgN :: !Int, cfgT2 :: !Int32, cfgT3 :: !Double }

readConfig :: FilePath -> IO Config
readConfig path = do
  s <- readFile path
  let kv = [(head ws, ws !! 1) | l <- lines s, let ws = words l, length ws >= 2]
  let getInt k def = maybe def read (lookup k kv)
      getDouble k def = maybe def read (lookup k kv) :: Double
  let n  = getInt "N" 0
      t2 = (read $ maybe "0" id (lookup "T2" kv)) :: Int32
      t3 = getDouble "T3" 0.0
  return (Config n t2 t3)


-- ---------- Weight blob ----------

data Weights = Weights
  { wAccept :: !(Ptr Word8)
  , wReject :: !(Ptr Int16)
  , wRefVec :: !(Ptr Double)
  , wW1     :: !(Ptr Double)
  , wB1     :: !(Ptr Double)
  , wW2     :: !(Ptr Double)
  , wB2     :: !(Ptr Double)
  , wCos    :: !(Ptr Double)
  }

readWeights :: FilePath -> IO Weights
readWeights path = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  -- We copy from the BS into a long-lived malloc'd buffer so the Ptrs
  -- below stay valid past the function scope.
  withForeignPtr fp $ \src -> do
    let off0 = 0
        sizeAccept = acceptBitmapBytes
        sizeReject = b2Slots * 2
        sizeRef    = k3 * eDim * 8
        sizeW1     = hDim * eDim * 8
        sizeB1     = hDim * 8
        sizeW2     = cCls * hDim * 8
        sizeB2     = cCls * 8
        sizeCos    = eDim * dimD * 8
        total = sizeAccept + sizeReject + sizeRef + sizeW1 + sizeB1 + sizeW2 + sizeB2 + sizeCos
    buf <- mallocBytes total :: IO (Ptr Word8)
    copyBytes buf src total
    let o1 = off0
        o2 = o1 + sizeAccept
        o3 = o2 + sizeReject
        o4 = o3 + sizeRef
        o5 = o4 + sizeW1
        o6 = o5 + sizeB1
        o7 = o6 + sizeW2
        o8 = o7 + sizeB2
    return $ Weights
      { wAccept = castPtr (buf `plusPtr` o1)
      , wReject = castPtr (buf `plusPtr` o2)
      , wRefVec = castPtr (buf `plusPtr` o3)
      , wW1     = castPtr (buf `plusPtr` o4)
      , wB1     = castPtr (buf `plusPtr` o5)
      , wW2     = castPtr (buf `plusPtr` o6)
      , wB2     = castPtr (buf `plusPtr` o7)
      , wCos    = castPtr (buf `plusPtr` o8)
      }


-- ---------- Stage kernels ----------

{-# INLINE stage1Decide #-}
stage1Decide :: Ptr Word8 -> Ptr Word8 -> IO Bool
stage1Decide !it !accept = do
  let goSig !i !acc
        | i >= dimD = return acc
        | otherwise = do
            !b <- peekElemOff it i
            goSig (i + 1) (acc + fromIntegral b :: Word32)
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
            !x <- peekElemOff it i
            !y <- peekElemOff it (i + 1)
            let !b = (fromIntegral x * 7 + fromIntegral y) .&. 0xFF :: Int
            !h <- peekElemOff histBuf b
            pokeElemOff histBuf b (h + 1)
            goBuild (i + 1)
  goBuild 0
  let goSum !k !acc
        | k >= b2Slots = return acc
        | otherwise = do
            !h <- peekElemOff histBuf k
            !w <- peekElemOff rejw k
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
                      !e <- peekElemOff emb j
                      !r <- peekElemOff refV (kk * eDim + j)
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
              !w <- peekElemOff w1 (h * eDim + j)
              !e <- peekElemOff emb j
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
                      !w <- peekElemOff w2 (c * hDim + h)
                      !hv <- peekElemOff hidden h
                      goH (h + 1) (acc + w * hv)
            !s <- goH 0 bc
            if s > bestS then goC (c + 1) c s else goC (c + 1) bestI bestS
  goC 0 0 (-1e300)


-- decide_item: returns the i32 decision for one item.
{-# INLINE decideItem #-}
decideItem :: Weights -> Int32 -> Double
           -> Ptr Word8 -> Ptr Int32 -> Ptr Double -> Ptr Double -> IO Int32
decideItem !w !t2 !t3 !it !histBuf !embBuf !hidBuf = do
  !ok1 <- stage1Decide it (wAccept w)
  if ok1
    then return acceptS1
    else do
      !s2 <- stage2Score it (wReject w) histBuf
      if s2 > t2
        then return rejectS2
        else do
          stage3Embed it (wCos w) embBuf
          (best, bestSim) <- stage3Best embBuf (wRefVec w)
          if bestSim > t3
            then return (acceptS3Base .|. fromIntegral best :: Int32)
            else do
              !cls <- stage4Classify embBuf (wW1 w) (wB1 w) (wW2 w) (wB2 w) hidBuf
              return (classBase .|. fromIntegral cls :: Int32)


-- ---------- Main ----------

readInput :: FilePath -> Int -> IO (Ptr Word8)
readInput path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return p

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: cip_seq_hs DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  cfg <- readConfig (dir ++ "/config.txt")
  w   <- readWeights (dir ++ "/weights.bin")
  items <- readInput (dir ++ "/input.bin") (cfgN cfg * dimD)
  histBuf <- mallocBytes (b2Slots * 4) :: IO (Ptr Int32)
  embBuf  <- mallocBytes (eDim * 8)    :: IO (Ptr Double)
  hidBuf  <- mallocBytes (hDim * 8)    :: IO (Ptr Double)

  t0 <- getCurrentTime
  let goItems !i !acc
        | i >= cfgN cfg = return ((acc :: Word32) .&. 0xFFFFFFFF)
        | otherwise = do
            let !it = items `plusPtr` (i * dimD)
            !d <- decideItem w (cfgT2 cfg) (cfgT3 cfg) it histBuf embBuf hidBuf
            let !w32 = fromIntegral (fromIntegral d :: Word32) :: Word32
            goItems (i + 1) ((acc + w32) .&. 0xFFFFFFFF)
  !cs <- goItems 0 0
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show cs)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
