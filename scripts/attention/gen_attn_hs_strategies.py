#!/usr/bin/env python3
"""Generate standalone GHC Strategies Haskell program for the full
transformer-block attention end-to-end. Storage: IOVector Storable Double.
Two phases with parList rseq parallelism over K row-blocks; barrier
between phases via NF forcing.
"""

import argparse, os


HS_TEMPLATE = r"""{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forM_)
import Control.Parallel.Strategies (using, parList, rseq)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

attnN, attnD, attnHeads, attnHeadDim, attnDff, attnVocab, attnBlocks :: Int
attnN       = __N__
attnD       = __D__
attnHeads   = __N_HEADS__
attnHeadDim = __HEAD_DIM__
attnDff     = __D_FF__
attnVocab   = __VOCAB__
attnBlocks  = __N_BLOCKS__

dataDir :: FilePath
dataDir = "__DATA_DIR__"

-- Top-level IORefs holding all shared state (filled by main, read by phase funcs)
{-# NOINLINE g_e #-}
g_e :: IORef (MV.IOVector Double)
g_e = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_pe #-}
g_pe :: IORef (MV.IOVector Double)
g_pe = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wq #-}
g_wq :: IORef (MV.IOVector Double)
g_wq = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wk #-}
g_wk :: IORef (MV.IOVector Double)
g_wk = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wv #-}
g_wv :: IORef (MV.IOVector Double)
g_wv = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wo #-}
g_wo :: IORef (MV.IOVector Double)
g_wo = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_w1 #-}
g_w1 :: IORef (MV.IOVector Double)
g_w1 = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_w2 #-}
g_w2 :: IORef (MV.IOVector Double)
g_w2 = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wu #-}
g_wu :: IORef (MV.IOVector Double)
g_wu = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln1w #-}
g_ln1w :: IORef (MV.IOVector Double)
g_ln1w = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln1b #-}
g_ln1b :: IORef (MV.IOVector Double)
g_ln1b = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln2w #-}
g_ln2w :: IORef (MV.IOVector Double)
g_ln2w = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln2b #-}
g_ln2b :: IORef (MV.IOVector Double)
g_ln2b = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_x #-}
g_x :: IORef (MV.IOVector Double)
g_x = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_xa #-}
g_xa :: IORef (MV.IOVector Double)
g_xa = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_q #-}
g_q :: IORef (MV.IOVector Double)
g_q = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_k #-}
g_k :: IORef (MV.IOVector Double)
g_k = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_v #-}
g_v :: IORef (MV.IOVector Double)
g_v = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_attn #-}
g_attn :: IORef (MV.IOVector Double)
g_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_xb #-}
g_xb :: IORef (MV.IOVector Double)
g_xb = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ffnh #-}
g_ffnh :: IORef (MV.IOVector Double)
g_ffnh = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_logits #-}
g_logits :: IORef (MV.IOVector Double)
g_logits = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_inputT #-}
g_inputT :: IORef (MV.IOVector Word8)
g_inputT = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_outputT #-}
g_outputT :: IORef (MV.IOVector Word8)
g_outputT = unsafePerformIO (newIORef =<< MV.replicate 0 0)

readDoubles :: FilePath -> Int -> IO (MV.IOVector Double)
readDoubles path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  v <- MV.replicate count 0.0
  withForeignPtr fp $ \src -> do
    let srcP = src
    forM_ [0..count-1] $ \i -> do
      !d <- peek (srcP `plusPtr` (i*8) :: Ptr Double)
      MV.write v i d
  return v

readBytes :: FilePath -> Int -> IO (MV.IOVector Word8)
readBytes path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  v <- MV.replicate count 0
  withForeignPtr fp $ \src -> do
    forM_ [0..count-1] $ \i -> do
      !w <- peek (src `plusPtr` i :: Ptr Word8)
      MV.write v i w
  return v

rowLo, rowHi :: Int -> Int
rowLo b = b * attnN `div` attnBlocks
rowHi b = if b == attnBlocks - 1 then attnN else (b + 1) * attnN `div` attnBlocks

matmulBlock :: Int -> Int -> MV.IOVector Double -> MV.IOVector Double
            -> MV.IOVector Double -> Int -> Int -> IO ()
matmulBlock !lo !hi !a !b !c !kDim !nDim = do
  forM_ [lo..hi-1] $ \m -> do
    forM_ [0..nDim-1] $ \n -> MV.write c (m*nDim + n) 0.0
    forM_ [0..kDim-1] $ \k -> do
      !av <- MV.read a (m*kDim + k)
      forM_ [0..nDim-1] $ \n -> do
        !bv <- MV.read b (k*nDim + n)
        !cv <- MV.read c (m*nDim + n)
        MV.write c (m*nDim + n) (cv + av*bv)

layerNormBlock :: Int -> Int -> MV.IOVector Double -> MV.IOVector Double
               -> MV.IOVector Double -> MV.IOVector Double -> Int -> IO ()
layerNormBlock !lo !hi !x !w !b !out !dim = do
  let eps = 1e-5
  forM_ [lo..hi-1] $ \i -> do
    !mu <- meanOver x (i*dim) dim
    !var <- varOver x (i*dim) dim mu
    let !inv = 1.0 / sqrt (var + eps)
    forM_ [0..dim-1] $ \j -> do
      !v <- MV.read x (i*dim + j)
      !wj <- MV.read w j
      !bj <- MV.read b j
      MV.write out (i*dim + j) ((v - mu) * inv * wj + bj)
  where
    meanOver !v !base !d = do
      let go !i !acc
            | i >= d    = return (acc / fromIntegral d)
            | otherwise = do
                !x' <- MV.read v (base + i)
                go (i+1) (acc + x')
      go 0 0.0
    varOver !v !base !d !mu = do
      let go !i !acc
            | i >= d    = return (acc / fromIntegral d)
            | otherwise = do
                !x' <- MV.read v (base + i)
                let !dd = x' - mu
                go (i+1) (acc + dd*dd)
      go 0 0.0

{-# NOINLINE runPhaseA #-}
runPhaseA :: Int -> ()
runPhaseA !b = unsafePerformIO $ do
  inputT <- readIORef g_inputT
  e      <- readIORef g_e
  pe     <- readIORef g_pe
  ln1w   <- readIORef g_ln1w
  ln1b   <- readIORef g_ln1b
  wQ     <- readIORef g_wq
  wK     <- readIORef g_wk
  wV     <- readIORef g_wv
  xBuf   <- readIORef g_x
  xaBuf  <- readIORef g_xa
  qBuf   <- readIORef g_q
  kBuf   <- readIORef g_k
  vBuf   <- readIORef g_v
  let !lo = rowLo b
      !hi = rowHi b
  forM_ [lo..hi-1] $ \i -> do
    !tok <- fromIntegral <$> MV.read inputT i
    forM_ [0..attnD-1] $ \j -> do
      !ev <- MV.read e (tok*attnD + j)
      !pv <- MV.read pe (i*attnD + j)
      MV.write xBuf (i*attnD + j) (ev + pv)
  layerNormBlock lo hi xBuf ln1w ln1b xaBuf attnD
  matmulBlock lo hi xaBuf wQ qBuf attnD attnD
  matmulBlock lo hi xaBuf wK kBuf attnD attnD
  matmulBlock lo hi xaBuf wV vBuf attnD attnD

{-# NOINLINE runPhaseB #-}
runPhaseB :: Int -> ()
runPhaseB !b = unsafePerformIO $ do
  qBuf   <- readIORef g_q
  kBuf   <- readIORef g_k
  vBuf   <- readIORef g_v
  attnB  <- readIORef g_attn
  xBuf   <- readIORef g_x
  xaBuf  <- readIORef g_xa
  xbBuf  <- readIORef g_xb
  ln2w   <- readIORef g_ln2w
  ln2b   <- readIORef g_ln2b
  wO     <- readIORef g_wo
  w1     <- readIORef g_w1
  w2     <- readIORef g_w2
  wU     <- readIORef g_wu
  ffnH   <- readIORef g_ffnh
  logits <- readIORef g_logits
  outputT<- readIORef g_outputT
  let !lo = rowLo b
      !hi = rowHi b
      !inv = 1.0 / sqrt (fromIntegral attnHeadDim)
  scores <- MV.replicate attnN 0.0
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnD-1] $ \j -> MV.write attnB (i*attnD + j) 0.0
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnHeads-1] $ \h -> do
      forM_ [0..attnN-1] $ \j -> do
        let go !k !acc
              | k >= attnHeadDim = return acc
              | otherwise = do
                  !q <- MV.read qBuf (i*attnD + h*attnHeadDim + k)
                  !kv <- MV.read kBuf (j*attnD + h*attnHeadDim + k)
                  go (k+1) (acc + q*kv)
        !s <- go 0 0.0
        MV.write scores j (s * inv)
      !mx0 <- MV.read scores 0
      let goMax !i !m
            | i >= attnN = return m
            | otherwise = do
                !v <- MV.read scores i
                goMax (i+1) (max m v)
      !mx <- goMax 1 mx0
      let goSum !i !acc
            | i >= attnN = return acc
            | otherwise = do
                !v <- MV.read scores i
                let !ev = exp (v - mx)
                MV.write scores i ev
                goSum (i+1) (acc + ev)
      !sm <- goSum 0 0.0
      forM_ [0..attnN-1] $ \j -> do
        !v <- MV.read scores j
        MV.write scores j (v / sm)
      forM_ [0..attnN-1] $ \j -> do
        !a <- MV.read scores j
        forM_ [0..attnHeadDim-1] $ \k -> do
          !vv <- MV.read vBuf (j*attnD + h*attnHeadDim + k)
          !cur <- MV.read attnB (i*attnD + h*attnHeadDim + k)
          MV.write attnB (i*attnD + h*attnHeadDim + k) (cur + a*vv)
  matmulBlock lo hi attnB wO xaBuf attnD attnD
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnD-1] $ \j -> do
      !xv <- MV.read xBuf (i*attnD + j)
      !yv <- MV.read xaBuf (i*attnD + j)
      MV.write xBuf (i*attnD + j) (xv + yv)
  layerNormBlock lo hi xBuf ln2w ln2b xbBuf attnD
  matmulBlock lo hi xbBuf w1 ffnH attnD attnDff
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnDff-1] $ \j -> do
      !v <- MV.read ffnH (i*attnDff + j)
      MV.write ffnH (i*attnDff + j) (if v < 0 then 0 else v)
  matmulBlock lo hi ffnH w2 xaBuf attnDff attnD
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnD-1] $ \j -> do
      !xv <- MV.read xBuf (i*attnD + j)
      !yv <- MV.read xaBuf (i*attnD + j)
      MV.write xBuf (i*attnD + j) (xv + yv)
  matmulBlock lo hi xBuf wU logits attnD attnVocab
  forM_ [lo..hi-1] $ \i -> do
    !l0 <- MV.read logits (i*attnVocab + 0)
    let goArg !v !bestV !bestVal
          | v >= attnVocab = return bestV
          | otherwise = do
              !lv <- MV.read logits (i*attnVocab + v)
              if lv > bestVal then goArg (v+1) v lv
                              else goArg (v+1) bestV bestVal
    !best <- goArg 1 0 l0
    MV.write outputT i (fromIntegral best :: Word8)

main :: IO ()
main = do
  e      <- readDoubles (dataDir ++ "/E.bin")      (attnVocab * attnD)
  peM    <- readDoubles (dataDir ++ "/PE.bin")     (attnN     * attnD)
  wQ     <- readDoubles (dataDir ++ "/W_Q.bin")    (attnD     * attnD)
  wK     <- readDoubles (dataDir ++ "/W_K.bin")    (attnD     * attnD)
  wV     <- readDoubles (dataDir ++ "/W_V.bin")    (attnD     * attnD)
  wO     <- readDoubles (dataDir ++ "/W_O.bin")    (attnD     * attnD)
  w1     <- readDoubles (dataDir ++ "/W_1.bin")    (attnD     * attnDff)
  w2     <- readDoubles (dataDir ++ "/W_2.bin")    (attnDff   * attnD)
  wU     <- readDoubles (dataDir ++ "/W_U.bin")    (attnD     * attnVocab)
  ln1w   <- readDoubles (dataDir ++ "/LN_1_w.bin") attnD
  ln1b   <- readDoubles (dataDir ++ "/LN_1_b.bin") attnD
  ln2w   <- readDoubles (dataDir ++ "/LN_2_w.bin") attnD
  ln2b   <- readDoubles (dataDir ++ "/LN_2_b.bin") attnD
  inputT <- readBytes   (dataDir ++ "/input_tokens.bin") attnN
  outputT<- MV.replicate attnN (0 :: Word8)

  xBuf   <- MV.replicate (attnN * attnD) 0.0
  xaBuf  <- MV.replicate (attnN * attnD) 0.0
  qBuf   <- MV.replicate (attnN * attnD) 0.0
  kBuf   <- MV.replicate (attnN * attnD) 0.0
  vBuf   <- MV.replicate (attnN * attnD) 0.0
  attnB  <- MV.replicate (attnN * attnD) 0.0
  xbBuf  <- MV.replicate (attnN * attnD) 0.0
  ffnH   <- MV.replicate (attnN * attnDff) 0.0
  logits <- MV.replicate (attnN * attnVocab) 0.0

  writeIORef g_e e; writeIORef g_pe peM
  writeIORef g_wq wQ; writeIORef g_wk wK; writeIORef g_wv wV; writeIORef g_wo wO
  writeIORef g_w1 w1; writeIORef g_w2 w2; writeIORef g_wu wU
  writeIORef g_ln1w ln1w; writeIORef g_ln1b ln1b
  writeIORef g_ln2w ln2w; writeIORef g_ln2b ln2b
  writeIORef g_x xBuf; writeIORef g_xa xaBuf
  writeIORef g_q qBuf; writeIORef g_k kBuf; writeIORef g_v vBuf
  writeIORef g_attn attnB; writeIORef g_xb xbBuf
  writeIORef g_ffnh ffnH; writeIORef g_logits logits
  writeIORef g_inputT inputT; writeIORef g_outputT outputT

  t0 <- getCurrentTime

  let !rA = map runPhaseA [0..attnBlocks-1] `using` parList rseq
  foldl' seq () rA `seq` return ()

  let !rB = map runPhaseB [0..attnBlocks-1] `using` parList rseq
  foldl' seq () rB `seq` return ()

  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  -- Checksum
  let goCs !i !acc
        | i >= attnN = return ((acc :: Word) .&. 0xFFFFFFFF)
        | otherwise = do
            !w <- MV.read outputT i
            goCs (i+1) (acc + fromIntegral (i+1) * fromIntegral w)
  !cs <- goCs 0 0
  putStrLn $ "CHECKSUM=" ++ show cs
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
"""


def emit(out_path, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
    os.makedirs(os.path.dirname(out_path) or ".", exist_ok=True)
    src = (HS_TEMPLATE
           .replace("__N__", str(N))
           .replace("__D__", str(D))
           .replace("__N_HEADS__", str(n_heads))
           .replace("__HEAD_DIM__", str(head_dim))
           .replace("__D_FF__", str(d_ff))
           .replace("__VOCAB__", str(vocab))
           .replace("__N_BLOCKS__", str(n_blocks))
           .replace("__DATA_DIR__", data_dir))
    with open(out_path, "w") as f:
        f.write(src)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()

    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
