#!/usr/bin/env python3
"""Generate Ribault Cascading Inference Pipeline files with Haskell-implemented supers.

Mirror of gen_cip_c.py / gen_cip_rust.py with Haskell super bodies using
raw `Ptr Word8` for items and raw `Ptr` accessors for weights — same
fairness idiom as cip_seq.hs / cip_strat.hs / cip_parpseq.hs.
"""

import argparse, os, struct


SUPER_INIT   = 10
SUPER_STAGE1 = 11
SUPER_STAGE2 = 12
SUPER_STAGE3 = 13
SUPER_STAGE4 = 14
SUPER_SYNC   = 15
SUPER_OUTPUT = 16
MAX_FANIN    = 30


HSK_TEMPLATE = """-- Auto-generated attn.hsk — declaration order REVERSE of super-number
-- assignment so that output->s16 sync->s15 stage4->s14 ... init->s10.

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO cipOutput
  )

sync_super c d =
  super syncImpl c d (
    syncImpl c d = unsafePerformIO cipSync
  )

stage4_super cid d =
  super stage4Impl cid d (
    stage4Impl cid d = unsafePerformIO (cipStage4 (fromIntegral cid))
  )

stage3_super cid d =
  super stage3Impl cid d (
    stage3Impl cid d = unsafePerformIO (cipStage3 (fromIntegral cid))
  )

stage2_super cid d =
  super stage2Impl cid d (
    stage2Impl cid d = unsafePerformIO (cipStage2 (fromIntegral cid))
  )

stage1_super cid d =
  super stage1Impl cid d (
    stage1Impl cid d = unsafePerformIO (cipStage1 (fromIntegral cid))
  )

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO cipInit
  )

main =
  let s = init_super 0
      a = stage1_super 0 s
      b = stage2_super 0 a
      c = stage3_super 0 b
      d = stage4_super 0 c
      e = sync_super  0 d
  in output_super e
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.), (.|.), shiftR)
import Control.Monad (forM_, when)

cipN, cipChunkSize :: Int
cipN         = __N__
cipChunkSize = __CHUNK_SIZE__

cipD, cipB2, cipE, cipK3, cipH, cipC, cipAcceptBitmapBytes, cipNClasses :: Int
cipD                  = 256
cipB2                 = 256
cipE                  = 64
cipK3                 = 8
cipH                  = 128
cipC                  = 16
cipAcceptBitmapBytes  = 8192
cipNClasses           = 4

cipDataDir :: FilePath
cipDataDir = "__DATA_DIR__"

cipAcceptS1, cipRejectS2, cipAcceptS3Base, cipClassBase :: Int32
cipAcceptS1     = 1
cipRejectS2     = 2
cipAcceptS3Base = 0x40
cipClassBase    = 0x80

{-# NOINLINE g_items #-}
g_items :: IORef (Ptr Word8)
g_items = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_decis #-}
g_decis :: IORef (Ptr Int32)
g_decis = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_emb #-}
g_emb :: IORef (Ptr Double)
g_emb = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_w #-}
g_w :: IORef ([Ptr Word8], Ptr Int16, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double, Ptr Double)
g_w = unsafePerformIO (newIORef ([], nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr, nullPtr))
{-# NOINLINE g_t2 #-}
g_t2 :: IORef [Int32]
g_t2 = unsafePerformIO (newIORef [])
{-# NOINLINE g_t3 #-}
g_t3 :: IORef [Double]
g_t3 = unsafePerformIO (newIORef [])
{-# NOINLINE g_chunk_class #-}
g_chunk_class :: IORef (Ptr Int32)
g_chunk_class = unsafePerformIO (newIORef nullPtr)

cipReadMalloc :: FilePath -> Int -> IO (Ptr Word8)
cipReadMalloc path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return p

cipReadConfig :: IO ()
cipReadConfig = do
  s <- readFile (cipDataDir ++ "/config.txt")
  let kv = [(head ws, ws !! 1) | l <- lines s, let ws = words l, length ws >= 2]
      t2s = [ (read $ maybe "0" id (lookup ("T2_CLASS_" ++ show c) kv)) :: Int32 | c <- [0 .. cipNClasses - 1] ]
      t3s = [ (read $ maybe "0.0" id (lookup ("T3_CLASS_" ++ show c) kv)) :: Double | c <- [0 .. cipNClasses - 1] ]
  writeIORef g_t2 t2s
  writeIORef g_t3 t3s

cipReadWeights :: IO ()
cipReadWeights = do
  let sizeAcceptAll = cipNClasses * cipAcceptBitmapBytes
      sizeReject = cipB2 * 2
      sizeRef    = cipK3 * cipE * 8
      sizeW1     = cipH * cipE * 8
      sizeB1     = cipH * 8
      sizeW2     = cipC * cipH * 8
      sizeB2     = cipC * 8
      sizeCos    = cipE * cipD * 8
      total = sizeAcceptAll + sizeReject + sizeRef + sizeW1 + sizeB1 + sizeW2 + sizeB2 + sizeCos
  buf <- cipReadMalloc (cipDataDir ++ "/weights.bin") total
  let bitmaps = [ castPtr (buf `plusPtr` (c * cipAcceptBitmapBytes)) | c <- [0 .. cipNClasses - 1] ]
      o2 = sizeAcceptAll
      o3 = o2 + sizeReject
      o4 = o3 + sizeRef
      o5 = o4 + sizeW1
      o6 = o5 + sizeB1
      o7 = o6 + sizeW2
      o8 = o7 + sizeB2
  writeIORef g_w
    ( bitmaps
    , castPtr (buf `plusPtr` o2)
    , castPtr (buf `plusPtr` o3)
    , castPtr (buf `plusPtr` o4)
    , castPtr (buf `plusPtr` o5)
    , castPtr (buf `plusPtr` o6)
    , castPtr (buf `plusPtr` o7)
    , castPtr (buf `plusPtr` o8)
    )

cipInit :: IO Int64
cipInit = do
  cipReadConfig
  cipReadWeights
  items <- cipReadMalloc (cipDataDir ++ "/input.bin") (cipN * cipD)
  writeIORef g_items items
  let !nChunks = (cipN + cipChunkSize - 1) `div` cipChunkSize
  ccBuf <- cipReadMalloc (cipDataDir ++ "/chunk_class.bin") (nChunks * 4)
  writeIORef g_chunk_class (castPtr ccBuf)
  decis <- mallocBytes (cipN * 4) :: IO (Ptr Int32)
  forM_ [0 .. cipN - 1] $ \i -> pokeElemOff decis i (0 :: Int32)
  writeIORef g_decis decis
  embAll <- mallocBytes (cipN * cipE * 8) :: IO (Ptr Double)
  writeIORef g_emb embAll
  return 0


{-# INLINE cipStage1Decide #-}
cipStage1Decide :: Ptr Word8 -> Ptr Word8 -> IO Bool
cipStage1Decide !it !accept = do
  let goSig !i !acc
        | i >= cipD = return acc
        | otherwise = do !b <- peekElemOff it i; goSig (i+1) (acc + fromIntegral b :: Word32)
  !sig0 <- goSig 0 0
  let !sig    = sig0 .&. 0xFFFF
      !byteI  = fromIntegral (sig `shiftR` 3) :: Int
      !bitOff = fromIntegral (sig .&. 7)      :: Int
  !byte <- peekElemOff accept byteI
  return (((fromIntegral byte :: Word32) `shiftR` bitOff) .&. 1 /= 0)

{-# INLINE cipStage2Score #-}
cipStage2Score :: Ptr Word8 -> Ptr Int16 -> Ptr Int32 -> IO Int32
cipStage2Score !it !rejw !histBuf = do
  forM_ [0 .. cipB2 - 1] $ \k -> pokeElemOff histBuf k (0 :: Int32)
  let goBuild !i
        | i >= cipD - 1 = return ()
        | otherwise = do
            !x <- peekElemOff it i; !y <- peekElemOff it (i+1)
            let !b = (fromIntegral x * 7 + fromIntegral y) .&. 0xFF :: Int
            !h <- peekElemOff histBuf b
            pokeElemOff histBuf b (h + 1); goBuild (i + 1)
  goBuild 0
  let goSum !k !acc
        | k >= cipB2 = return acc
        | otherwise = do !h <- peekElemOff histBuf k; !w <- peekElemOff rejw k
                         goSum (k + 1) (acc + h * fromIntegral w :: Int32)
  goSum 0 0

{-# INLINE cipStage3Embed #-}
cipStage3Embed :: Ptr Word8 -> Ptr Double -> Ptr Double -> IO ()
cipStage3Embed !it !cosT !emb = do
  forM_ [0 .. cipE - 1] $ \j -> do
    let goRow !i !acc
          | i >= cipD = return acc
          | otherwise = do
              !c <- peekElemOff cosT (j * cipD + i)
              !b <- peekElemOff it i
              goRow (i + 1) (acc + c * (fromIntegral b / 255.0))
    !s <- goRow 0 0
    pokeElemOff emb j s
  let goN2 !j !acc
        | j >= cipE = return acc
        | otherwise = do !v <- peekElemOff emb j; goN2 (j+1) (acc + v*v)
  !n2 <- goN2 0 0
  when (n2 > 0.0) $ do
    let !inv = 1.0 / sqrt n2
    forM_ [0 .. cipE - 1] $ \j -> do !v <- peekElemOff emb j; pokeElemOff emb j (v * inv)

{-# INLINE cipStage3Best #-}
cipStage3Best :: Ptr Double -> Ptr Double -> IO (Int, Double)
cipStage3Best !emb !refV = do
  let goK !kk !bestI !bestS
        | kk >= cipK3 = return (bestI, bestS)
        | otherwise = do
            let goJ !j !acc
                  | j >= cipE = return acc
                  | otherwise = do !e <- peekElemOff emb j; !r <- peekElemOff refV (kk*cipE + j)
                                   goJ (j+1) (acc + e*r)
            !s <- goJ 0 0
            if s > bestS then goK (kk+1) kk s else goK (kk+1) bestI bestS
  goK 0 0 (-1e300)

{-# INLINE cipStage4Classify #-}
cipStage4Classify :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO Int
cipStage4Classify !emb !w1 !b1 !w2 !b2 !hidden = do
  forM_ [0 .. cipH - 1] $ \h -> do
    !bh <- peekElemOff b1 h
    let goJ !j !acc
          | j >= cipE = return acc
          | otherwise = do !w <- peekElemOff w1 (h*cipE + j); !e <- peekElemOff emb j
                           goJ (j+1) (acc + w*e)
    !s <- goJ 0 bh
    pokeElemOff hidden h (if s > 0 then s else 0)
  let goC !c !bestI !bestS
        | c >= cipC = return bestI
        | otherwise = do
            !bc <- peekElemOff b2 c
            let goH !h !acc
                  | h >= cipH = return acc
                  | otherwise = do !w <- peekElemOff w2 (c*cipH + h); !hv <- peekElemOff hidden h
                                   goH (h+1) (acc + w*hv)
            !s <- goH 0 bc
            if s > bestS then goC (c+1) c s else goC (c+1) bestI bestS
  goC 0 0 (-1e300)


cipChunkClass :: Int -> IO Int
cipChunkClass !cid = do
  ccPtr <- readIORef g_chunk_class
  !c <- peekElemOff ccPtr cid
  return (fromIntegral c :: Int)

cipStage1 :: Int -> IO Int64
cipStage1 !cid = do
  items <- readIORef g_items
  decis <- readIORef g_decis
  (bitmaps, _, _, _, _, _, _, _) <- readIORef g_w
  classId <- cipChunkClass cid
  let !accept = bitmaps !! classId
      !lo = cid * cipChunkSize
      !hi = if lo + cipChunkSize > cipN then cipN else lo + cipChunkSize
  forM_ [lo .. hi - 1] $ \i -> do
    let !it = items `plusPtr` (i * cipD)
    ok <- cipStage1Decide it accept
    when ok $ pokeElemOff decis i cipAcceptS1
  return 0

cipStage2 :: Int -> IO Int64
cipStage2 !cid = do
  items <- readIORef g_items
  decis <- readIORef g_decis
  (_, rejw, _, _, _, _, _, _) <- readIORef g_w
  t2s <- readIORef g_t2
  classId <- cipChunkClass cid
  let !t2 = t2s !! classId
  histBuf <- mallocBytes (cipB2 * 4) :: IO (Ptr Int32)
  let !lo = cid * cipChunkSize
      !hi = if lo + cipChunkSize > cipN then cipN else lo + cipChunkSize
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !it = items `plusPtr` (i * cipD)
      !s <- cipStage2Score it rejw histBuf
      when (s > t2) $ pokeElemOff decis i cipRejectS2
  return 0

cipStage3 :: Int -> IO Int64
cipStage3 !cid = do
  items <- readIORef g_items
  decis <- readIORef g_decis
  embAll <- readIORef g_emb
  (_, _, refV, _, _, _, _, cosT) <- readIORef g_w
  t3s <- readIORef g_t3
  classId <- cipChunkClass cid
  let !t3 = t3s !! classId
      !lo = cid * cipChunkSize
      !hi = if lo + cipChunkSize > cipN then cipN else lo + cipChunkSize
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !it  = items `plusPtr` (i * cipD)
          !emb = embAll `plusPtr` (i * cipE * 8)
      cipStage3Embed it cosT emb
      (best, bs) <- cipStage3Best emb refV
      when (bs > t3) $ pokeElemOff decis i (cipAcceptS3Base .|. fromIntegral best)
  return 0

cipStage4 :: Int -> IO Int64
cipStage4 !cid = do
  items <- readIORef g_items
  decis <- readIORef g_decis
  embAll <- readIORef g_emb
  (_, _, _, w1, b1, w2, b2, _) <- readIORef g_w
  hidBuf <- mallocBytes (cipH * 8) :: IO (Ptr Double)
  let !lo = cid * cipChunkSize
      !hi = if lo + cipChunkSize > cipN then cipN else lo + cipChunkSize
  forM_ [lo .. hi - 1] $ \i -> do
    !d <- peekElemOff decis i
    when (d == 0) $ do
      let !emb = embAll `plusPtr` (i * cipE * 8)
      !cls <- cipStage4Classify emb w1 b1 w2 b2 hidBuf
      pokeElemOff decis i (cipClassBase .|. fromIntegral cls)
  return 0

cipSync :: IO Int64
cipSync = return 0

cipOutput :: IO Int64
cipOutput = do
  decis <- readIORef g_decis
  let goCs !i !acc
        | i >= cipN = return ((acc :: Word32) .&. 0xFFFFFFFF)
        | otherwise = do
            !d <- peekElemOff decis i
            let !w = fromIntegral (fromIntegral d :: Word32) :: Word32
            goCs (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  return (fromIntegral cs :: Int64)
"""


def emit_fl(out_dir, n_chunks):
    fl_lines = [
        f"superinst('init',     {SUPER_INIT},     1, False, False)",
        f"superinst('stage1',   {SUPER_STAGE1},   1, False, False)",
        f"superinst('stage2',   {SUPER_STAGE2},   1, False, False)",
        f"superinst('stage3',   {SUPER_STAGE3},   1, False, False)",
        f"superinst('stage4',   {SUPER_STAGE4},   1, False, False)",
        f"superinst('sync',     {SUPER_SYNC},     1, False, False)",
        f"superinst('output',   {SUPER_OUTPUT},   1, False, False)",
        "avgtime('stage1',  100)",
        "avgtime('stage2',  500)",
        "avgtime('stage3', 5000)",
        "avgtime('stage4', 5000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for k in range(n_chunks):
        fl_lines.append(f"const cid_{k}, {k}")
        fl_lines.append(f"stage1 s1_{k}, cid_{k}, ini")
        fl_lines.append(f"stage2 s2_{k}, cid_{k}, s1_{k}")
        fl_lines.append(f"stage3 s3_{k}, cid_{k}, s2_{k}")
        fl_lines.append(f"stage4 s4_{k}, cid_{k}, s3_{k}")
    current = [f"s4_{k}" for k in range(n_chunks)]
    sync_id = 0
    if len(current) == 1:
        root_var = current[0]
    else:
        while len(current) > 1:
            next_level = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl_lines.append(f"const {cname}, 0")
                fl_lines.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                next_level.append(sname)
                sync_id += 1
            current = next_level
        root_var = current[0]
    fl_lines.append(f"output out, {root_var}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, N, CHUNK_SIZE):
    os.makedirs(out_dir, exist_ok=True)
    n_chunks = (N + CHUNK_SIZE - 1) // CHUNK_SIZE
    emit_fl(out_dir, n_chunks)
    print(f"[gen_cip_hs] wrote {out_dir}/attn.fl  (n_chunks={n_chunks})")
    with open(os.path.join(out_dir, "attn.hsk"), "w") as f:
        f.write(HSK_TEMPLATE)
    print(f"[gen_cip_hs] wrote {out_dir}/attn.hsk")
    inject = (INJECT_TEMPLATE
              .replace("__N__", str(N))
              .replace("__CHUNK_SIZE__", str(CHUNK_SIZE))
              .replace("__DATA_DIR__", data_dir))
    inj_path = os.path.join(out_dir, "supers_inject.hs")
    with open(inj_path, "w") as f:
        f.write(inject)
    print(f"[gen_cip_hs] wrote {inj_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.out_dir, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CHUNK_SIZE"]))


if __name__ == "__main__":
    main()
