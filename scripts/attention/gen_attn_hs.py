#!/usr/bin/env python3
"""Generate Ribault attention files with HASKELL-implemented supers.

Mirrors gen_attn_c.py / gen_attn_rust.py exactly in dataflow shape (same
5-super graph: init, phaseA, barrier, phaseB, output; same .fl emission
with chunked fan-in for barrier/output to stay within the TALM 5-bit
src-count limit). The only thing that changes is the super body
language: Haskell, with IDIOMATIC mutable storage (IORef +
Data.Vector.Storable.Mutable) — no Foreign.Ptr in user code, no
mallocBytes, no allocaBytes. The Trebuchet C runtime carries the FFI
cost at the super boundary; user-visible Haskell uses Vector.Storable.

This is the deferred Ribault-Hs attention variant described in
results/attn_paper_final/description.txt section 11 as "future work";
it is the Haskell-tier numerator (its denominator is the Haskell
sequential baseline, gen_attn_hs_sequential.py). The same effort and
the same compute backend as GHC Strategies — only the parallel
scheduler (Trebuchet dataflow vs parList rseq sparks) differs.

Outputs:
  - <out-dir>/attn.hsk          : minimal .hsk for supersgen (5 supers
                                   declared in REVERSE order so that the
                                   compiler-assigned numbers come out as
                                   barrier=s14, init=s13, phaseA=s12,
                                   phaseB=s11, output=s10 — matching
                                   gen_attn_c.py's SUPER_* constants).
  - <out-dir>/attn.fl           : pre-expanded dataflow graph (bypasses
                                   the .hsk -> .fl codegen so we can
                                   handle arbitrary n_blocks with the
                                   chunked fan-in; same .fl that
                                   gen_attn_c.py / gen_attn_rust.py emit).
  - <out-dir>/supers_inject.hs  : Haskell implementation (NOINLINE IORefs
                                   for global state + per-super kernels)
                                   injected into the auto-generated
                                   Supers.hs by build_supers.sh via
                                   SUPERS_INJECT_FILE.
"""

import argparse, os


SUPER_INIT    = 13
SUPER_PHASE_A = 12
SUPER_BARRIER = 14
SUPER_PHASE_B = 11
SUPER_RESULT  = 10


HSK_TEMPLATE = """-- attn.hsk  (auto-generated, minimal for supersgen)
-- Constants for reference; the real graph is in attn.fl.
-- N=__N__  D=__D__  N_HEADS=__N_HEADS__  N_BLOCKS=__N_BLOCKS__
--
-- Declaration order is REVERSE of super-number assignment:
--   1st declared (barrier_super)  -> s14
--   2nd declared (init_super)     -> s13
--   3rd declared (phaseA_super)   -> s12
--   4th declared (phaseB_super)   -> s11
--   5th declared (output_super)   -> s10

barrier_super dep =
  super barrierImpl dep (
    barrierImpl dep = unsafePerformIO attnBarrier
  )

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO attnInit
  )

phaseA_super dep idx =
  super pAImpl dep idx (
    pAImpl dep idx = unsafePerformIO (attnPhaseA (fromIntegral idx))
  )

phaseB_super dep idx =
  super pBImpl dep idx (
    pBImpl dep idx = unsafePerformIO (attnPhaseB (fromIntegral idx))
  )

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO attnOutput
  )

main =
  let s = init_super 0
      a = phaseA_super s 0
      b = barrier_super a
      c = phaseB_super b 0
  in output_super c
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8)
import Data.Bits ((.&.))
import Control.Monad (forM_)

-- Constants (baked at codegen time).
attnN, attnD, attnHeads, attnHeadDim, attnDff, attnVocab, attnBlocks :: Int
attnN       = __N__
attnD       = __D__
attnHeads   = __N_HEADS__
attnHeadDim = __HEAD_DIM__
attnDff     = __D_FF__
attnVocab   = __VOCAB__
attnBlocks  = __N_BLOCKS__

attnDataDir :: FilePath
attnDataDir = "__DATA_DIR__"

-- Global shared state via NOINLINE IORefs to mutable storable vectors.
-- One IORef per tensor / activation. Same idiom as gen_attn_hs_strategies.py.
{-# NOINLINE g_e_attn #-}
g_e_attn :: IORef (MV.IOVector Double)
g_e_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_pe_attn #-}
g_pe_attn :: IORef (MV.IOVector Double)
g_pe_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wq_attn #-}
g_wq_attn :: IORef (MV.IOVector Double)
g_wq_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wk_attn #-}
g_wk_attn :: IORef (MV.IOVector Double)
g_wk_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wv_attn #-}
g_wv_attn :: IORef (MV.IOVector Double)
g_wv_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wo_attn #-}
g_wo_attn :: IORef (MV.IOVector Double)
g_wo_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_w1_attn #-}
g_w1_attn :: IORef (MV.IOVector Double)
g_w1_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_w2_attn #-}
g_w2_attn :: IORef (MV.IOVector Double)
g_w2_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_wu_attn #-}
g_wu_attn :: IORef (MV.IOVector Double)
g_wu_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln1w_attn #-}
g_ln1w_attn :: IORef (MV.IOVector Double)
g_ln1w_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln1b_attn #-}
g_ln1b_attn :: IORef (MV.IOVector Double)
g_ln1b_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln2w_attn #-}
g_ln2w_attn :: IORef (MV.IOVector Double)
g_ln2w_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ln2b_attn #-}
g_ln2b_attn :: IORef (MV.IOVector Double)
g_ln2b_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_x_attn #-}
g_x_attn :: IORef (MV.IOVector Double)
g_x_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_xa_attn #-}
g_xa_attn :: IORef (MV.IOVector Double)
g_xa_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_q_attn #-}
g_q_attn :: IORef (MV.IOVector Double)
g_q_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_k_attn #-}
g_k_attn :: IORef (MV.IOVector Double)
g_k_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_v_attn #-}
g_v_attn :: IORef (MV.IOVector Double)
g_v_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_attn_attn #-}
g_attn_attn :: IORef (MV.IOVector Double)
g_attn_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_xb_attn #-}
g_xb_attn :: IORef (MV.IOVector Double)
g_xb_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_ffnh_attn #-}
g_ffnh_attn :: IORef (MV.IOVector Double)
g_ffnh_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_logits_attn #-}
g_logits_attn :: IORef (MV.IOVector Double)
g_logits_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_inputT_attn #-}
g_inputT_attn :: IORef (MV.IOVector Word8)
g_inputT_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)
{-# NOINLINE g_outputT_attn #-}
g_outputT_attn :: IORef (MV.IOVector Word8)
g_outputT_attn = unsafePerformIO (newIORef =<< MV.replicate 0 0)

attnReadDoubles :: FilePath -> Int -> IO (MV.IOVector Double)
attnReadDoubles path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  v <- MV.replicate count 0.0
  withForeignPtr fp $ \src ->
    forM_ [0..count-1] $ \i -> do
      !d <- peek (src `plusPtr` (i*8) :: Ptr Double)
      MV.write v i d
  return v

attnReadBytes :: FilePath -> Int -> IO (MV.IOVector Word8)
attnReadBytes path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  v <- MV.replicate count 0
  withForeignPtr fp $ \src ->
    forM_ [0..count-1] $ \i -> do
      !w <- peek (src `plusPtr` i :: Ptr Word8)
      MV.write v i w
  return v

attnRowLo :: Int -> Int
attnRowLo b = b * attnN `div` attnBlocks
attnRowHi :: Int -> Int
attnRowHi b = if b == attnBlocks - 1 then attnN else (b + 1) * attnN `div` attnBlocks

-- Per-block matmul: rows [lo, hi) of c = a @ b.
attnMatmulBlock :: Int -> Int -> MV.IOVector Double -> MV.IOVector Double
                -> MV.IOVector Double -> Int -> Int -> IO ()
attnMatmulBlock !lo !hi !a !b !c !kDim !nDim =
  forM_ [lo..hi-1] $ \m -> do
    forM_ [0..nDim-1] $ \n -> MV.write c (m*nDim + n) 0.0
    forM_ [0..kDim-1] $ \k -> do
      !av <- MV.read a (m*kDim + k)
      forM_ [0..nDim-1] $ \n -> do
        !bv <- MV.read b (k*nDim + n)
        !cv <- MV.read c (m*nDim + n)
        MV.write c (m*nDim + n) (cv + av*bv)

-- Per-block LayerNorm: out[lo..hi, :] = LN(x[lo..hi, :])
attnLayerNormBlock :: Int -> Int -> MV.IOVector Double -> MV.IOVector Double
                   -> MV.IOVector Double -> MV.IOVector Double -> Int -> IO ()
attnLayerNormBlock !lo !hi !x !w !b !out !dim = do
  let eps = 1e-5
  forM_ [lo..hi-1] $ \i -> do
    !mu <- attnMean x (i*dim) dim
    !var <- attnVar x (i*dim) dim mu
    let !inv = 1.0 / sqrt (var + eps)
    forM_ [0..dim-1] $ \j -> do
      !v  <- MV.read x (i*dim + j)
      !wj <- MV.read w j
      !bj <- MV.read b j
      MV.write out (i*dim + j) ((v - mu) * inv * wj + bj)
  where
    attnMean !v !base !d = do
      let go !i !acc
            | i >= d    = return (acc / fromIntegral d)
            | otherwise = do
                !x' <- MV.read v (base + i)
                go (i+1) (acc + x')
      go 0 0.0
    attnVar !v !base !d !mu = do
      let go !i !acc
            | i >= d    = return (acc / fromIntegral d)
            | otherwise = do
                !x' <- MV.read v (base + i)
                let !dd = x' - mu
                go (i+1) (acc + dd*dd)
      go 0 0.0

-- ============================================================
-- Super bodies (called from the s14..s10 stubs auto-generated
-- by supersgen for the .hsk; the .hsk's super wrappers do
-- unsafePerformIO of these IO actions).
-- ============================================================

-- INIT (s13): load all weights and inputs, allocate every activation buffer.
attnInit :: IO Int64
attnInit = do
  e       <- attnReadDoubles (attnDataDir ++ "/E.bin")      (attnVocab * attnD)
  peM     <- attnReadDoubles (attnDataDir ++ "/PE.bin")     (attnN     * attnD)
  wQ      <- attnReadDoubles (attnDataDir ++ "/W_Q.bin")    (attnD     * attnD)
  wK      <- attnReadDoubles (attnDataDir ++ "/W_K.bin")    (attnD     * attnD)
  wV      <- attnReadDoubles (attnDataDir ++ "/W_V.bin")    (attnD     * attnD)
  wO      <- attnReadDoubles (attnDataDir ++ "/W_O.bin")    (attnD     * attnD)
  w1      <- attnReadDoubles (attnDataDir ++ "/W_1.bin")    (attnD     * attnDff)
  w2      <- attnReadDoubles (attnDataDir ++ "/W_2.bin")    (attnDff   * attnD)
  wU      <- attnReadDoubles (attnDataDir ++ "/W_U.bin")    (attnD     * attnVocab)
  ln1w    <- attnReadDoubles (attnDataDir ++ "/LN_1_w.bin") attnD
  ln1b    <- attnReadDoubles (attnDataDir ++ "/LN_1_b.bin") attnD
  ln2w    <- attnReadDoubles (attnDataDir ++ "/LN_2_w.bin") attnD
  ln2b    <- attnReadDoubles (attnDataDir ++ "/LN_2_b.bin") attnD
  inputT  <- attnReadBytes   (attnDataDir ++ "/input_tokens.bin") attnN
  outputT <- MV.replicate attnN (0 :: Word8)

  xBuf   <- MV.replicate (attnN * attnD)     0.0
  xaBuf  <- MV.replicate (attnN * attnD)     0.0
  qBuf   <- MV.replicate (attnN * attnD)     0.0
  kBuf   <- MV.replicate (attnN * attnD)     0.0
  vBuf   <- MV.replicate (attnN * attnD)     0.0
  attnB  <- MV.replicate (attnN * attnD)     0.0
  xbBuf  <- MV.replicate (attnN * attnD)     0.0
  ffnH   <- MV.replicate (attnN * attnDff)   0.0
  logits <- MV.replicate (attnN * attnVocab) 0.0

  writeIORef g_e_attn e
  writeIORef g_pe_attn peM
  writeIORef g_wq_attn wQ
  writeIORef g_wk_attn wK
  writeIORef g_wv_attn wV
  writeIORef g_wo_attn wO
  writeIORef g_w1_attn w1
  writeIORef g_w2_attn w2
  writeIORef g_wu_attn wU
  writeIORef g_ln1w_attn ln1w
  writeIORef g_ln1b_attn ln1b
  writeIORef g_ln2w_attn ln2w
  writeIORef g_ln2b_attn ln2b
  writeIORef g_inputT_attn inputT
  writeIORef g_outputT_attn outputT
  writeIORef g_x_attn xBuf
  writeIORef g_xa_attn xaBuf
  writeIORef g_q_attn qBuf
  writeIORef g_k_attn kBuf
  writeIORef g_v_attn vBuf
  writeIORef g_attn_attn attnB
  writeIORef g_xb_attn xbBuf
  writeIORef g_ffnh_attn ffnH
  writeIORef g_logits_attn logits
  return 0

-- PHASE A (s12): rows [lo, hi) of block b — embed + LN_1 + Q/K/V.
attnPhaseA :: Int -> IO Int64
attnPhaseA !b = do
  inputT <- readIORef g_inputT_attn
  e      <- readIORef g_e_attn
  peM    <- readIORef g_pe_attn
  ln1w   <- readIORef g_ln1w_attn
  ln1b   <- readIORef g_ln1b_attn
  wQ     <- readIORef g_wq_attn
  wK     <- readIORef g_wk_attn
  wV     <- readIORef g_wv_attn
  xBuf   <- readIORef g_x_attn
  xaBuf  <- readIORef g_xa_attn
  qBuf   <- readIORef g_q_attn
  kBuf   <- readIORef g_k_attn
  vBuf   <- readIORef g_v_attn
  let !lo = attnRowLo b
      !hi = attnRowHi b
  forM_ [lo..hi-1] $ \i -> do
    !tok <- fromIntegral <$> MV.read inputT i
    forM_ [0..attnD-1] $ \j -> do
      !ev <- MV.read e (tok*attnD + j)
      !pv <- MV.read peM (i*attnD + j)
      MV.write xBuf (i*attnD + j) (ev + pv)
  attnLayerNormBlock lo hi xBuf ln1w ln1b xaBuf attnD
  attnMatmulBlock lo hi xaBuf wQ qBuf attnD attnD
  attnMatmulBlock lo hi xaBuf wK kBuf attnD attnD
  attnMatmulBlock lo hi xaBuf wV vBuf attnD attnD
  return 0

-- BARRIER (s14): pure synchronization. Inputs ignored.
attnBarrier :: IO Int64
attnBarrier = return 0

-- PHASE B (s11): rows [lo, hi) — MHSA + W_O + LN_2 + FFN + W_U + argmax.
attnPhaseB :: Int -> IO Int64
attnPhaseB !b = do
  qBuf   <- readIORef g_q_attn
  kBuf   <- readIORef g_k_attn
  vBuf   <- readIORef g_v_attn
  attnB  <- readIORef g_attn_attn
  xBuf   <- readIORef g_x_attn
  xaBuf  <- readIORef g_xa_attn
  xbBuf  <- readIORef g_xb_attn
  ln2w   <- readIORef g_ln2w_attn
  ln2b   <- readIORef g_ln2b_attn
  wO     <- readIORef g_wo_attn
  w1     <- readIORef g_w1_attn
  w2     <- readIORef g_w2_attn
  wU     <- readIORef g_wu_attn
  ffnH   <- readIORef g_ffnh_attn
  logits <- readIORef g_logits_attn
  outputT<- readIORef g_outputT_attn
  let !lo = attnRowLo b
      !hi = attnRowHi b
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
                  !q  <- MV.read qBuf (i*attnD + h*attnHeadDim + k)
                  !kv <- MV.read kBuf (j*attnD + h*attnHeadDim + k)
                  go (k+1) (acc + q*kv)
        !s <- go 0 0.0
        MV.write scores j (s * inv)
      !mx0 <- MV.read scores 0
      let goMax !ii !m
            | ii >= attnN = return m
            | otherwise = do
                !v <- MV.read scores ii
                goMax (ii+1) (max m v)
      !mx <- goMax 1 mx0
      let goSum !ii !acc
            | ii >= attnN = return acc
            | otherwise = do
                !v <- MV.read scores ii
                let !ev = exp (v - mx)
                MV.write scores ii ev
                goSum (ii+1) (acc + ev)
      !sm <- goSum 0 0.0
      forM_ [0..attnN-1] $ \j -> do
        !v <- MV.read scores j
        MV.write scores j (v / sm)
      forM_ [0..attnN-1] $ \j -> do
        !a <- MV.read scores j
        forM_ [0..attnHeadDim-1] $ \k -> do
          !vv  <- MV.read vBuf (j*attnD + h*attnHeadDim + k)
          !cur <- MV.read attnB (i*attnD + h*attnHeadDim + k)
          MV.write attnB (i*attnD + h*attnHeadDim + k) (cur + a*vv)
  attnMatmulBlock lo hi attnB wO xaBuf attnD attnD
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnD-1] $ \j -> do
      !xv <- MV.read xBuf  (i*attnD + j)
      !yv <- MV.read xaBuf (i*attnD + j)
      MV.write xBuf (i*attnD + j) (xv + yv)
  attnLayerNormBlock lo hi xBuf ln2w ln2b xbBuf attnD
  attnMatmulBlock lo hi xbBuf w1 ffnH attnD attnDff
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnDff-1] $ \j -> do
      !v <- MV.read ffnH (i*attnDff + j)
      MV.write ffnH (i*attnDff + j) (if v < 0 then 0 else v)
  attnMatmulBlock lo hi ffnH w2 xaBuf attnDff attnD
  forM_ [lo..hi-1] $ \i ->
    forM_ [0..attnD-1] $ \j -> do
      !xv <- MV.read xBuf  (i*attnD + j)
      !yv <- MV.read xaBuf (i*attnD + j)
      MV.write xBuf (i*attnD + j) (xv + yv)
  attnMatmulBlock lo hi xBuf wU logits attnD attnVocab
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
  return 0

-- OUTPUT (s10): final fan-in -- read output_tokens and print checksum.
attnOutput :: IO Int64
attnOutput = do
  outputT <- readIORef g_outputT_attn
  let goCs !i !acc
        | i >= attnN = return ((acc :: Word) .&. 0xFFFFFFFF)
        | otherwise = do
            !w <- MV.read outputT i
            goCs (i+1) (acc + fromIntegral (i+1) * fromIntegral w)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  return (fromIntegral cs :: Int64)
"""


def emit_chunked_fan_in(final_kind, leaf_names, final_name,
                        intermediate_kind="barrier", branch=16):
    """Logarithmic-depth fan-in. The TALM assembler encodes source count in
    5 bits so each super accepts at most 31 inputs; we chunk into groups of
    `branch` (default 16). Intermediate levels use `intermediate_kind`
    (no side effects); only the final level uses `final_kind`.
    Mirrors emit_chunked_fan_in() in gen_attn_c.py exactly.
    """
    if len(leaf_names) <= branch:
        return [f"{final_kind} {final_name}, " + ", ".join(leaf_names)], final_name
    lines = []
    intermediates = []
    for ci in range(0, len(leaf_names), branch):
        chunk = leaf_names[ci:ci + branch]
        name = f"{final_name}_lvl0_{ci // branch}"
        lines.append(f"{intermediate_kind} {name}, " + ", ".join(chunk))
        intermediates.append(name)
    if len(intermediates) <= branch:
        lines.append(f"{final_kind} {final_name}, " + ", ".join(intermediates))
        return lines, final_name
    raise ValueError(f"n_blocks={len(leaf_names)} exceeds branch^2={branch*branch}; extend tree")


def emit(out_dir, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
    os.makedirs(out_dir, exist_ok=True)

    # ---- 1. minimal .hsk (for supersgen) ----
    hsk_src = (HSK_TEMPLATE
               .replace("__N__", str(N))
               .replace("__D__", str(D))
               .replace("__N_HEADS__", str(n_heads))
               .replace("__N_BLOCKS__", str(n_blocks)))
    hsk_path = os.path.join(out_dir, "attn.hsk")
    with open(hsk_path, "w") as f:
        f.write(hsk_src)
    print(f"[gen_attn_hs] wrote {hsk_path}")

    # ---- 2. attn.fl  (mirrors gen_attn_c.py exactly) ----
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('phaseA',  {SUPER_PHASE_A}, 1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('phaseB',  {SUPER_PHASE_B}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('phaseA', 1000)",
        "avgtime('phaseB', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    # Phase A supers (one per block)
    for b in range(n_blocks):
        fl_lines.append(f"const k_A_{b}, {b}")
        fl_lines.append(f"phaseA a{b}, ini, k_A_{b}")
    # Chunked barrier (handles n_blocks > 31). Intermediate AND final use "barrier".
    barrier_lines, barrier_name = emit_chunked_fan_in(
        "barrier", [f"a{b}" for b in range(n_blocks)], "br",
        intermediate_kind="barrier")
    fl_lines.extend(barrier_lines)
    # Phase B supers (one per block)
    for b in range(n_blocks):
        fl_lines.append(f"const k_B_{b}, {b}")
        fl_lines.append(f"phaseB b{b}, {barrier_name}, k_B_{b}")
    # Chunked output: intermediate "barrier", top "output" (prints CHECKSUM=).
    output_lines, _ = emit_chunked_fan_in(
        "output", [f"b{b}" for b in range(n_blocks)], "out",
        intermediate_kind="barrier")
    fl_lines.extend(output_lines)

    fl_path = os.path.join(out_dir, "attn.fl")
    with open(fl_path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_attn_hs] wrote {fl_path}  (n_blocks={n_blocks})")

    # ---- 3. supers_inject.hs ----
    inject_src = (INJECT_TEMPLATE
                  .replace("__N__", str(N))
                  .replace("__D__", str(D))
                  .replace("__N_HEADS__", str(n_heads))
                  .replace("__HEAD_DIM__", str(head_dim))
                  .replace("__D_FF__", str(d_ff))
                  .replace("__VOCAB__", str(vocab))
                  .replace("__N_BLOCKS__", str(n_blocks))
                  .replace("__DATA_DIR__", data_dir))
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    with open(inject_path, "w") as f:
        f.write(inject_src)
    print(f"[gen_attn_hs] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
