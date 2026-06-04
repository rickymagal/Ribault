#!/usr/bin/env python3
"""Generate Ribault sparse-Cholesky files with HASKELL-implemented supers.

Mirrors gen_sc_c.py / gen_sc_rust.py exactly in dataflow shape (same
.fl with one super per DAG op, same super-number assignment). Super
bodies are Haskell using raw `Ptr Double` (mallocBytes for matrix,
peekElemOff / pokeElemOff in POTRF/TRSM/SYRK/GEMM inner kernels).

Outputs:
  attn.hsk           minimal .hsk (6 supers — declaration order REVERSE
                     of super-number assignment to match Ribault convention)
  attn.fl            pre-expanded DF graph (same as gen_sc_c.py)
  supers_inject.hs   Haskell implementations + IORef-based state
"""

import argparse, os, struct


SUPER_OUTPUT = 10
SUPER_TRSM   = 11
SUPER_POTRF  = 12
SUPER_INIT   = 13
SUPER_SYRK   = 14
SUPER_GEMM   = 15

OP_KIND_TO_NAME = {0: 'potrf', 1: 'trsm', 2: 'syrk', 3: 'gemm'}


def read_dag(data_dir):
    path = os.path.join(data_dir, "dag.bin")
    ops = []
    with open(path, "rb") as f:
        (n_ops,) = struct.unpack("<i", f.read(4))
        for _ in range(n_ops):
            kind, ti, tj, s1i, s1j, s2i, s2j, level, n_deps = struct.unpack("<9i", f.read(36))
            deps = list(struct.unpack(f"<{n_deps}i", f.read(4 * n_deps))) if n_deps else []
            ops.append({'id': len(ops), 'kind': kind, 'ti': ti, 'tj': tj,
                        's1i': s1i, 's1j': s1j, 's2i': s2i, 's2j': s2j,
                        'level': level, 'deps': deps})
    return ops


def node_var(op_id):
    return f"o_{op_id}"


HSK_TEMPLATE = """-- attn.hsk  (auto-generated; declaration order REVERSE of super-number
--             assignment: gemm->s15 syrk->s14 init->s13 potrf->s12
--             trsm->s11 output->s10, matching the .fl convention.)

gemm_super idx d1 d2 =
  super gemmImpl idx d1 d2 (
    gemmImpl idx d1 d2 = unsafePerformIO (scGemm (fromIntegral idx))
  )

syrk_super idx d =
  super syrkImpl idx d (
    syrkImpl idx d = unsafePerformIO (scSyrk (fromIntegral idx))
  )

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO scInit
  )

potrf_super idx d =
  super potrfImpl idx d (
    potrfImpl idx d = unsafePerformIO (scPotrf (fromIntegral idx))
  )

trsm_super idx d =
  super trsmImpl idx d (
    trsmImpl idx d = unsafePerformIO (scTrsm (fromIntegral idx))
  )

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO scOutput
  )

main =
  let s = init_super 0
      a = potrf_super 0 s
      b = trsm_super 1 a
      c = syrk_super 2 b
      d = gemm_super 3 b c
  in output_super d
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, poke, peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.))
import Control.Monad (forM_, when)

scNB, scB, scNOps :: Int
scNB    = __NB__
scB     = __B__
scNOps  = __N_OPS__

scDataDir :: FilePath
scDataDir = "__DATA_DIR__"

{-# INLINE scBlockIdx #-}
scBlockIdx :: Int -> Int -> Int
scBlockIdx i j = i*(i+1) `div` 2 + j

{-# NOINLINE g_arr_sc #-}
g_arr_sc :: IORef (Ptr Double)
g_arr_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_kind_sc #-}
g_kind_sc :: IORef (Ptr Int32)
g_kind_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_ti_sc #-}
g_ti_sc :: IORef (Ptr Int32)
g_ti_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_tj_sc #-}
g_tj_sc :: IORef (Ptr Int32)
g_tj_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_s1i_sc #-}
g_s1i_sc :: IORef (Ptr Int32)
g_s1i_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_s1j_sc #-}
g_s1j_sc :: IORef (Ptr Int32)
g_s1j_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_s2i_sc #-}
g_s2i_sc :: IORef (Ptr Int32)
g_s2i_sc = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_s2j_sc #-}
g_s2j_sc :: IORef (Ptr Int32)
g_s2j_sc = unsafePerformIO (newIORef nullPtr)

scAllocI32 :: Int -> IO (Ptr Int32)
scAllocI32 n = do
  p <- mallocBytes (n * 4) :: IO (Ptr Int32)
  forM_ [0..n-1] $ \i -> pokeElemOff p i (0 :: Int32)
  return p

scReadF64 :: FilePath -> Int -> IO (Ptr Double)
scReadF64 path nbytes = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes nbytes :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src nbytes
  return (castPtr p)

scInit :: IO Int64
scInit = do
  let nBlocks = scNB*(scNB+1) `div` 2
      total = nBlocks * scB * scB
      nbytes = total * 8
  arr <- scReadF64 (scDataDir ++ "/A.bin") nbytes
  writeIORef g_arr_sc arr

  -- Load flat DAG arrays.
  ki <- scAllocI32 scNOps; ti <- scAllocI32 scNOps; tj <- scAllocI32 scNOps
  s1i <- scAllocI32 scNOps; s1j <- scAllocI32 scNOps
  s2i <- scAllocI32 scNOps; s2j <- scAllocI32 scNOps
  bs <- BS.readFile (scDataDir ++ "/dag.bin")
  let !(BSI.BS fp _) = bs
  withForeignPtr fp $ \src8 -> do
    let src = castPtr src8 :: Ptr Int32
        readOp !off !i
          | i >= scNOps = return ()
          | otherwise = do
              !k  <- peekElemOff src off
              !ti'<- peekElemOff src (off + 1)
              !tj'<- peekElemOff src (off + 2)
              !a' <- peekElemOff src (off + 3)
              !b' <- peekElemOff src (off + 4)
              !c' <- peekElemOff src (off + 5)
              !d' <- peekElemOff src (off + 6)
              !nd <- (fromIntegral :: Int32 -> Int) <$> peekElemOff src (off + 8)
              pokeElemOff ki i k
              pokeElemOff ti i ti'; pokeElemOff tj i tj'
              pokeElemOff s1i i a'; pokeElemOff s1j i b'
              pokeElemOff s2i i c'; pokeElemOff s2j i d'
              readOp (off + 9 + nd) (i + 1)
    readOp 1 0
  writeIORef g_kind_sc ki
  writeIORef g_ti_sc ti; writeIORef g_tj_sc tj
  writeIORef g_s1i_sc s1i; writeIORef g_s1j_sc s1j
  writeIORef g_s2i_sc s2i; writeIORef g_s2j_sc s2j
  return 0

{-# INLINE scPotrfBlock #-}
scPotrfBlock :: Ptr Double -> Int -> IO ()
scPotrfBlock !arr !tgt = do
  let b = scB
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

{-# INLINE scTrsmBlock #-}
scTrsmBlock :: Ptr Double -> Int -> Int -> IO ()
scTrsmBlock !arr !x !l =
  let b = scB in
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

{-# INLINE scSyrkBlock #-}
scSyrkBlock :: Ptr Double -> Int -> Int -> IO ()
scSyrkBlock !arr !c !a =
  let b = scB in
  forM_ [0..b-1] $ \i -> forM_ [0..i] $ \j -> do
    let go !kk !acc | kk >= b = return acc
                    | otherwise = do
                        !x <- peekElemOff arr (a + i*b + kk)
                        !y <- peekElemOff arr (a + j*b + kk)
                        go (kk+1) (acc + x*y)
    !s <- go 0 0.0
    !c0 <- peekElemOff arr (c + i*b + j)
    pokeElemOff arr (c + i*b + j) (c0 - s)

{-# INLINE scGemmBlock #-}
scGemmBlock :: Ptr Double -> Int -> Int -> Int -> IO ()
scGemmBlock !arr !c !a !b_ =
  let b = scB in
  forM_ [0..b-1] $ \i -> forM_ [0..b-1] $ \j -> do
    let go !kk !acc | kk >= b = return acc
                    | otherwise = do
                        !x <- peekElemOff arr (a + i*b + kk)
                        !y <- peekElemOff arr (b_ + j*b + kk)
                        go (kk+1) (acc + x*y)
    !s <- go 0 0.0
    !c0 <- peekElemOff arr (c + i*b + j)
    pokeElemOff arr (c + i*b + j) (c0 - s)

{-# INLINE scOpOff #-}
scOpOff :: IORef (Ptr Int32) -> IORef (Ptr Int32) -> Int -> IO Int
scOpOff iRef jRef !idx = do
  !pi' <- readIORef iRef
  !pj' <- readIORef jRef
  !i <- peekElemOff pi' idx
  !j <- peekElemOff pj' idx
  return (scBlockIdx (fromIntegral i) (fromIntegral j) * scB * scB)

scPotrf :: Int -> IO Int64
scPotrf !idx = do
  arr <- readIORef g_arr_sc
  off <- scOpOff g_ti_sc g_tj_sc idx
  scPotrfBlock arr off
  return 0

scTrsm :: Int -> IO Int64
scTrsm !idx = do
  arr <- readIORef g_arr_sc
  tgt <- scOpOff g_ti_sc g_tj_sc idx
  src <- scOpOff g_s1i_sc g_s1j_sc idx
  scTrsmBlock arr tgt src
  return 0

scSyrk :: Int -> IO Int64
scSyrk !idx = do
  arr <- readIORef g_arr_sc
  tgt <- scOpOff g_ti_sc g_tj_sc idx
  src <- scOpOff g_s1i_sc g_s1j_sc idx
  scSyrkBlock arr tgt src
  return 0

scGemm :: Int -> IO Int64
scGemm !idx = do
  arr <- readIORef g_arr_sc
  tgt <- scOpOff g_ti_sc g_tj_sc idx
  s1  <- scOpOff g_s1i_sc g_s1j_sc idx
  s2  <- scOpOff g_s2i_sc g_s2j_sc idx
  scGemmBlock arr tgt s1 s2
  return 0

scOutput :: IO Int64
scOutput = do
  arr <- readIORef g_arr_sc
  let nBlocks = scNB*(scNB+1) `div` 2
      totalElems = nBlocks * scB * scB
      goCs !i !acc
        | i >= totalElems = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !v <- peekElemOff arr i
            let !fixed = truncate (v * 1e6) :: Int
                !w = fromIntegral (fromIntegral fixed :: Word32) :: Word64
            goCs (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  return (fromIntegral cs :: Int64)
"""


def emit_fl_v2(out_dir, ops):
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('potrf',  {SUPER_POTRF},  1, False, False)",
        f"superinst('trsm',   {SUPER_TRSM},   1, False, False)",
        f"superinst('syrk',   {SUPER_SYRK},   1, False, False)",
        f"superinst('gemm',   {SUPER_GEMM},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('potrf', 100)",
        "avgtime('trsm',  500)",
        "avgtime('syrk',  500)",
        "avgtime('gemm',  500)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for op in ops:
        op_id   = op['id']
        op_name = OP_KIND_TO_NAME[op['kind']]
        dep_vars = [node_var(d) for d in op['deps']] if op['deps'] else ['ini']
        fl_lines.append(f"const k_{op_id}, {op_id}")
        fl_lines.append(f"{op_name} {node_var(op_id)}, k_{op_id}, " + ", ".join(dep_vars))
    fl_lines.append(f"output out, {node_var(len(ops) - 1)}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, NB, B):
    os.makedirs(out_dir, exist_ok=True)
    ops = read_dag(data_dir)
    emit_fl_v2(out_dir, ops)
    print(f"[gen_sc_hs] wrote {out_dir}/attn.fl  (n_ops={len(ops)})")

    with open(os.path.join(out_dir, "attn.hsk"), "w") as f:
        f.write(HSK_TEMPLATE)
    print(f"[gen_sc_hs] wrote {out_dir}/attn.hsk")

    inject = (INJECT_TEMPLATE
              .replace("__NB__", str(NB))
              .replace("__B__", str(B))
              .replace("__N_OPS__", str(len(ops)))
              .replace("__DATA_DIR__", data_dir))
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    with open(inject_path, "w") as f:
        f.write(inject)
    print(f"[gen_sc_hs] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["NB"], cfg["B"])


if __name__ == "__main__":
    main()
