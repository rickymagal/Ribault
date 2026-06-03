#!/usr/bin/env python3
"""Generate Ribault mergesort files with HASKELL-implemented supers.

Mirrors gen_ms_c.py / gen_ms_rust.py exactly in dataflow shape (same
.fl with binary tree of leaves + merges, same tree.bin). Super bodies
are Haskell using raw `Ptr Int32` (mallocBytes for arr/tmp, peekElemOff
/ pokeElemOff in inner kernels). Same tight idiom as attention's
ribault_hs raw-Ptr build.

Outputs:
  attn.hsk           minimal .hsk (5 supers in reverse declaration order)
  attn.fl            pre-expanded DF graph (same as gen_ms_c.py)
  supers_inject.hs   Haskell implementation (NOINLINE IORefs + kernels)
"""

import argparse, os, struct


SUPER_INIT    = 13
SUPER_LEAF    = 12
SUPER_BARRIER = 14
SUPER_MERGE   = 11
SUPER_RESULT  = 10


def build_tree(N, cutoff):
    leaves = []; merges = []
    def recurse(lo, hi):
        if hi - lo <= cutoff:
            i = len(leaves); leaves.append((lo, hi)); return ("leaf", i)
        mid = lo + (hi - lo) // 2
        left = recurse(lo, mid); right = recurse(mid, hi)
        i = len(merges); merges.append((lo, mid, hi, left, right))
        return ("merge", i)
    root = recurse(0, N)
    return leaves, merges, root


def write_tree_bin(out_dir, leaves, merges):
    path = os.path.join(out_dir, "tree.bin")
    with open(path, "wb") as f:
        f.write(struct.pack("<2i", len(leaves), len(merges)))
        for (lo, hi) in leaves:
            f.write(struct.pack("<2i", lo, hi))
        for (lo, mid, hi, _l, _r) in merges:
            f.write(struct.pack("<3i", lo, mid, hi))


HSK_TEMPLATE = """-- attn.hsk  (auto-generated; declaration order REVERSE of super-number
--             assignment so that barrier->s14 init->s13 leaf->s12
--             merge->s11 output->s10, matching the .fl convention.)

barrier_super dep =
  super barrierImpl dep (
    barrierImpl dep = unsafePerformIO msBarrier
  )

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO msInit
  )

leaf_super dep idx =
  super leafImpl dep idx (
    leafImpl dep idx = unsafePerformIO (msLeaf (fromIntegral idx))
  )

merge_super l r idx =
  super mergeImpl l r idx (
    mergeImpl l r idx = unsafePerformIO (msMerge (fromIntegral idx))
  )

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO msOutput
  )

main =
  let s = init_super 0
      a = leaf_super s 0
      b = barrier_super a
      c = merge_super a b 0
  in output_super c
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, poke, peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Int (Int32)
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.))
import Control.Monad (forM_, when)

msN, msCutoff, msNLeaves, msNMerges :: Int
msN        = __N__
msCutoff   = __CUTOFF__
msNLeaves  = __N_LEAVES__
msNMerges  = __N_MERGES__

msDataDir :: FilePath
msDataDir = "__DATA_DIR__"

-- Global state: raw Ptr Int32 for arr/tmp, raw Ptr Int32 for tree tables.
{-# NOINLINE g_arr_ms #-}
g_arr_ms :: IORef (Ptr Int32)
g_arr_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_tmp_ms #-}
g_tmp_ms :: IORef (Ptr Int32)
g_tmp_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_leaf_lo_ms #-}
g_leaf_lo_ms :: IORef (Ptr Int32)
g_leaf_lo_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_leaf_hi_ms #-}
g_leaf_hi_ms :: IORef (Ptr Int32)
g_leaf_hi_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_merge_lo_ms #-}
g_merge_lo_ms :: IORef (Ptr Int32)
g_merge_lo_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_merge_mid_ms #-}
g_merge_mid_ms :: IORef (Ptr Int32)
g_merge_mid_ms = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_merge_hi_ms #-}
g_merge_hi_ms :: IORef (Ptr Int32)
g_merge_hi_ms = unsafePerformIO (newIORef nullPtr)

msAllocI32 :: Int -> IO (Ptr Int32)
msAllocI32 count = do
  p <- mallocBytes (count * 4) :: IO (Ptr Int32)
  forM_ [0..count-1] $ \i -> pokeElemOff p i (0 :: Int32)
  return p

msReadBin :: FilePath -> Int -> IO (Ptr Int32)
msReadBin path count = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  p <- mallocBytes (count * 4) :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src (count * 4)
  return (castPtr p)

msInit :: IO Int64
msInit = do
  arr <- msReadBin (msDataDir ++ "/input.bin") msN
  tmp <- msAllocI32 msN

  -- Load tree.bin into 5 raw int32 arrays.
  treeBS <- BS.readFile (msDataDir ++ "/tree.bin")
  let !(BSI.BS fp _) = treeBS
  withForeignPtr fp $ \src8 -> do
    let src = castPtr src8 :: Ptr Int32
    -- Skip 2-int header.
    leafLo  <- msAllocI32 msNLeaves
    leafHi  <- msAllocI32 msNLeaves
    mergeLo <- msAllocI32 msNMerges
    mergeMid<- msAllocI32 msNMerges
    mergeHi <- msAllocI32 msNMerges
    -- Leaves: at offset 2, pairs (lo, hi) for each leaf.
    forM_ [0..msNLeaves-1] $ \i -> do
      !lo <- peekElemOff src (2 + i*2)
      !hi <- peekElemOff src (2 + i*2 + 1)
      pokeElemOff leafLo i lo
      pokeElemOff leafHi i hi
    -- Merges: at offset 2 + 2*N_LEAVES, 4-tuples (lo, mid, hi, level) per merge.
    -- The level field is used only by STRAT/parpseq; here we read but ignore it.
    let mergeBase = 2 + 2 * msNLeaves
    forM_ [0..msNMerges-1] $ \i -> do
      !lo  <- peekElemOff src (mergeBase + i*4)
      !mid <- peekElemOff src (mergeBase + i*4 + 1)
      !hi  <- peekElemOff src (mergeBase + i*4 + 2)
      pokeElemOff mergeLo  i lo
      pokeElemOff mergeMid i mid
      pokeElemOff mergeHi  i hi
    writeIORef g_leaf_lo_ms   leafLo
    writeIORef g_leaf_hi_ms   leafHi
    writeIORef g_merge_lo_ms  mergeLo
    writeIORef g_merge_mid_ms mergeMid
    writeIORef g_merge_hi_ms  mergeHi
  writeIORef g_arr_ms arr
  writeIORef g_tmp_ms tmp
  return 0

{-# INLINE msInsertionSort #-}
msInsertionSort :: Ptr Int32 -> Int -> Int -> IO ()
msInsertionSort !a !lo !hi = go (lo + 1)
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

{-# INLINE msMergeOp #-}
msMergeOp :: Ptr Int32 -> Ptr Int32 -> Int -> Int -> Int -> IO ()
msMergeOp !arr !tmp !lo !mid !hi = do
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

msLeaf :: Int -> IO Int64
msLeaf !leafIdx = do
  arr     <- readIORef g_arr_ms
  leafLo  <- readIORef g_leaf_lo_ms
  leafHi  <- readIORef g_leaf_hi_ms
  !lo <- peekElemOff leafLo leafIdx
  !hi <- peekElemOff leafHi leafIdx
  msInsertionSort arr (fromIntegral lo) (fromIntegral hi)
  return 0

msMerge :: Int -> IO Int64
msMerge !mergeIdx = do
  arr     <- readIORef g_arr_ms
  tmp     <- readIORef g_tmp_ms
  mergeLo <- readIORef g_merge_lo_ms
  mergeMid<- readIORef g_merge_mid_ms
  mergeHi <- readIORef g_merge_hi_ms
  !lo  <- peekElemOff mergeLo  mergeIdx
  !mid <- peekElemOff mergeMid mergeIdx
  !hi  <- peekElemOff mergeHi  mergeIdx
  msMergeOp arr tmp (fromIntegral lo) (fromIntegral mid) (fromIntegral hi)
  return 0

msBarrier :: IO Int64
msBarrier = return 0

msOutput :: IO Int64
msOutput = do
  arr <- readIORef g_arr_ms
  let goCs !i !acc
        | i >= msN  = return ((acc :: Word64) .&. 0xFFFFFFFF)
        | otherwise = do
            !x <- peekElemOff arr i
            when (i > 0) $ do
              !y <- peekElemOff arr (i - 1)
              when (x < y) $ error ("not sorted at i=" ++ show i)
            let !w = fromIntegral (fromIntegral x :: Word32) :: Word64
            goCs (i + 1) ((acc + w) .&. 0xFFFFFFFF)
  !cs <- goCs 0 0
  putStrLn ("CHECKSUM=" ++ show cs)
  return (fromIntegral cs :: Int64)
"""


def node_var(kind, idx):
    return f"l_{idx}" if kind == "leaf" else f"m_{idx}"


def emit_fl(out_dir, leaves, merges, root):
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('leaf',    {SUPER_LEAF},    1, False, False)",
        f"superinst('merge',   {SUPER_MERGE},   1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('leaf', 100)",
        "avgtime('merge', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for i in range(len(leaves)):
        fl_lines.append(f"const lk_{i}, {i}")
        fl_lines.append(f"leaf l_{i}, ini, lk_{i}")
    for i, (lo, mid, hi, left, right) in enumerate(merges):
        fl_lines.append(f"const mk_{i}, {i}")
        fl_lines.append(f"merge m_{i}, {node_var(*left)}, {node_var(*right)}, mk_{i}")
    fl_lines.append(f"output out, {node_var(*root)}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")


def emit(out_dir, data_dir, N, cutoff):
    os.makedirs(out_dir, exist_ok=True)
    leaves, merges, root = build_tree(N, cutoff)
    # tree.bin owned by gen_input.py
    emit_fl(out_dir, leaves, merges, root)

    hsk_path = os.path.join(out_dir, "attn.hsk")
    with open(hsk_path, "w") as f:
        f.write(HSK_TEMPLATE)
    print(f"[gen_ms_hs] wrote {hsk_path}")

    print(f"[gen_ms_hs] wrote {out_dir}/attn.fl  (leaves={len(leaves)} merges={len(merges)})")

    inject_src = (INJECT_TEMPLATE
                  .replace("__N__", str(N))
                  .replace("__CUTOFF__", str(cutoff))
                  .replace("__N_LEAVES__", str(len(leaves)))
                  .replace("__N_MERGES__", str(len(merges)))
                  .replace("__DATA_DIR__", data_dir))
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    with open(inject_path, "w") as f:
        f.write(inject_src)
    print(f"[gen_ms_hs] wrote {inject_path}")


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
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["N"], cfg["CUTOFF"])


if __name__ == "__main__":
    main()
