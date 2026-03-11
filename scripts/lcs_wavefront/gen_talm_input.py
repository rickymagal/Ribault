#!/usr/bin/env python3
"""Generate TALM files for LCS wavefront benchmark.

Generates three files:
  1. A minimal .hsk with super definitions (for supersgen / build_supers.sh)
  2. A preprocessor .fl using flowasm macros (bypasses codegen for the
     dataflow graph -- no call-site limit on DIM)
  3. supers_inject.hs with the Haskell super implementations

Memory-efficient: stores only block boundary rows/columns instead of the
full N*N matrix.  Memory: O((DIM_ROWS + DIM_COLS) * N) instead of O(N^2).

Supports rectangular grids (DIM_ROWS x DIM_COLS) for tuning grain size
independently from parallelism level.

Block super uses `superi` (super with immediate) to pass the block
index.  The Haskell super retrieves it via FFI to `treb_get_tid()`.

Super IDs (from codegen on the minimal .hsk, verified stable):
  init_super   -> super 6  (s6)
  block_super  -> super 5  (s5)
  result_super -> super 4  (s4)
"""

import argparse, os


# Super IDs assigned by the codegen for our 3-super .hsk.
# Verified stable: init=6, block=5, result=4.
SUPER_INIT   = 6
SUPER_BLOCK  = 5
SUPER_RESULT = 4


def emit(path, input_dir, dim_rows, dim_cols):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])

    # ---- 1. Minimal .hsk (for supersgen -> Supers.hs) ----
    hsk_lines = [
        "-- lcs_wavefront.hsk  (auto-generated, minimal for supersgen)",
        f"-- N={seq_len}  ALPHA={alphabet}  SEED={seed}  ROWS={dim_rows}  COLS={dim_cols}",
        "",
        "init_super seed =",
        "  super single input (seed) output (state)",
        "#BEGINSUPER",
        "    state = unsafePerformIO (lcsInit (fromIntegral seed))",
        "#ENDSUPER",
        "",
        "block_super dep =",
        "  super single input (dep) output (result)",
        "#BEGINSUPER",
        "    result = unsafePerformIO (lcsBlockTid dep)",
        "#ENDSUPER",
        "",
        "result_super dep =",
        "  super single input (dep) output (out)",
        "#BEGINSUPER",
        "    out = unsafePerformIO (lcsResult dep)",
        "#ENDSUPER",
        "",
        "main =",
        "  let s = init_super 0",
        "      b = block_super s",
        "  in result_super b",
    ]
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(hsk_lines) + "\n")
    print(f"[gen_lcs_wf_talm] wrote {path}  (N={seq_len}, {dim_rows}x{dim_cols})")

    # ---- 2. Flowasm .fl (pre-expanded, bypasses codegen) ----
    fl_path = os.path.join(out_dir, "lcs_wf.fl")
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('block',  {SUPER_BLOCK},  1, False, True)",
        f"superinst('output', {SUPER_RESULT}, 1, False, False)",
        f"avgtime('block', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
        "block blck0, ini, 0",
    ]

    def bname(i, j):
        return f"blck{i * dim_cols + j}"

    # First row: depends on left neighbor only
    for j in range(1, dim_cols):
        idx = j
        fl_lines.append(f"block {bname(0, j)}, {bname(0, j-1)}, {idx}")

    # First column: depends on top neighbor only
    for i in range(1, dim_rows):
        idx = i * dim_cols
        fl_lines.append(f"block {bname(i, 0)}, {bname(i-1, 0)}, {idx}")

    # Interior blocks: 2 inputs (top + left) + immediate block index
    for i in range(1, dim_rows):
        for j in range(1, dim_cols):
            idx = i * dim_cols + j
            fl_lines.append(
                f"block {bname(i, j)}, {bname(i-1, j)}, {bname(i, j-1)}, {idx}"
            )

    fl_lines.append(f"output out, {bname(dim_rows-1, dim_cols-1)}")

    total_blocks = dim_rows * dim_cols
    with open(fl_path, "w", encoding="utf-8") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_lcs_wf_talm] wrote {fl_path}  ({total_blocks} blocks, {dim_rows}x{dim_cols})")

    # ---- 3. supers_inject.hs ----
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject = f"""import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (callocBytes, allocaBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)

-- FFI: get immediate value from superi instruction
foreign import ccall unsafe "treb_get_tid" c_treb_get_tid :: IO CInt

-- Constants
lcsSeqLen :: Int
lcsSeqLen = {seq_len}

lcsAlpha :: Int
lcsAlpha = {alphabet}

lcsSeed :: Word64
lcsSeed = {seed}

lcsDimRows :: Int
lcsDimRows = {dim_rows}

lcsDimCols :: Int
lcsDimCols = {dim_cols}

-- LCG PRNG (Word64, zero GMP)
lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

-- Generate sequence into Ptr Int (C-allocated, outside GHC heap)
lcsGenSeq :: Word64 -> Int -> Int -> IO (Ptr Int, Word64)
lcsGenSeq !rng0 len_ alpha = do
  arr <- callocBytes (len_ * 8)
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha) :: Int
            pokeElemOff arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  return (arr, rng')

-- Global shared state (boundary arrays instead of full matrix)
data LCSGlobal = LCSGlobal
  {{ lcsA      :: !(Ptr Int)
  , lcsB      :: !(Ptr Int)
  , lcsHBound :: !(Ptr Int)  -- (DIM_ROWS+1) rows, each (N+1) Ints
  , lcsVBound :: !(Ptr Int)  -- (DIM_COLS+1) cols, each (N+1) Ints
  }}

{{-# NOINLINE globalLCS #-}}
globalLCS :: IORef (Maybe LCSGlobal)
globalLCS = unsafePerformIO (newIORef Nothing)

-- Init: generate sequences, allocate boundary arrays (zeroed)
lcsInit :: Int -> IO Int64
lcsInit _ = do
  let !n = lcsSeqLen
      !cols = n + 1
  (seqA, rng1) <- lcsGenSeq lcsSeed n lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    n lcsAlpha
  hBound <- callocBytes ((lcsDimRows + 1) * cols * 8)
  vBound <- callocBytes ((lcsDimCols + 1) * cols * 8)
  writeIORef globalLCS (Just (LCSGlobal seqA seqB hBound vBound))
  return 0

-- Block computation using treb_get_tid() for block index
lcsBlockTid :: Int64 -> IO Int64
lcsBlockTid _ = do
  blockIdx <- fromIntegral <$> c_treb_get_tid
  lcsBlock blockIdx

-- Block computation: compute block (bi, bj) using boundary arrays
lcsBlock :: Int -> IO Int64
lcsBlock blockIdx = do
  Just g <- readIORef globalLCS
  let !bi = blockIdx `div` lcsDimCols
      !bj = blockIdx `mod` lcsDimCols
      !n  = lcsSeqLen
      !cols = n + 1
      !chunkR = n `div` lcsDimRows
      !chunkC = n `div` lcsDimCols
      !rowStart = bi * chunkR + 1
      !rowEnd   = if bi == lcsDimRows - 1 then n else (bi + 1) * chunkR
      !colStart = bj * chunkC + 1
      !colEnd   = if bj == lcsDimCols - 1 then n else (bj + 1) * chunkC
      !localCols = colEnd - colStart + 1
      !sa  = lcsA g
      !sb  = lcsB g
      !hB  = lcsHBound g
      !vB  = lcsVBound g
      -- Pre-computed offsets (loop-invariant)
      !hBReadBase  = bi * cols + colStart - 1
      !hBWriteBase = (bi + 1) * cols + colStart - 1
      !vBReadBase  = bj * cols
      !vBWriteBase = (bj + 1) * cols
      !sbBase      = colStart - 2
      !rowBytes    = (localCols + 1) * 8
  allocaBytes rowBytes $ \\buf1 ->
    allocaBytes rowBytes $ \\buf2 -> do
      -- Initialize buf1 from hBound[bi]
      let initPrev !lj
            | lj > localCols = return ()
            | otherwise = do
                !v <- peekElemOff hB (hBReadBase + lj)
                pokeElemOff buf1 lj v
                initPrev (lj + 1)
      initPrev 0
      -- Process rows
      let outerLoop !prev !cur !i
            | i > rowEnd = do
                -- Write prev -> hBound[bi+1]
                let writeFinal !lj
                      | lj > localCols = return ()
                      | otherwise = do
                          !v <- peekElemOff prev lj
                          pokeElemOff hB (hBWriteBase + lj) v
                          writeFinal (lj + 1)
                writeFinal 0
            | otherwise = do
                !leftVal <- peekElemOff vB (vBReadBase + i)
                pokeElemOff cur 0 leftVal
                !ai <- peekElemOff sa (i - 1)
                let innerLoop !lj
                      | lj > localCols = return ()
                      | otherwise = do
                          !bj' <- peekElemOff sb (sbBase + lj)
                          if ai == bj'
                            then do
                              !d <- peekElemOff prev (lj - 1)
                              pokeElemOff cur lj (d + 1)
                            else do
                              !u <- peekElemOff prev lj
                              !l <- peekElemOff cur (lj - 1)
                              pokeElemOff cur lj (max u l)
                          innerLoop (lj + 1)
                innerLoop 1
                !rightVal <- peekElemOff cur localCols
                pokeElemOff vB (vBWriteBase + i) rightVal
                outerLoop cur prev (i + 1)
      outerLoop buf1 buf2 rowStart
  return 0

-- Result: read final LCS score from hBound[DIM_ROWS][N]
lcsResult :: Int64 -> IO Int64
lcsResult _ = do
  Just g <- readIORef globalLCS
  !score <- peekElemOff (lcsHBound g) (lcsDimRows * (lcsSeqLen + 1) + lcsSeqLen)
  putStrLn ("RESULT=" ++ show (score :: Int))
  hFlush stdout
  return 0
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_lcs_wf_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True,
                    help="Output .hsk path (minimal, for supersgen)")
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim-rows", type=int, default=None,
                    help="Row dimension of block grid")
    ap.add_argument("--dim-cols", type=int, default=None,
                    help="Column dimension of block grid")
    ap.add_argument("--dim", type=int, default=6,
                    help="Square grid dimension (used if --dim-rows/--dim-cols not set)")
    args = ap.parse_args()
    dim_rows = args.dim_rows if args.dim_rows is not None else args.dim
    dim_cols = args.dim_cols if args.dim_cols is not None else args.dim
    emit(args.out, args.input_dir, dim_rows, dim_cols)


if __name__ == "__main__":
    main()
