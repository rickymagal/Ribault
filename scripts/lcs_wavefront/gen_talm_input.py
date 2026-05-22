#!/usr/bin/env python3
"""Generate TALM files for LCS wavefront benchmark.

Generates three files:
  1. A minimal .hsk with super definitions (for supersgen / build_supers.sh)
  2. A preprocessor .fl using flowasm macros (bypasses codegen for the
     dataflow graph -- no call-site limit on DIM)
  3. supers_inject.hs with the Haskell super implementations

Memory-efficient: stores only block boundary rows/columns instead of the
full N*N matrix.  Memory: O((DIM_ROWS + DIM_COLS) * N) instead of O(N^2).

Supports rectangular grids (DIM_ROWS x DIM_COLS).

Design: block index is passed as a REGULAR DATAFLOW INPUT (via a const node
routed to each block super), not as an immediate. The Haskell super receives
the block index as a normal Int64 argument — no FFI back to the runtime via
treb_get_tid is needed. This keeps the entire per-block computation in
Haskell, which is the user's stated invariant.

Because boundary blocks have 1 sync dep + idx (2 inputs) and interior blocks
have 2 sync deps + idx (3 inputs), the runtime needs two block super arities.
The Haskell body is identical for both — they just declare different arities
so the firing rule waits for the correct number of tokens.

Super IDs assigned by supersgen (user-defined supers come after the built-ins
s0..s9, in REVERSE declaration order):
  init_super    (decl 1st) -> s13
  block1_super  (decl 2nd) -> s12   (single-dep + idx)
  block2_super  (decl 3rd) -> s11   (double-dep + idx)
  result_super  (decl 4th) -> s10
"""

import argparse, os


SUPER_INIT    = 13
SUPER_BLOCK1  = 12   # 2 inputs: dep + idx (boundary blocks)
SUPER_BLOCK2  = 11   # 3 inputs: top + left + idx (interior blocks)
SUPER_RESULT  = 10


def emit(path, input_dir, dim_rows, dim_cols):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])

    # ---- 1. Minimal .hsk (for supersgen -> Supers.hs) ----
    # NEW super syntax: super <implName> <args...> ( <implName> <args...> = <body> )
    # block1_super: 1 sync dep + 1 idx                (used for boundary blocks)
    # block2_super: 2 sync deps + 1 idx               (used for interior blocks)
    # The sync deps' values are unused — they only serve as firing tokens. The
    # block index is the only argument that actually drives computation.
    hsk_lines = [
        "-- lcs_wavefront.hsk  (auto-generated, minimal for supersgen)",
        f"-- N={seq_len}  ALPHA={alphabet}  SEED={seed}  ROWS={dim_rows}  COLS={dim_cols}",
        "",
        "init_super seed =",
        "  super initImpl seed (",
        "    initImpl seed = unsafePerformIO (lcsInit (fromIntegral seed))",
        "  )",
        "",
        "block1_super dep idx =",
        "  super blockImpl1 dep idx (",
        "    blockImpl1 dep idx = unsafePerformIO (lcsBlock (fromIntegral idx))",
        "  )",
        "",
        "block2_super top left idx =",
        "  super blockImpl2 top left idx (",
        "    blockImpl2 top left idx = unsafePerformIO (lcsBlock (fromIntegral idx))",
        "  )",
        "",
        "result_super dep =",
        "  super resultImpl dep (",
        "    resultImpl dep = unsafePerformIO (lcsResult dep)",
        "  )",
        "",
        "main =",
        "  let s = init_super 0",
        "      b = block1_super s 0",
        "      c = block2_super s b 0",
        "  in result_super c",
    ]
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(hsk_lines) + "\n")
    print(f"[gen_lcs_wf_talm] wrote {path}  (N={seq_len}, {dim_rows}x{dim_cols})")

    # ---- 2. Flowasm .fl (pre-expanded, bypasses codegen) ----
    fl_path = os.path.join(out_dir, "lcs_wf.fl")
    # superinst args: (instname, blocknumber, resnum, isspec, has_immed)
    # resnum = number of OUTPUT operands the super produces (NOT input count!).
    # Input arity is inferred by the assembler from the call sites.
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},    1, False, False)",
        f"superinst('block1', {SUPER_BLOCK1},  1, False, False)",
        f"superinst('block2', {SUPER_BLOCK2},  1, False, False)",
        f"superinst('output', {SUPER_RESULT},  1, False, False)",
        f"avgtime('block1', 10000)",
        f"avgtime('block2', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    def bname(i, j):
        return f"blck{i * dim_cols + j}"

    def kname(i, j):
        return f"k_{i}_{j}"

    # Emit const node for each block index, then call block_superN with it.
    # block1: 1 sync dep + idx (boundary). block2: 2 sync deps + idx (interior).

    # Top-left block (0,0): only dep is `ini`
    fl_lines.append(f"const {kname(0, 0)}, 0")
    fl_lines.append(f"block1 {bname(0, 0)}, ini, {kname(0, 0)}")

    # First row (i=0, j>=1): depends on left neighbor (0, j-1) only
    for j in range(1, dim_cols):
        idx = j
        fl_lines.append(f"const {kname(0, j)}, {idx}")
        fl_lines.append(f"block1 {bname(0, j)}, {bname(0, j-1)}, {kname(0, j)}")

    # First column (i>=1, j=0): depends on top neighbor (i-1, 0) only
    for i in range(1, dim_rows):
        idx = i * dim_cols
        fl_lines.append(f"const {kname(i, 0)}, {idx}")
        fl_lines.append(f"block1 {bname(i, 0)}, {bname(i-1, 0)}, {kname(i, 0)}")

    # Interior blocks: 2 deps (top + left) + idx
    for i in range(1, dim_rows):
        for j in range(1, dim_cols):
            idx = i * dim_cols + j
            fl_lines.append(f"const {kname(i, j)}, {idx}")
            fl_lines.append(
                f"block2 {bname(i, j)}, {bname(i-1, j)}, {bname(i, j-1)}, {kname(i, j)}"
            )

    fl_lines.append(f"output out, {bname(dim_rows-1, dim_cols-1)}")

    total_blocks = dim_rows * dim_cols
    with open(fl_path, "w", encoding="utf-8") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_lcs_wf_talm] wrote {fl_path}  ({total_blocks} blocks, {dim_rows}x{dim_cols})")

    # ---- 3. supers_inject.hs ----
    # No more FFI to treb_get_tid — block index arrives as a normal super input.
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject = f"""import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MV

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

-- LCG PRNG
lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

-- Generate sequence using natural Haskell mutable vector.
lcsGenSeq :: Word64 -> Int -> Int -> IO (MV.IOVector Int, Word64)
lcsGenSeq !rng0 len_ alpha = do
  arr <- MV.replicate len_ 0
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha) :: Int
            MV.write arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  return (arr, rng')

-- Global shared state via IORefs to mutable unboxed vectors.
{{-# NOINLINE g_sa #-}}
g_sa :: IORef (MV.IOVector Int)
g_sa = unsafePerformIO (newIORef =<< MV.new 0)

{{-# NOINLINE g_sb #-}}
g_sb :: IORef (MV.IOVector Int)
g_sb = unsafePerformIO (newIORef =<< MV.new 0)

{{-# NOINLINE g_hBound #-}}
g_hBound :: IORef (MV.IOVector Int)
g_hBound = unsafePerformIO (newIORef =<< MV.new 0)

{{-# NOINLINE g_vBound #-}}
g_vBound :: IORef (MV.IOVector Int)
g_vBound = unsafePerformIO (newIORef =<< MV.new 0)

-- Init: generate sequences, allocate boundary arrays (zeroed).
lcsInit :: Int -> IO Int64
lcsInit _ = do
  let !n = lcsSeqLen
      !cols = n + 1
  (seqA, rng1) <- lcsGenSeq lcsSeed n lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    n lcsAlpha
  hBound <- MV.replicate ((lcsDimRows + 1) * cols) 0
  vBound <- MV.replicate ((lcsDimCols + 1) * cols) 0
  writeIORef g_sa seqA
  writeIORef g_sb seqB
  writeIORef g_hBound hBound
  writeIORef g_vBound vBound
  return 0

-- Block computation. Idiomatic Haskell using Data.Vector.Unboxed.Mutable
-- (the modern Haskell idiom for tight DP loops) — no Foreign.Ptr, no
-- callocBytes, no allocaBytes. As a programmer would write it.
{{-# INLINE lcsBlock #-}}
lcsBlock :: Int -> IO Int64
lcsBlock blockIdx = do
  !sa <- readIORef g_sa
  !sb <- readIORef g_sb
  !hB <- readIORef g_hBound
  !vB <- readIORef g_vBound
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
      !hBReadBase  = bi * cols + colStart - 1
      !hBWriteBase = (bi + 1) * cols + colStart - 1
      !vBReadBase  = bj * cols
      !vBWriteBase = (bj + 1) * cols
      !sbBase      = colStart - 2
  buf1 <- MV.replicate (localCols + 1) 0
  buf2 <- MV.replicate (localCols + 1) 0
  -- Initialize buf1 from hBound[bi]
  let initPrev !lj
        | lj > localCols = return ()
        | otherwise = do
            !v <- MV.read hB (hBReadBase + lj)
            MV.write buf1 lj v
            initPrev (lj + 1)
  initPrev 0
  -- Process rows; swap prev/cur each iteration.
  let outerLoop !prev !cur !i
        | i > rowEnd = do
            -- Write final prev row into hBound[bi+1].
            let writeFinal !lj
                  | lj > localCols = return ()
                  | otherwise = do
                      !v <- MV.read prev lj
                      MV.write hB (hBWriteBase + lj) v
                      writeFinal (lj + 1)
            writeFinal 0
        | otherwise = do
            !leftVal <- MV.read vB (vBReadBase + i)
            MV.write cur 0 leftVal
            !ai <- MV.read sa (i - 1)
            let innerLoop !lj
                  | lj > localCols = return ()
                  | otherwise = do
                      !bj' <- MV.read sb (sbBase + lj)
                      if ai == bj'
                        then do
                          !d <- MV.read prev (lj - 1)
                          MV.write cur lj (d + 1)
                        else do
                          !u <- MV.read prev lj
                          !l <- MV.read cur (lj - 1)
                          MV.write cur lj (max u l)
                      innerLoop (lj + 1)
            innerLoop 1
            !rightVal <- MV.read cur localCols
            MV.write vB (vBWriteBase + i) rightVal
            outerLoop cur prev (i + 1)
  outerLoop buf1 buf2 rowStart
  return 0

-- Result: read final LCS score from hBound[DIM_ROWS][N].
lcsResult :: Int64 -> IO Int64
lcsResult _ = do
  !hB <- readIORef g_hBound
  !score <- MV.read hB (lcsDimRows * (lcsSeqLen + 1) + lcsSeqLen)
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
