#!/usr/bin/env python3
"""Generate TALM files for LCS wavefront benchmark.

Generates three files:
  1. A minimal .hsk with super definitions (for supersgen / build_supers.sh)
  2. A preprocessor .fl using flowasm macros (bypasses codegen for the
     dataflow graph — no call-site limit on DIM)
  3. supers_inject.hs with the Haskell super implementations

The .fl uses the flowasm preprocessor loop macros to unroll the
wavefront dependency graph, following the same pattern as the original
hand-written LCS benchmark.

Block super uses `superi` (super with immediate) to pass the block
index.  The Haskell super retrieves it via FFI to `treb_get_tid()`.

Super IDs (from codegen on the minimal .hsk, verified stable):
  init_super   → super 6  (s6)
  block_super  → super 5  (s5)
  result_super → super 4  (s4)
"""

import argparse, os


# Super IDs assigned by the codegen for our 3-super .hsk.
# Verified stable: init=6, block=5, result=4.
SUPER_INIT   = 6
SUPER_BLOCK  = 5
SUPER_RESULT = 4


def emit(path, input_dir, dim, iters=1):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])

    # ---- 1. Minimal .hsk (for supersgen → Supers.hs) ----
    hsk_lines = [
        "-- lcs_wavefront.hsk  (auto-generated, minimal for supersgen)",
        f"-- N={seq_len}  ALPHA={alphabet}  SEED={seed}  DIM={dim}",
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
    print(f"[gen_lcs_wf_talm] wrote {path}  (N={seq_len}, DIM={dim})")

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
        return f"blck{i * dim + j}"

    # First row: depends on left neighbor only
    for j in range(1, dim):
        idx = j
        fl_lines.append(f"block {bname(0, j)}, {bname(0, j-1)}, {idx}")

    # First column: depends on top neighbor only
    for i in range(1, dim):
        idx = i * dim
        fl_lines.append(f"block {bname(i, 0)}, {bname(i-1, 0)}, {idx}")

    # Interior blocks: 2 inputs (top + left) + immediate block index
    for i in range(1, dim):
        for j in range(1, dim):
            idx = i * dim + j
            fl_lines.append(
                f"block {bname(i, j)}, {bname(i-1, j)}, {bname(i, j-1)}, {idx}"
            )

    fl_lines.append(f"output out, {bname(dim-1, dim-1)}")

    with open(fl_path, "w", encoding="utf-8") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_lcs_wf_talm] wrote {fl_path}  ({dim*dim} blocks)")

    # ---- 3. supers_inject.hs ----
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    stride = seq_len + 1  # row stride for flat matrix indexing
    inject = f"""import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (callocBytes)
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

lcsDim :: Int
lcsDim = {dim}

lcsStride :: Int
lcsStride = {stride}

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

-- Global shared state (all Ptr-based, outside GHC heap)
data LCSGlobal = LCSGlobal
  {{ lcsA   :: !(Ptr Int)
  , lcsB   :: !(Ptr Int)
  , lcsMat :: !(Ptr Int)   -- flat (N+1)*(N+1) matrix, row-major
  }}

{{-# NOINLINE globalLCS #-}}
globalLCS :: IORef (Maybe LCSGlobal)
globalLCS = unsafePerformIO (newIORef Nothing)

-- Init: generate sequences, allocate matrix
lcsInit :: Int -> IO Int64
lcsInit _ = do
  (seqA, rng1) <- lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
  (seqB, _)    <- lcsGenSeq rng1    lcsSeqLen lcsAlpha
  mat <- callocBytes (lcsStride * lcsStride * 8)  -- (N+1)^2 ints, zeroed
  writeIORef globalLCS (Just (LCSGlobal seqA seqB mat))
  return 0

-- Block computation using treb_get_tid() for block index
lcsBlockTid :: Int64 -> IO Int64
lcsBlockTid _ = do
  blockIdx <- fromIntegral <$> c_treb_get_tid
  lcsBlock blockIdx

-- Block computation: compute block (bi, bj) of the DP matrix
lcsBlock :: Int -> IO Int64
lcsBlock blockIdx = do
  Just g <- readIORef globalLCS
  let !bi = blockIdx `div` lcsDim
      !bj = blockIdx `mod` lcsDim
      !n  = lcsSeqLen
      !chunkR = n `div` lcsDim
      !chunkC = n `div` lcsDim
      !rowStart = bi * chunkR + 1
      !rowEnd   = if bi == lcsDim - 1 then n else (bi + 1) * chunkR
      !colStart = bj * chunkC + 1
      !colEnd   = if bj == lcsDim - 1 then n else (bj + 1) * chunkC
      !sa  = lcsA g
      !sb  = lcsB g
      !mat = lcsMat g
      !str = lcsStride
  let outerLoop !i
        | i > rowEnd = return ()
        | otherwise = do
            !ai <- peekElemOff sa (i - 1)
            let innerLoop !j
                  | j > colEnd = return ()
                  | otherwise = do
                      !bj' <- peekElemOff sb (j - 1)
                      if ai == bj'
                        then do
                          !d <- peekElemOff mat ((i-1)*str + (j-1))
                          pokeElemOff mat (i*str + j) (d + 1)
                        else do
                          !u <- peekElemOff mat ((i-1)*str + j)
                          !l <- peekElemOff mat (i*str + (j-1))
                          pokeElemOff mat (i*str + j) (max u l)
                      innerLoop (j + 1)
            innerLoop colStart
            outerLoop (i + 1)
  outerLoop rowStart
  return 0

-- Result: read final score and print
lcsResult :: Int64 -> IO Int64
lcsResult _ = do
  Just g <- readIORef globalLCS
  !score <- peekElemOff (lcsMat g) (lcsSeqLen * lcsStride + lcsSeqLen)
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
    ap.add_argument("--dim", type=int, default=6,
                    help="Block grid dimension (DIM×DIM blocks)")
    ap.add_argument("--iters", type=int, default=1,
                    help="Iterations per block (multiply work)")
    args = ap.parse_args()
    emit(args.out, args.input_dir, args.dim, args.iters)


if __name__ == "__main__":
    main()
