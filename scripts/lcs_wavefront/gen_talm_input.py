#!/usr/bin/env python3
"""Generate TALM .hsk for LCS wavefront benchmark.

Wavefront parallelism: DIM×DIM block grid over the DP matrix.
Block(i,j) depends on block(i-1,j) and block(i,j-1).
Dependencies for interior blocks are joined via addition (both must
complete before the block_super fires).

Three supers:
  init_super   — generate sequences, allocate shared score matrix
  block_super  — compute one block of the DP matrix
  result_super — read final score, print RESULT=
"""

import argparse, os


MAX_CALL_SITES = 63


def emit_hsk(path, input_dir, dim):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len = int(parts[0])
        alphabet = int(parts[1])
        seed = int(parts[2])

    # Each block is its own super type, plus init + result
    n_blocks = dim * dim
    # We need: 1 (init) + n_blocks (one per block) + 1 (result) call sites
    if n_blocks + 2 > MAX_CALL_SITES:
        # Reduce dim to fit
        import math
        dim = int(math.floor(math.sqrt(MAX_CALL_SITES - 2)))
        n_blocks = dim * dim
        print(f"[gen_lcs_wf_talm] WARNING: reduced DIM to {dim} (max call sites)")

    lines = []
    lines.append(f"-- lcs_wavefront.hsk  (auto-generated)")
    lines.append(f"-- N={seq_len}  ALPHA={alphabet}  SEED={seed}  DIM={dim}")
    lines.append(f"-- Wavefront LCS: {dim}x{dim} block grid")
    lines.append("")

    # init_super: generate sequences + allocate matrix
    lines.append("init_super seed =")
    lines.append("  super single input (seed) output (state)")
    lines.append("#BEGINSUPER")
    lines.append("    state = unsafePerformIO (lcsInit (fromIntegral seed))")
    lines.append("#ENDSUPER")
    lines.append("")

    # Per-block supers: each has the block index hardcoded
    for i in range(dim):
        for j in range(dim):
            idx = i * dim + j
            lines.append(f"block_{idx}_super dep =")
            lines.append(f"  super single input (dep) output (result)")
            lines.append(f"#BEGINSUPER")
            lines.append(f"    result = unsafePerformIO (lcsBlock {idx})")
            lines.append(f"#ENDSUPER")
            lines.append(f"")

    # result_super: read and print final score
    lines.append("result_super dep =")
    lines.append("  super single input (dep) output (out)")
    lines.append("#BEGINSUPER")
    lines.append('    out = unsafePerformIO (lcsResult dep)')
    lines.append("#ENDSUPER")
    lines.append("")

    # Main: unrolled wavefront with dependency joins via +
    lines.append("main =")
    lines.append("  let s = init_super 0")

    # Generate block let-bindings
    def bname(i, j):
        return f"b_{i}_{j}"

    for i in range(dim):
        for j in range(dim):
            idx = i * dim + j
            bn = bname(i, j)
            if i == 0 and j == 0:
                lines.append(f"      {bn} = block_{idx}_super s")
            elif i == 0:
                lines.append(f"      {bn} = block_{idx}_super {bname(i, j-1)}")
            elif j == 0:
                lines.append(f"      {bn} = block_{idx}_super {bname(i-1, j)}")
            else:
                # Join two dependencies via +
                lines.append(f"      {bn} = block_{idx}_super ({bname(i-1, j)} + {bname(i, j-1)})")

    last = bname(dim - 1, dim - 1)
    lines.append(f"  in result_super {last}")

    hsk = "\n".join(lines) + "\n"
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_lcs_wf_talm] wrote {path}  (N={seq_len}, DIM={dim})")

    # Generate supers_inject.hs
    inject_path = os.path.join(os.path.dirname(path) or ".", "supers_inject.hs")

    inject = f"""import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Constants
lcsSeqLen :: Int
lcsSeqLen = {seq_len}

lcsAlpha :: Int
lcsAlpha = {alphabet}

lcsSeed :: Word64
lcsSeed = {seed}

lcsDim :: Int
lcsDim = {dim}

-- LCG PRNG (Word64, zero GMP)
lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

-- Generate sequence into UArray
lcsGenSeq :: Word64 -> Int -> Int -> (UArray Int Int, Word64)
lcsGenSeq !rng0 len_ alpha = runST $ do
  arr <- Data.Array.ST.newArray (0, len_ - 1) 0 :: ST s (STUArray s Int Int)
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha)
            Data.Array.ST.writeArray arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  frozen <- unsafeFreeze arr
  return (frozen, rng')

-- Global shared state
data LCSGlobal = LCSGlobal
  {{ lcsA :: !(UArray Int Int)
  , lcsB :: !(UArray Int Int)
  , lcsMat :: !(IOUArray (Int,Int) Int)
  }}

{{-# NOINLINE globalLCS #-}}
globalLCS :: IORef (Maybe LCSGlobal)
globalLCS = unsafePerformIO (newIORef Nothing)

-- Init: generate sequences, allocate matrix
lcsInit :: Int -> IO Int64
lcsInit _ = do
  let (seqA, rng1) = lcsGenSeq lcsSeed lcsSeqLen lcsAlpha
      (seqB, _)    = lcsGenSeq rng1    lcsSeqLen lcsAlpha
  mat <- Data.Array.IO.newArray ((0,0), (lcsSeqLen, lcsSeqLen)) 0
  writeIORef globalLCS (Just (LCSGlobal seqA seqB mat))
  return 0

-- Block computation: compute block (bi, bj) of the DP matrix
-- Matrix is (0..N) x (0..N), row 0 and col 0 are base cases (0).
-- Block (bi,bj) covers rows [bi*chunk+1 .. (bi+1)*chunk] and
-- cols [bj*chunk+1 .. (bj+1)*chunk], where chunk = N/DIM.
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
      !sa = lcsA g
      !sb = lcsB g
      !mat = lcsMat g
  let outerLoop !i
        | i > rowEnd = return ()
        | otherwise = do
            let innerLoop !j
                  | j > colEnd = return ()
                  | otherwise = do
                      let !ai = sa ! (i - 1)
                          !bj' = sb ! (j - 1)
                      if ai == bj'
                        then do
                          !d <- Data.Array.IO.readArray mat (i-1, j-1)
                          Data.Array.IO.writeArray mat (i, j) (d + 1)
                        else do
                          !u <- Data.Array.IO.readArray mat (i-1, j)
                          !l <- Data.Array.IO.readArray mat (i, j-1)
                          Data.Array.IO.writeArray mat (i, j) (max u l)
                      innerLoop (j + 1)
            innerLoop colStart
            outerLoop (i + 1)
  outerLoop rowStart
  return 0

-- Result: read final score and print
lcsResult :: Int64 -> IO Int64
lcsResult _ = do
  Just g <- readIORef globalLCS
  !score <- Data.Array.IO.readArray (lcsMat g) (lcsSeqLen, lcsSeqLen)
  putStrLn ("RESULT=" ++ show score)
  hFlush stdout
  return 0
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_lcs_wf_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--dim", type=int, default=6,
                    help="Block grid dimension (DIM×DIM blocks)")
    args = ap.parse_args()
    emit_hsk(args.out, args.input_dir, args.dim)


if __name__ == "__main__":
    main()
