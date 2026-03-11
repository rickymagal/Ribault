#!/usr/bin/env python3
"""Generate TALM .hsk for LCS benchmark.

K independent supers, each taking a chunk index (0..K-1).
Each super computes LCS for its assigned string pairs and returns the
sum of LCS lengths.  Main sums all chunk results via a balanced binary tree.

Same architecture as the knapsack benchmark: independent range supers
with a balanced reduction tree in the dataflow graph.
"""

import argparse, os


MAX_CALL_SITES = 63  # tagRadix=64 in codegen


def _balanced_sum(vars):
    """Build a balanced binary tree of + operations."""
    if len(vars) == 0:
        return "0"
    if len(vars) == 1:
        return vars[0]
    if len(vars) == 2:
        return f"({vars[0]} + {vars[1]})"
    mid = len(vars) // 2
    left = _balanced_sum(vars[:mid])
    right = _balanced_sum(vars[mid:])
    return f"({left} + {right})"


def emit_hsk(path, input_dir, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        parts = f.read().split()
        n_pairs = int(parts[0])
        str_len = int(parts[1])
        alphabet = int(parts[2])
        seed = int(parts[3])

    n_funcs = min(n_funcs, MAX_CALL_SITES, n_pairs)

    # Verify each chunk is non-empty
    chunks = []
    for i in range(n_funcs):
        lo = i * n_pairs // n_funcs
        hi = (i + 1) * n_pairs // n_funcs
        if hi > lo:
            chunks.append(i)
    n_funcs = len(chunks)

    lines = []
    lines.append(f"-- lcs.hsk  (auto-generated)")
    lines.append(f"-- N_PAIRS={n_pairs}  STR_LEN={str_len}  ALPHABET={alphabet}")
    lines.append(f"-- N_FUNCS={n_funcs}  SEED={seed}")
    lines.append(f"-- LCS benchmark: sum of LCS lengths for {n_pairs} string pairs")
    lines.append("")

    # K range-processing supers
    for i in range(n_funcs):
        lines.append(f"pf{i} chunkIdx =")
        lines.append(f"  super single input (chunkIdx) output (result)")
        lines.append("#BEGINSUPER")
        lines.append(f"    result = unsafePerformIO $ lcsSearchChunk (fromIntegral chunkIdx)")
        lines.append("#ENDSUPER")
        lines.append("")

    # print_result super
    lines.append("print_result r =")
    lines.append("  super single input (r) output (out)")
    lines.append("#BEGINSUPER")
    lines.append('    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show r); hFlush stdout; pure 0)')
    lines.append("#ENDSUPER")
    lines.append("")

    # Main: nested lets + balanced sum tree
    lines.append("main =")
    for i in range(n_funcs):
        lines.append(f"  let r{i} = pf{i} {i}")
        lines.append(f"  in")

    vars = [f"r{i}" for i in range(n_funcs)]
    sum_expr = _balanced_sum(vars)
    lines.append(f"  let total = {sum_expr}")
    lines.append(f"  in print_result total")

    hsk = "\n".join(lines) + "\n"
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_lcs_talm] wrote {path}  (n_pairs={n_pairs}, n_funcs={n_funcs})")

    # Generate supers_inject.hs
    inject_path = os.path.join(os.path.dirname(path) or ".", "supers_inject.hs")

    inject = f"""import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

-- LCS benchmark helpers (injected by gen_talm_input.py)

lcsTotalPairs :: Int
lcsTotalPairs = {n_pairs}

lcsStrLen :: Int
lcsStrLen = {str_len}

lcsAlphabetSize :: Int
lcsAlphabetSize = {alphabet}

lcsNFuncs :: Int
lcsNFuncs = {n_funcs}

lcsSeed :: Word64
lcsSeed = {seed}

-- LCG PRNG using Word64 (zero GMP allocation)
lcsNextRng :: Word64 -> Word64
lcsNextRng r = (6364136223846793005 * r + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF

lcsSkipRng :: Word64 -> Int -> Word64
lcsSkipRng !r 0 = r
lcsSkipRng !r n = lcsSkipRng (lcsNextRng r) (n - 1)

-- Generate string directly into UArray (no list allocation)
lcsGenArray :: Word64 -> Int -> Int -> (UArray Int Int, Word64)
lcsGenArray !rng0 len_ alpha = runST $ do
  arr <- newArray (0, len_ - 1) 0 :: ST s (STUArray s Int Int)
  let go !i !r
        | i >= len_ = return r
        | otherwise = do
            let !r' = lcsNextRng r
                !c  = fromIntegral ((r' `shiftR` 33) `mod` fromIntegral alpha)
            writeArray arr i c
            go (i + 1) r'
  rng' <- go 0 rng0
  frozen <- unsafeFreeze arr
  return (frozen, rng')

-- LCS DP on UArrays (zero allocation in inner loop)
lcsLen :: UArray Int Int -> UArray Int Int -> Int
lcsLen xarr yarr = runST $ do
  let (_, mx) = bounds xarr
      (_, my) = bounds yarr
      !m = mx + 1
      !n = my + 1
  a0 <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  a1 <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  pRef <- newSTRef a0
  cRef <- newSTRef a1
  let outer !i
        | i >= m    = do p <- readSTRef pRef; readArray p n
        | otherwise = do
            p <- readSTRef pRef
            c <- readSTRef cRef
            writeArray c 0 0
            let !xi = xarr ! i
            let inner !j
                  | j >= n    = return ()
                  | otherwise = do
                      if xi == yarr ! j
                        then do !d <- readArray p j
                                writeArray c (j+1) (d + 1)
                        else do !a <- readArray p (j+1)
                                !l <- readArray c j
                                writeArray c (j+1) (max a l)
                      inner (j+1)
            inner 0
            writeSTRef pRef c
            writeSTRef cRef p
            outer (i+1)
  outer 0

-- Process a chunk of pairs: compute LCS for pairs [lo, hi), return sum.
lcsSearchChunk :: Int -> IO Int64
lcsSearchChunk chunkIdx = do
  let lo  = chunkIdx * lcsTotalPairs `div` lcsNFuncs
      hi  = (chunkIdx + 1) * lcsTotalPairs `div` lcsNFuncs
      rng0 = lcsSkipRng lcsSeed (lo * 2 * lcsStrLen)
      go !_ 0 !acc = acc
      go !rng np !acc =
        let (a, rng1) = lcsGenArray rng  lcsStrLen lcsAlphabetSize
            (b, rng2) = lcsGenArray rng1 lcsStrLen lcsAlphabetSize
            !l        = lcsLen a b
        in go rng2 (np - 1) (acc + l)
      !result = go rng0 (hi - lo) 0
  return (fromIntegral result)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_lcs_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--n-funcs", type=int, default=32,
                    help="Number of independent range supers (max 63)")
    args = ap.parse_args()
    emit_hsk(args.out, args.input_dir, args.n_funcs)


if __name__ == "__main__":
    main()
