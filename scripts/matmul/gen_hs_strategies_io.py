#!/usr/bin/env python3
"""Generate GHC Strategies matmul .hs that reads matrices from binary files.

Each block independently reads both A.bin and BT.bin via BS.readFile,
which allocates on the GHC heap, creating GC pressure under parallelism.
"""

import argparse, os


def emit_hs(path, N, n_funcs, data_dir):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    n_funcs = min(n_funcs, N)

    ranges = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            ranges.append((lo, hi))

    range_list = "[" + ", ".join(f"({lo}, {hi})" for lo, hi in ranges) + "]"

    src = f"""\
{{-# LANGUAGE BangPatterns #-}}
-- Auto-generated: MatMul GHC Strategies (file IO via BS.readFile)
-- N={N}  N_FUNCS={len(ranges)}

import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekElemOff)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

n :: Int
n = {N}

aFile :: FilePath
aFile = "{data_dir}/A.bin"

btFile :: FilePath
btFile = "{data_dir}/BT.bin"

-- Dot product: A row i with BT row k (both from flat Ptr Double arrays)
dot :: Ptr Double -> Ptr Double -> Int -> Int -> IO Double
dot !aP !btP !i !k = go 0 0.0
  where
    !aBase = i * n
    !bBase = k * n
    go !j !acc
      | j >= n    = return acc
      | otherwise = do
          !a <- peekElemOff aP (aBase + j)
          !b <- peekElemOff btP (bBase + j)
          go (j + 1) (acc + a * b)

-- Process block: BS.readFile for both matrices (allocates on GHC heap)
processBlock :: Int -> Int -> IO Int64
processBlock !lo !hi = do
  let !rows = hi - lo
  -- BS.readFile allocates strict ByteString on GHC heap
  aBS  <- BS.readFile aFile
  btBS <- BS.readFile btFile
  let !(BSI.BS afp alen)  = aBS
      !(BSI.BS bfp blen)  = btBS
  withForeignPtr afp $ \\aRaw ->
    withForeignPtr bfp $ \\bRaw -> do
      let !aP  = castPtr aRaw :: Ptr Double
          !btP = castPtr bRaw :: Ptr Double
      blockSum aP btP lo rows 0 0 0.0
  where
    blockSum !aP !btP !rowOff !rows !ri !k !acc
      | ri >= rows = return $! truncate (acc * 1000000 :: Double)
      | k >= n     = blockSum aP btP rowOff rows (ri + 1) 0 acc
      | otherwise  = do
          !d <- dot aP btP (rowOff + ri) k
          blockSum aP btP rowOff rows ri (k + 1) (acc + d)

{{-# NOINLINE evalBlock #-}}
evalBlock :: (Int, Int) -> Int64
evalBlock (lo, hi) = unsafePerformIO $ processBlock lo hi

main :: IO ()
main = do
  t0 <- getCurrentTime
  let ranges = {range_list}
      !results = parMap rdeepseq evalBlock ranges
      !total = sum results
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "CHECKSUM=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_matmul_strat_io] wrote {path} (N={N}, n_funcs={len(ranges)})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
