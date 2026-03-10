#!/usr/bin/env python3
"""Generate sequential attention .hs (baseline, file IO via BS.readFile).

Single-head self-attention: O = softmax(Q * K^T / sqrt(D)) * V
Each block independently reads Q, K, V from binary files.
"""

import argparse, os


def emit_hs(path, N, D, n_funcs, data_dir):
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
-- Auto-generated: Attention sequential baseline (file IO via BS.readFile)
-- N={N}  D={D}  N_FUNCS={len(ranges)}

import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

seqLen :: Int
seqLen = {N}

dModel :: Int
dModel = {D}

invSqrtD :: Double
invSqrtD = 1.0 / sqrt (fromIntegral dModel)

qFile :: FilePath
qFile = "{data_dir}/Q.bin"

kFile :: FilePath
kFile = "{data_dir}/K.bin"

vFile :: FilePath
vFile = "{data_dir}/V.bin"

-- Dot product of row i from A (stride sA) with row k from B (stride sB)
dotRow :: Ptr Double -> Ptr Double -> Int -> Int -> Int -> Int -> IO Double
dotRow !aP !bP !sA !sB !i !k = go 0 0.0
  where
    !aBase = i * sA
    !bBase = k * sB
    go !j !acc
      | j >= dModel = return acc
      | otherwise   = do
          !a <- peekElemOff aP (aBase + j)
          !b <- peekElemOff bP (bBase + j)
          go (j + 1) (acc + a * b)

-- Softmax in-place on array of length len
softmaxInPlace :: Ptr Double -> Int -> IO ()
softmaxInPlace !p !len = do
  -- Find max for numerical stability
  !mx <- findMax p len 0 (-1e308)
  -- Compute exp(x - max) and sum
  !s <- expAndSum p len mx 0 0.0
  -- Normalize
  normalize p len s 0
  where
    findMax !p' !n !i !m
      | i >= n    = return m
      | otherwise = do
          !v <- peekElemOff p' i
          findMax p' n (i + 1) (max m v)
    expAndSum !p' !n !mx' !i !acc
      | i >= n    = return acc
      | otherwise = do
          !v <- peekElemOff p' i
          let !ev = exp (v - mx')
          pokeElemOff p' i ev
          expAndSum p' n mx' (i + 1) (acc + ev)
    normalize !p' !n !s' !i
      | i >= n    = return ()
      | otherwise = do
          !v <- peekElemOff p' i
          pokeElemOff p' i (v / s')
          normalize p' n s' (i + 1)

-- Process one block: read files, compute attention, return checksum
processBlock :: Int -> Int -> IO Int64
processBlock !lo !hi = do
  let !rows = hi - lo
  -- Read Q, K, V via BS.readFile (GHC heap allocation)
  qBS <- BS.readFile qFile
  kBS <- BS.readFile kFile
  vBS <- BS.readFile vFile
  let !(BSI.BS qfp _) = qBS
      !(BSI.BS kfp _) = kBS
      !(BSI.BS vfp _) = vBS
  withForeignPtr qfp $ \\qRaw ->
    withForeignPtr kfp $ \\kRaw ->
      withForeignPtr vfp $ \\vRaw -> do
        let !qP = castPtr qRaw :: Ptr Double
            !kP = castPtr kRaw :: Ptr Double
            !vP = castPtr vRaw :: Ptr Double
        -- Allocate scratch: scores row (N) + output row (D)
        sRow <- mallocBytes (seqLen * 8)
        oRow <- mallocBytes (dModel * 8)
        !cs <- rowLoop qP kP vP sRow oRow rows lo 0 0.0
        free sRow
        free oRow
        return $! truncate (cs * 1000000 :: Double)
  where
    rowLoop !qP !kP !vP !sRow !oRow !rows !rowOff !ri !acc
      | ri >= rows = return acc
      | otherwise  = do
          let !qi = rowOff + ri
          -- Compute scores: sRow[k] = Q[qi] . K[k] / sqrt(D)
          computeScores qP kP sRow qi 0
          -- Softmax in-place
          softmaxInPlace sRow seqLen
          -- Compute output: oRow[j] = sum_k sRow[k] * V[k][j]
          computeOutput vP sRow oRow 0
          -- Accumulate checksum from output row
          !rowCS <- sumRow oRow 0 0.0
          rowLoop qP kP vP sRow oRow rows rowOff (ri + 1) (acc + rowCS)

    computeScores !qP !kP !sRow !qi !k
      | k >= seqLen = return ()
      | otherwise   = do
          !d <- dotRow qP kP dModel dModel qi k
          pokeElemOff sRow k (d * invSqrtD)
          computeScores qP kP sRow qi (k + 1)

    computeOutput !vP !sRow !oRow !j
      | j >= dModel = return ()
      | otherwise   = do
          !v <- weightedSum vP sRow j 0 0.0
          pokeElemOff oRow j v
          computeOutput vP sRow oRow (j + 1)

    weightedSum !vP !sRow !j !k !acc
      | k >= seqLen = return acc
      | otherwise   = do
          !s <- peekElemOff sRow k
          !v <- peekElemOff vP (k * dModel + j)
          weightedSum vP sRow j (k + 1) (acc + s * v)

    sumRow !oRow !j !acc
      | j >= dModel = return acc
      | otherwise   = do
          !v <- peekElemOff oRow j
          sumRow oRow (j + 1) (acc + v)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let ranges = {range_list}
  !total <- sumBlocks ranges 0
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "CHECKSUM=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  where
    sumBlocks [] !acc = return acc
    sumBlocks ((lo,hi):rs) !acc = do
      !v <- processBlock lo hi
      sumBlocks rs (acc + v)
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_attn_seq] wrote {path} (N={N}, D={D}, n_funcs={len(ranges)})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--D", type=int, default=512)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit_hs(args.out, args.N, args.D, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
