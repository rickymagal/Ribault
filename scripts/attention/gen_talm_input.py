#!/usr/bin/env python3
"""Generate TALM attention with file IO.

Same pattern as matmul: separate supers per block, each block reads Q/K/V
via BS.readFile (GHC heap allocation → GC pressure).

Single-head self-attention: O = softmax(Q * K^T / sqrt(D)) * V
Each block handles rows [lo, hi) of Q.

Generates:
  1. Minimal .hsk with separate block supers
  2. supers_inject.hs with Haskell implementation
"""

import argparse, os


def emit(path, N, D, n_funcs, data_dir):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    n_funcs = min(n_funcs, N)
    blocks = []
    for i in range(n_funcs):
        lo = i * N // n_funcs
        hi = (i + 1) * N // n_funcs
        if hi > lo:
            blocks.append((lo, hi))
    nblocks = len(blocks)
    SHIFT = N + 1

    # ---- 1. .hsk with separate supers per block ----
    super_defs = []
    for idx, (lo, hi) in enumerate(blocks):
        rows = hi - lo
        packed = lo * SHIFT + rows
        super_defs.append(f"""-- SUPER block_{idx}: attention rows [{lo}..{hi})
block_{idx} dummy =
  super single input (dummy) output (cs)
#BEGINSUPER
    cs = unsafePerformIO (attnBlock {packed})
#ENDSUPER
""")

    leaf_lets = []
    for i in range(nblocks):
        kw = "let" if i == 0 else "in let"
        leaf_lets.append(f"  {kw} b{i} = block_{i} 0")

    sum_expr = " + ".join(f"b{i}" for i in range(nblocks))

    hsk = f"""-- attention.hsk  (auto-generated, file IO, separate supers)
-- N={N}  D={D}  N_FUNCS={nblocks}

{"".join(super_defs)}
-- SUPER: print final checksum
print_checksum cs =
  super single input (cs) output (out)
#BEGINSUPER
    out = unsafePerformIO
      (do
        putStrLn ("CHECKSUM=" ++ show cs)
        pure 0)
#ENDSUPER

main =
{chr(10).join(leaf_lets)}
  in let total = {sum_expr}
  in print_checksum total
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_attn_talm] wrote {path} (N={N}, D={D}, n_funcs={nblocks})")

    # ---- 2. supers_inject.hs ----
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject = f"""import Data.Int (Int64)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

attnN :: Int
attnN = {N}

attnD :: Int
attnD = {D}

attnShift :: Int
attnShift = {SHIFT}

attnInvSqrtD :: Double
attnInvSqrtD = 1.0 / sqrt (fromIntegral attnD)

attnQFile :: FilePath
attnQFile = "{data_dir}/Q.bin"

attnKFile :: FilePath
attnKFile = "{data_dir}/K.bin"

attnVFile :: FilePath
attnVFile = "{data_dir}/V.bin"

attnDotRow :: Ptr Double -> Ptr Double -> Int -> Int -> Int -> Int -> IO Double
attnDotRow !aP !bP !sA !sB !i !k = go 0 0.0
  where
    !aBase = i * sA
    !bBase = k * sB
    go !j !acc
      | j >= attnD = return acc
      | otherwise  = do
          !a <- peekElemOff aP (aBase + j)
          !b <- peekElemOff bP (bBase + j)
          go (j + 1) (acc + a * b)

attnSoftmaxInPlace :: Ptr Double -> Int -> IO ()
attnSoftmaxInPlace !p !len = do
  !mx <- findMax p len 0 (-1e308)
  !s  <- expAndSum p len mx 0 0.0
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

-- Block computation: BS.readFile for Q, K, V (GHC heap allocation)
attnBlock :: Int64 -> IO Int64
attnBlock packed64 = do
  let packed = fromIntegral packed64 :: Int
      lo   = packed `div` attnShift
      rows = packed `mod` attnShift
  qBS <- BS.readFile attnQFile
  kBS <- BS.readFile attnKFile
  vBS <- BS.readFile attnVFile
  let !(BSI.BS qfp _) = qBS
      !(BSI.BS kfp _) = kBS
      !(BSI.BS vfp _) = vBS
  withForeignPtr qfp $ \\qRaw ->
    withForeignPtr kfp $ \\kRaw ->
      withForeignPtr vfp $ \\vRaw -> do
        let !qP = castPtr qRaw :: Ptr Double
            !kP = castPtr kRaw :: Ptr Double
            !vP = castPtr vRaw :: Ptr Double
        sRow <- mallocBytes (attnN * 8)
        oRow <- mallocBytes (attnD * 8)
        !cs <- rowLoop qP kP vP sRow oRow rows lo 0 0.0
        free sRow
        free oRow
        return $! truncate (cs * 1000000 :: Double)
  where
    rowLoop !qP !kP !vP !sRow !oRow !rows !rowOff !ri !acc
      | ri >= rows = return acc
      | otherwise  = do
          let !qi = rowOff + ri
          computeScores qP kP sRow qi 0
          attnSoftmaxInPlace sRow attnN
          computeOutput vP sRow oRow 0
          !rowCS <- sumRow oRow 0 0.0
          rowLoop qP kP vP sRow oRow rows rowOff (ri + 1) (acc + rowCS)

    computeScores !qP !kP !sRow !qi !k
      | k >= attnN = return ()
      | otherwise  = do
          !d <- attnDotRow qP kP attnD attnD qi k
          pokeElemOff sRow k (d * attnInvSqrtD)
          computeScores qP kP sRow qi (k + 1)

    computeOutput !vP !sRow !oRow !j
      | j >= attnD = return ()
      | otherwise  = do
          !v <- weightedSum vP sRow j 0 0.0
          pokeElemOff oRow j v
          computeOutput vP sRow oRow (j + 1)

    weightedSum !vP !sRow !j !k !acc
      | k >= attnN = return acc
      | otherwise  = do
          !s <- peekElemOff sRow k
          !v <- peekElemOff vP (k * attnD + j)
          weightedSum vP sRow j (k + 1) (acc + s * v)

    sumRow !oRow !j !acc
      | j >= attnD = return acc
      | otherwise  = do
          !v <- peekElemOff oRow j
          sumRow oRow (j + 1) (acc + v)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_attn_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--D", type=int, default=512)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit(args.out, args.N, args.D, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
