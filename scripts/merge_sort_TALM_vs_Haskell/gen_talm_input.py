#!/usr/bin/env python3
"""Generate TALM .hsk + supers_inject.hs for array-based merge sort benchmark.

Same algorithm as all other variants: top-down merge sort on flat C array.
TALM provides a thin DF wrapper; the actual parallel merge sort happens
INSIDE a single par_sort_super via GHC's par/pseq (same code as the
standalone GHC par/pseq variant).

The DF graph is trivial:
  init_super → par_sort_super → verify_super → print_result

This avoids GHC capability contention from concurrent super calls.
TALM runs with P=1 (1 PE thread); GHC handles parallelism internally
via SUPERS_RTS_N=$P capabilities.

Pipeline:
  codegen -> .fl -> assembler -> .flb/.pla
  build_supers.sh (with SUPERS_INJECT_FILE) -> libsupers.so
"""

import argparse, os


# ── Super definitions ────────────────────────────────────────
SUPERS_TMPL = r"""-- Merge Sort: TALM benchmark (array-based, par/pseq inside single super)
-- N=__N__  P=__P__  G=__G__

-- init_super: allocate arrays, fill with [N, N-1, ..., 1], return N
init_super inp =
  super single input (inp) output (result)
#BEGINSUPER
    result = unsafePerformIO (msInit (fromIntegral (head (toList inp))))
#ENDSUPER

-- par_sort_super: parallel merge sort using par/pseq internally
-- Input: N (array size).  Cutoff = N / (P*G).
par_sort_super n =
  super single input (n) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      Just g <- readIORef globalMS
      let !n0 = fromIntegral n :: Int
          !_ = mergeSortPar (msArr g) (msTmp g) n0 0 n0
      return 0
#ENDSUPER

-- verify_super: check arr[0..N-1] is sorted, return 1 or 0
verify_super n =
  super single input (n) output (result)
#BEGINSUPER
    result = unsafePerformIO (msVerify (fromIntegral n))
#ENDSUPER

-- print_result: print RESULT=v to stdout
print_result v =
  super single input (v) output (out)
#BEGINSUPER
    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show v); hFlush stdout; pure 0)
#ENDSUPER

"""


def _gen_body(n):
    """Generate the trivial DF body: init → par_sort → verify → print."""
    return f"""main =
  let n = init_super [{n}]
      done = par_sort_super n
  in print_result (verify_super (n + done))
"""


# ── supers_inject.hs ─────────────────────────────────────────
# Injected into Supers.hs via SUPERS_INJECT_FILE.
# Uses same peekI/pokeI primops and mergeSortPar as GHC par/pseq variant.

INJECT_TMPL = r"""import Control.Parallel.Strategies (runEval, rpar, rseq)
import Foreign.Marshal.Alloc (mallocBytes)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import GHC.Ptr (Ptr(..))
import GHC.IO (IO(..))
import GHC.Exts (readIntOffAddr#, writeIntOffAddr#, Int(..), Int#, Addr#, isTrue#, (+#), (-#), (<=#), quotInt#)

{-# INLINE peekI #-}
peekI :: Ptr Int -> Int -> IO Int
peekI (Ptr addr) (I# i) = IO $ \s ->
  case readIntOffAddr# addr i s of (# s', v #) -> (# s', I# v #)

{-# INLINE pokeI #-}
pokeI :: Ptr Int -> Int -> Int -> IO ()
pokeI (Ptr addr) (I# i) (I# v) = IO $ \s ->
  case writeIntOffAddr# addr i v s of s' -> (# s', () #)

data MSGlobal = MSGlobal
  { msArr :: !(Ptr Int)
  , msTmp :: !(Ptr Int)
  }

{-# NOINLINE globalMS #-}
globalMS :: IORef (Maybe MSGlobal)
globalMS = unsafePerformIO (newIORef Nothing)

{-# NOINLINE globalStartTime #-}
globalStartTime :: IORef UTCTime
globalStartTime = unsafePerformIO (getCurrentTime >>= newIORef)

parCutoff :: Int
parCutoff = __CUTOFF__

msInit :: Int -> IO Int64
msInit n = do
  arr <- mallocBytes (n * 8)
  tmp <- mallocBytes (n * 8)
  let fill !i
        | i >= n    = return ()
        | otherwise = do pokeI arr i (n - i :: Int)
                         pokeI tmp i (0 :: Int)
                         fill (i + 1)
  fill 0
  writeIORef globalMS (Just (MSGlobal arr tmp))
  t0 <- getCurrentTime
  writeIORef globalStartTime t0
  return (fromIntegral n)

{-# INLINE mergeArr #-}
mergeArr :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> IO ()
mergeArr arr tmp lo mid hi = do
  let go !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do
            v <- peekI arr j
            pokeI tmp k v
            go i (j+1) (k+1)
        | j >= hi = do
            v <- peekI arr i
            pokeI tmp k v
            go (i+1) j (k+1)
        | otherwise = do
            vi <- peekI arr i
            vj <- peekI arr j
            if (vi :: Int) <= vj
              then pokeI tmp k vi >> go (i+1) j (k+1)
              else pokeI tmp k vj >> go i (j+1) (k+1)
  go lo mid lo
  let copy !k
        | k >= hi   = return ()
        | otherwise = peekI tmp k >>= pokeI arr k >> copy (k+1)
  copy lo

mergeSortSeq :: Ptr Int -> Ptr Int -> Int -> Int -> IO ()
mergeSortSeq (Ptr arr) (Ptr tmp) (I# lo) (I# hi) = sortSeqW arr tmp lo hi
{-# INLINE mergeSortSeq #-}

sortSeqW :: Addr# -> Addr# -> Int# -> Int# -> IO ()
sortSeqW arr tmp lo hi
  | isTrue# (hi -# lo <=# 1#) = return ()
  | otherwise = do
      let !mid = lo +# quotInt# (hi -# lo) 2#
      sortSeqW arr tmp lo mid
      sortSeqW arr tmp mid hi
      mergeArr (Ptr arr) (Ptr tmp) (I# lo) (I# mid) (I# hi)
{-# NOINLINE sortSeqW #-}

{-# NOINLINE mergeSortPar #-}
mergeSortPar :: Ptr Int -> Ptr Int -> Int -> Int -> Int -> ()
mergeSortPar arr tmp n0 lo hi
  | hi - lo <= 1 = ()
  | hi - lo <= n0 `div` parCutoff = unsafePerformIO (mergeSortSeq arr tmp lo hi)
  | otherwise = unsafePerformIO $ do
      let mid = lo + (hi - lo) `div` 2
          !_ = runEval $ do
                 sl <- rpar (mergeSortPar arr tmp n0 lo mid)
                 sr <- rseq (mergeSortPar arr tmp n0 mid hi)
                 _ <- rseq sl
                 return sr
      mergeArr arr tmp lo mid hi

msVerify :: Int -> IO Int64
msVerify n = do
  Just g <- readIORef globalMS
  let arr = msArr g
      go !i !ok !prev
        | i >= n    = return ok
        | otherwise = do
            v <- peekI arr i :: IO Int
            go (i+1) (ok && v >= prev) v
  ok <- go 0 True minBound
  t1 <- getCurrentTime
  t0 <- readIORef globalStartTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RUNTIME_SEC=" ++ show secs
  hFlush stdout
  return (if ok then 1 else 0)
"""


def emit(path, n, p, g):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    cutoff = p * g

    # Write .hsk with trivial DF body
    header = (SUPERS_TMPL
              .replace("__N__", str(n))
              .replace("__P__", str(p))
              .replace("__G__", str(g)))
    body = _gen_body(n)
    src = header + body

    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_talm] wrote {path}  (N={n}, P={p}, G={g}, cutoff=N/{cutoff})")

    # Write supers_inject.hs
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject_src = INJECT_TMPL.replace("__CUTOFF__", str(cutoff))
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject_src)
    print(f"[gen_ms_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="Output .hsk path")
    ap.add_argument("--N", type=int, required=True, help="Array size")
    ap.add_argument("--P", type=int, required=True, help="Parallelism level")
    ap.add_argument("--G", type=int, default=1, help="Granularity multiplier (cutoff = N/(P*G))")
    args = ap.parse_args()
    emit(args.out, args.N, args.P, args.G)


if __name__ == "__main__":
    main()
