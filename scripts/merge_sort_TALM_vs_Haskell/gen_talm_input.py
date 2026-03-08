#!/usr/bin/env python3
"""Generate TALM .hsk + supers_inject.hs for array-based merge sort benchmark.

Same algorithm as all other variants: top-down merge sort on flat C array.
TALM provides parallelism via the DF graph (tree of sort/merge nodes).
All O(N) work is inside supers; the DF graph only carries scalar indices.

Global shared state: two C arrays (data + scratch) in an IORef,
accessed by all supers.  No DF-pair encoding for bulk data.

Supers are N/P-independent -> compile once, reuse across all (N,P).
The .fl graph structure is also (N,P)-independent -> codegen once,
then sed-replace the two constants before assembly.

Pipeline:
  codegen -> .fl -> assembler -> .flb/.pla
  build_supers.sh (with SUPERS_INJECT_FILE) -> libsupers.so
"""

import argparse, os


# ── .hsk template ─────────────────────────────────────────────
# Super bodies are thin wrappers; actual logic is in supers_inject.hs.

HSK_TMPL = r"""-- Merge Sort: TALM benchmark (array-based, global shared state)
-- N=__N__  P=__P__

-- init_super: allocate arrays, fill with [N, N-1, ..., 1], return N
init_super inp =
  super single input (inp) output (result)
#BEGINSUPER
    result = unsafePerformIO (msInit (fromIntegral (head (toList inp))))
#ENDSUPER

-- sort_super: sort arr[lo..hi-1] sequentially, return 0
-- Input: encoded = lo * BASE + hi  (BASE = 1000000000)
sort_super encoded =
  super single input (encoded) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      let (lo, hi) = msDecodePair encoded
      msSortSeq lo hi
      return 0
#ENDSUPER

-- merge_super: merge two sorted halves, return 0
-- Input: encoded = lo * BASE + hi  (mid = lo + (hi-lo)/2)
merge_super encoded =
  super single input (encoded) output (result)
#BEGINSUPER
    result = unsafePerformIO $ do
      let (lo, hi) = msDecodePair encoded
          mid = lo + (hi - lo) `div` 2
      msMerge lo mid hi
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

p = __P__

-- Encoding base: indices packed as lo * BASE + hi (single scalar)
b = 1000000000

-- DF recursion: tree depth = log2(P), only scalar indices flow.
-- sort_super/merge_super return 0 (done signal).
-- Data dependencies via lo+doneL, hi+doneR ensure correct ordering.
mergeSortT n lo hi =
  if (hi - lo) <= (n / p)
  then sort_super (lo * b + hi)
  else
    let mid = lo + (hi - lo) / 2
        doneL = mergeSortT n lo mid
        doneR = mergeSortT n mid hi
    in merge_super ((lo + doneL) * b + (hi + doneR))

main =
  let n = init_super [__N__]
      done = mergeSortT n 0 n
  in print_result (verify_super (n + done))
"""


# ── supers_inject.hs ─────────────────────────────────────────
# Injected into Supers.hs via SUPERS_INJECT_FILE.
# Import lines go after existing imports; declarations go before
# the first 'foreign export'.

INJECT_TMPL = r"""import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)

data MSGlobal = MSGlobal
  { msArr :: !(Ptr Int)
  , msTmp :: !(Ptr Int)
  }

{-# NOINLINE globalMS #-}
globalMS :: IORef (Maybe MSGlobal)
globalMS = unsafePerformIO (newIORef Nothing)

msBase :: Int
msBase = 1000000000

msDecodePair :: Int64 -> (Int, Int)
msDecodePair enc = let e = fromIntegral enc :: Int
                   in (e `div` msBase, e `mod` msBase)

msInit :: Int -> IO Int64
msInit n = do
  arr <- mallocBytes (n * 8)
  tmp <- mallocBytes (n * 8)
  let fill !i
        | i >= n    = return ()
        | otherwise = pokeElemOff arr i (n - i :: Int) >> fill (i + 1)
  fill 0
  writeIORef globalMS (Just (MSGlobal arr tmp))
  return (fromIntegral n)

msMerge :: Int -> Int -> Int -> IO ()
msMerge lo mid hi = do
  Just g <- readIORef globalMS
  let arr = msArr g
      tmp = msTmp g
      go !i !j !k
        | i >= mid && j >= hi = return ()
        | i >= mid = do
            v <- peekElemOff arr j
            pokeElemOff tmp k v
            go i (j+1) (k+1)
        | j >= hi = do
            v <- peekElemOff arr i
            pokeElemOff tmp k v
            go (i+1) j (k+1)
        | otherwise = do
            vi <- peekElemOff arr i
            vj <- peekElemOff arr j
            if (vi :: Int) <= vj
              then pokeElemOff tmp k vi >> go (i+1) j (k+1)
              else pokeElemOff tmp k vj >> go i (j+1) (k+1)
  go lo mid lo
  let copy !k
        | k >= hi   = return ()
        | otherwise = peekElemOff tmp k >>= pokeElemOff arr k >> copy (k+1)
  copy lo

msSortSeq :: Int -> Int -> IO ()
msSortSeq lo hi
  | hi - lo <= 1 = return ()
  | otherwise = do
      let mid = lo + (hi - lo) `div` 2
      msSortSeq lo mid
      msSortSeq mid hi
      msMerge lo mid hi

msVerify :: Int -> IO Int64
msVerify n = do
  Just g <- readIORef globalMS
  let arr = msArr g
      go !i !ok !prev
        | i >= n    = return ok
        | otherwise = do
            v <- peekElemOff arr i :: IO Int
            go (i+1) (ok && v >= prev) v
  ok <- go 0 True minBound
  return (if ok then 1 else 0)
"""


def emit(path, input_dir, p):
    out_dir = os.path.dirname(path) or "."
    os.makedirs(out_dir, exist_ok=True)

    with open(os.path.join(input_dir, "params.txt")) as f:
        n = int(f.read().strip())

    # Write .hsk
    src = HSK_TMPL.replace("__N__", str(n)).replace("__P__", str(p))
    with open(path, "w", encoding="utf-8") as f:
        f.write(src)
    print(f"[gen_ms_talm] wrote {path}  (N={n}, P={p})")

    # Write supers_inject.hs (N/P-independent)
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(INJECT_TMPL)
    print(f"[gen_ms_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True, help="Output .hsk path")
    ap.add_argument("--input-dir", required=True)
    ap.add_argument("--P", type=int, required=True, help="Parallelism level (cutoff = N/P)")
    args = ap.parse_args()
    emit(args.out, args.input_dir, args.P)


if __name__ == "__main__":
    main()
