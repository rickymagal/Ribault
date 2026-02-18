#!/usr/bin/env python3
"""Generate a parametric text-search .hsk for the TALM benchmark.

Equivalent structure to GHC benchmarks: K independent range-supers,
each processing a contiguous range of files (readFile + countOcc for
each file in the range, summing counts internally).  This minimizes
dataflow tokens while maximizing super-level parallelism.

The range is packed into a single Int64: packed = lo * 65536 + hi.
The super unpacks it in Haskell.

Tag safety:
  - tagRadix = 16 → max 15 unique call sites per function
  - K ≤ 14 from main: K range calls + 1 print_result ≤ 15
"""

import argparse, os


MAX_CALL_SITES = 14  # K range calls + 1 print_result = K+1 ≤ 15
PACK_SHIFT = 65536   # lo * 65536 + hi


def emit_hsk(path, n_files, keyword, corpus_dir, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    n_funcs = min(n_funcs, n_files, MAX_CALL_SITES)

    # Compute file ranges for each search function
    ranges = []
    for i in range(n_funcs):
        lo = i * n_files // n_funcs
        hi = (i + 1) * n_files // n_funcs
        if hi > lo:
            ranges.append((lo, hi))
    n_funcs = len(ranges)

    lines = []
    lines.append(f"-- textsearch.hsk  (auto-generated)")
    lines.append(f"-- N_FILES={n_files}  KEYWORD=\"{keyword}\"  N_FUNCS={n_funcs}")
    lines.append(f"-- Range packing: packed = lo * {PACK_SHIFT} + hi")
    lines.append("")

    # K range-processing supers — each handles a contiguous range of files
    for i in range(n_funcs):
        lines.append(f"pf{i} packed =")
        lines.append(f"  super single input (packed) output (count)")
        lines.append("#BEGINSUPER")
        lines.append(f"    count = unsafePerformIO $ do")
        lines.append(f"      let loI = fromIntegral (packed `div` {PACK_SHIFT}) :: Int")
        lines.append(f"          hiI = fromIntegral (packed `mod` {PACK_SHIFT}) :: Int")
        lines.append(f"      tsProcessRange loI hiI")
        lines.append("#ENDSUPER")
        lines.append("")

    # Super: print result
    lines.append("print_result r =")
    lines.append("  super single input (r) output (out)")
    lines.append("#BEGINSUPER")
    lines.append('    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show r); hFlush stdout; pure 0)')
    lines.append("#ENDSUPER")
    lines.append("")

    # Main: nested lets calling each range super with packed args
    lines.append("main =")
    for i, (lo, hi) in enumerate(ranges):
        packed = lo * PACK_SHIFT + hi
        lines.append(f"  let r{i} = pf{i} {packed}")
        lines.append(f"  in")

    vars = [f"r{i}" for i in range(n_funcs)]
    sum_expr = _balanced_sum(vars)
    lines.append(f"  let total = {sum_expr}")
    lines.append(f"  in print_result total")

    hsk = "\n".join(lines) + "\n"
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_ts_talm] wrote {path}  (n_files={n_files}, n_funcs={n_funcs})")

    # Generate the inject file for Supers.hs
    inject_path = os.path.join(os.path.dirname(path) or ".", "supers_inject.hs")
    abs_corpus = os.path.abspath(corpus_dir)
    inject = f"""
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Text-search helpers (injected by gen_talm_input.py)
-- Sequential readFile + countOcc per file (same as GHC variants).
-- IO/compute overlap comes from TALM's OS-thread dispatch of supers.
tsCorpusDir :: String
tsCorpusDir = "{abs_corpus}"

tsKwBytes :: BS.ByteString
tsKwBytes = BS.pack {list(keyword.encode('ascii'))}

tsPadInt :: Int -> Int -> String
tsPadInt w n = let s = show n in replicate (w - length s) '0' ++ s

-- Zero-allocation keyword counting (same algorithm as GHC benchmarks)
tsCountOcc :: BS.ByteString -> BS.ByteString -> Int
tsCountOcc buf kw = go 0 0
  where
    kwLen  = BS.length kw
    bufLen = BS.length buf
    end    = bufLen - kwLen + 1
    go !i !acc
      | i >= end    = acc
      | matchAt i 0 = go (i + kwLen) (acc + 1)
      | otherwise   = go (i + 1) acc
    matchAt !off !k
      | k >= kwLen = True
      | BSU.unsafeIndex buf (off + k) /= BSU.unsafeIndex kw k = False
      | otherwise  = matchAt off (k + 1)

-- Process a range of files [lo, hi), returning sum of counts.
tsProcessRange :: Int -> Int -> IO Int64
tsProcessRange lo hi = go lo 0
  where
    go !i !acc
      | i >= hi   = return acc
      | otherwise = do
          let path = tsCorpusDir ++ "/file_" ++ tsPadInt 4 i ++ ".txt"
          contents <- BS.readFile path
          let !c = tsCountOcc contents tsKwBytes
          go (i + 1) (acc + fromIntegral c)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_ts_talm] wrote {inject_path}")


def _balanced_sum(vars):
    """Build a balanced binary tree of additions."""
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--n-files", type=int, required=True)
    ap.add_argument("--keyword", default="FINDME")
    ap.add_argument("--corpus-dir", required=True)
    ap.add_argument("--n-funcs", type=int, default=12,
                    help="Number of independent range supers (default: 12, max: 14)")
    args = ap.parse_args()
    emit_hsk(args.out, args.n_files, args.keyword, args.corpus_dir, args.n_funcs)


if __name__ == "__main__":
    main()
