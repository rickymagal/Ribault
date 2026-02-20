#!/usr/bin/env python3
"""Generate a parametric 0/1 knapsack brute-force .hsk for the TALM benchmark.

K independent range-supers, each taking a chunk index (0..K-1).
The super computes lo/hi from the chunk index using compile-time constants
(ksTotalSubsets, ksNFuncs) embedded in supers_inject.hs.

This avoids packing large subset indices into a single 32-bit constant
(the assembler's const instruction is limited to 32-bit signed integers).

Tag safety:
  - tagRadix = 64 in codegen → max 63 unique call sites per function
  - K ≤ 62 from main: K range calls + 1 print_result ≤ 63
"""

import argparse, os


MAX_CALL_SITES = 63  # tagRadix=64 in codegen → up to 63 call sites per function


def _balanced_max(vars):
    """Build a balanced binary tree of max operations using if/then/else."""
    if len(vars) == 0:
        return "0"
    if len(vars) == 1:
        return vars[0]
    if len(vars) == 2:
        a, b = vars
        return f"(if {a} > {b} then {a} else {b})"
    mid = len(vars) // 2
    left = _balanced_max(vars[:mid])
    right = _balanced_max(vars[mid:])
    return f"(if {left} > {right} then {left} else {right})"


def emit_hsk(path, items_dir, n_items, n_funcs):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    n_funcs = min(n_funcs, MAX_CALL_SITES)
    total_subsets = 2 ** n_items

    # Read items and capacity
    items = []
    with open(os.path.join(items_dir, "items.txt")) as f:
        for line in f:
            w, v = line.split()
            items.append((int(w), int(v)))
    with open(os.path.join(items_dir, "capacity.txt")) as f:
        capacity = int(f.read().strip())

    # Compute actual number of chunks (some may be empty for small N)
    actual_funcs = min(n_funcs, total_subsets)
    # Verify each chunk is non-empty
    chunks = []
    for i in range(actual_funcs):
        lo = i * total_subsets // actual_funcs
        hi = (i + 1) * total_subsets // actual_funcs
        if hi > lo:
            chunks.append(i)
    n_funcs = len(chunks)

    lines = []
    lines.append(f"-- knapsack.hsk  (auto-generated)")
    lines.append(f"-- N_ITEMS={n_items}  CAPACITY={capacity}  N_FUNCS={n_funcs}")
    lines.append(f"-- Total subsets: {total_subsets}")
    lines.append("")

    # K range-processing supers — each takes chunk index
    for i in range(n_funcs):
        lines.append(f"pf{i} chunkIdx =")
        lines.append(f"  super single input (chunkIdx) output (best)")
        lines.append("#BEGINSUPER")
        lines.append(f"    best = unsafePerformIO $ ksSearchChunk (fromIntegral chunkIdx)")
        lines.append("#ENDSUPER")
        lines.append("")

    # print_result super
    lines.append("print_result r =")
    lines.append("  super single input (r) output (out)")
    lines.append("#BEGINSUPER")
    lines.append('    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show r); hFlush stdout; pure 0)')
    lines.append("#ENDSUPER")
    lines.append("")

    # Main: nested lets + balanced max tree
    lines.append("main =")
    for i in range(n_funcs):
        lines.append(f"  let r{i} = pf{i} {i}")
        lines.append(f"  in")

    vars = [f"r{i}" for i in range(n_funcs)]
    max_expr = _balanced_max(vars)
    lines.append(f"  let best = {max_expr}")
    lines.append(f"  in print_result best")

    hsk = "\n".join(lines) + "\n"
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_ks_talm] wrote {path}  (n_items={n_items}, n_funcs={n_funcs})")

    # Generate supers_inject.hs
    inject_path = os.path.join(os.path.dirname(path) or ".", "supers_inject.hs")
    items_literal = "[" + ", ".join(f"({w},{v})" for w, v in items) + "]"

    inject = f"""
-- Knapsack helpers (injected by gen_talm_input.py)
-- List-based brute-force: identical algorithm to GHC variants.

ksItems :: [(Int, Int)]
ksItems = {items_literal}

ksCapacity :: Int
ksCapacity = {capacity}

ksTotalSubsets :: Int
ksTotalSubsets = {total_subsets}

ksNFuncs :: Int
ksNFuncs = {n_funcs}

-- Evaluate one subset by walking bits against the item list.
-- Deliberately list-based for allocation pressure.
ksEvalSubset :: Int -> Int
ksEvalSubset s = go ksItems s 0 0
  where
    go [] _ !tw !tv
      | tw <= ksCapacity = tv
      | otherwise        = 0
    go _ 0 !tw !tv
      | tw <= ksCapacity = tv
      | otherwise        = 0
    go ((w,v):rest) !bits !tw !tv
      | bits `mod` 2 == 1 = go rest (bits `div` 2) (tw+w) (tv+v)
      | otherwise          = go rest (bits `div` 2) tw tv

-- Search range [lo, hi), return max value found.
ksSearchRange :: Int -> Int -> Int
ksSearchRange lo hi = go lo 0
  where
    go !i !best
      | i >= hi   = best
      | otherwise = let !v = ksEvalSubset i
                        !b = if v > best then v else best
                    in go (i + 1) b

-- Compute lo/hi from chunk index, then search.
ksSearchChunk :: Int -> IO Int64
ksSearchChunk idx = do
  let lo = idx * ksTotalSubsets `div` ksNFuncs
      hi = (idx + 1) * ksTotalSubsets `div` ksNFuncs
      !result = ksSearchRange lo hi
  return (fromIntegral result)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_ks_talm] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--items-dir", required=True)
    ap.add_argument("--n-items", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14,
                    help="Number of independent range supers (max 14)")
    args = ap.parse_args()
    emit_hsk(args.out, args.items_dir, args.n_items, args.n_funcs)


if __name__ == "__main__":
    main()
