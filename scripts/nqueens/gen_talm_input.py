#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate a parametric N-Queens .hsk for the TALM benchmark.

Uses UNROLLED column iteration (no try_col recursion) to keep tag depth
within TALM's base-9 limit (~19). Each row's column attempts are generated
as a balanced addition tree.

Board state: packed into Int64, 4 bits per queen column (supports N<=15).
is_safe and pack_queen are supers (compiled Haskell) to minimize dataflow overhead.
"""

import argparse, os


def _balanced_sum(names):
    """Build a balanced binary sum expression from a list of names."""
    if len(names) == 0:
        return "0"
    if len(names) == 1:
        return names[0]
    mid = len(names) // 2
    left = _balanced_sum(names[:mid])
    right = _balanced_sum(names[mid:])
    return f"({left} + {right})"


def _gen_try_body(start, end):
    """Generate the body of a try function for columns [start, end)."""
    let_bindings = []
    var_names = []
    for c in range(start, end):
        vname = f"s{c}"
        var_names.append(vname)
        if c == start:
            let_bindings.append(f"  let {vname} = if is_safe row cols {c} == 1 then nqueens (row + 1) (pack_queen cols row {c}) else 0")
        else:
            let_bindings.append(f"  in let {vname} = if is_safe row cols {c} == 1 then nqueens (row + 1) (pack_queen cols row {c}) else 0")
    balanced = _balanced_sum(var_names)
    return "\n".join(let_bindings) + f"\n  in {balanced}"


def emit_hsk(path, N, CUTOFF):
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)

    # Generate try_all: unrolled column attempts with balanced sum.
    # tagRadix=16 supports up to 15 calls to the same callee per function body.
    try_all_body = _gen_try_body(0, N)

    hsk = f"""-- nqueens.hsk  (auto-generated)
-- N={N}  CUTOFF={CUTOFF}

-- SUPER: sequential backtracking from packed state
-- packed = cols * 16 + row  (row in lower 4 bits)
solve_seq packed =
  super single input (packed) output (count)
#BEGINSUPER
    count =
      let
        n = {N}
        row0 = fromIntegral (packed .&. 15)
        cols0 = packed `shiftR` 4
        getQ cs r = fromIntegral ((cs `shiftR` (4 * r)) .&. 15)
        packQ cs r c = cs .|. (fromIntegral c `shiftL` (4 * r))
        safe cs r c = go 0
          where go i | i >= r    = True
                     | otherwise = let ci = getQ cs i
                                   in ci /= c && abs (ci - c) /= (r - i) && go (i + 1)
        solve r cs
          | r >= n    = 1
          | otherwise = sum [ solve (r + 1) (packQ cs r c)
                            | c <- [0..n-1], safe cs r c ]
      in solve row0 cols0
#ENDSUPER

-- SUPER: print result
print_result r =
  super single input (r) output (out)
#BEGINSUPER
    out = unsafePerformIO (do putStrLn ("RESULT=" ++ show r); hFlush stdout; pure 0)
#ENDSUPER

-- SUPER: is_safe check
-- packed3 = cols * 256 + row * 16 + col
-- col in bits 0-3, row in bits 4-7, cols in bits 8+
is_safe_super packed3 =
  super single input (packed3) output (result)
#BEGINSUPER
    result =
      let
        col0 = fromIntegral (packed3 .&. 15)
        row0 = fromIntegral ((packed3 `shiftR` 4) .&. 15)
        cols0 = packed3 `shiftR` 8
        getQ cs r = fromIntegral ((cs `shiftR` (4 * r)) .&. 15)
        go i | i >= row0 = 1
             | otherwise = let ci = getQ cols0 i
                           in if ci /= col0 && abs (ci - col0) /= (row0 - i)
                              then go (i + 1) else 0
      in go 0
#ENDSUPER

-- SUPER: pack queen into board
-- same packing as is_safe_super
pack_queen_super packed3 =
  super single input (packed3) output (result)
#BEGINSUPER
    result =
      let
        col0 = fromIntegral (packed3 .&. 15)
        row0 = fromIntegral ((packed3 `shiftR` 4) .&. 15)
        cols0 = packed3 `shiftR` 8
      in cols0 .|. (fromIntegral col0 `shiftL` (4 * row0))
#ENDSUPER

-- Dataflow wrappers: pack 3 args into single Int64, call super
is_safe row cols col = is_safe_super (cols * 256 + row * 16 + col)

pack_queen cols row col = pack_queen_super (cols * 256 + row * 16 + col)

-- Parallel N-Queens: unrolled columns, balanced addition tree
nqueens row cols =
  if row >= {N}
  then 1
  else if row >= {CUTOFF}
  then solve_seq (cols * 16 + row)
  else try_all row cols

-- Unrolled column attempts
try_all row cols =
{try_all_body}

main = print_result (nqueens 0 0)
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_nq_talm] wrote {path} (N={N}, cutoff={CUTOFF})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=3)
    args = ap.parse_args()
    emit_hsk(args.out, args.N, args.cutoff)


if __name__ == "__main__":
    main()
