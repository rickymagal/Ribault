#!/usr/bin/env python3
"""Generate TALM matmul with file IO.

Like LCS wavefront: uses init/block/result supers with superi for block index.
Each block_super independently reads its rows of A and all of BT from binary files.

Generates:
  1. Minimal .hsk (for supersgen / build_supers.sh)
  2. Preprocessor .fl (flowasm macros)
  3. supers_inject.hs with Haskell super implementations
"""

import argparse, os

# Super IDs from codegen on the 3-super .hsk (same as LCS)
SUPER_INIT   = 6
SUPER_BLOCK  = 5
SUPER_RESULT = 4


def emit(path, N, n_funcs, data_dir):
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

    # ---- 1. Minimal .hsk ----
    hsk = f"""-- matmul_io.hsk  (auto-generated, file IO, minimal for supersgen)
-- N={N}  N_FUNCS={nblocks}

init_super seed =
  super single input (seed) output (state)
#BEGINSUPER
    state = unsafePerformIO (mmInit (fromIntegral seed))
#ENDSUPER

block_super dep =
  super single input (dep) output (result)
#BEGINSUPER
    result = unsafePerformIO (mmBlockTid dep)
#ENDSUPER

result_super dep =
  super single input (dep) output (out)
#BEGINSUPER
    out = unsafePerformIO (mmResult dep)
#ENDSUPER

main =
  let s = init_super 0
      b = block_super s
  in result_super b
"""
    with open(path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_matmul_talm_io] wrote {path} (N={N}, n_funcs={nblocks})")

    # ---- 2. Flowasm .fl ----
    fl_path = os.path.join(out_dir, "mm.fl")
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('block',  {SUPER_BLOCK},  1, False, True)",
        f"superinst('output', {SUPER_RESULT}, 1, False, False)",
        f"avgtime('block', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    # All blocks depend on init; no inter-block dependencies (embarrassingly parallel)
    for idx in range(nblocks):
        fl_lines.append(f"block blk{idx}, ini, {idx}")

    # Result depends on all blocks (use last block as trigger, sum in super)
    # Actually, result needs to wait for ALL blocks.  Use a chain:
    # result depends on all blocks via multi-input.
    # But flowasm 'output' only takes one input. Use a reduce chain.
    if nblocks == 1:
        fl_lines.append(f"output out, blk0")
    else:
        # Chain: each block feeds into next, result depends on last
        # No -- blocks are independent.  We need a merge node.
        # Use block with 2 inputs for reduction, or just have result
        # depend on all blocks by daisy-chaining through dummy blocks.
        #
        # Simplest: output depends on last block, but we need ALL blocks
        # to finish. Since blocks are independent and all depend on init,
        # we need a barrier.  Use a reduce tree.
        #
        # Actually, for embarrassingly parallel matmul:
        # - All blocks fire from init
        # - We need a reduction to sum partial checksums
        # - Use block_super with 2 inputs for reduction
        #
        # Simpler approach: chain blocks sequentially in the dataflow
        # but they still execute in parallel (dataflow allows it when
        # there's no TRUE dependency).
        #
        # Wait no -- if block1 depends on block0, it won't fire until
        # block0 is done. That serializes them.
        #
        # The right approach: have a separate "reduce" super, or use
        # the result_super to wait on all blocks.
        #
        # For now: use the same pattern as the original matmul --
        # each block is independent, result_super sums via global state.
        # The result_super just needs to wait for all blocks.
        # We can have result depend on the last block, and arrange
        # blocks in a chain where each passes through a dummy value.
        #
        # Actually the cleanest: use the original matmul pattern.
        # Each block_super returns its partial checksum.
        # The dataflow graph sums them and passes to result.
        #
        # But that requires addition nodes in the dataflow.
        # Let's use a different approach: global IORef accumulator.
        # Each block atomically adds its partial sum.
        # Result reads the final sum.
        # Blocks are all independent (depend only on init).
        # Result depends on all blocks.
        #
        # For result to depend on all blocks, we chain:
        # blk0 -> blk1 -> ... -> blkN -> result
        # But blocks ignore the dependency value (they use treb_get_tid).
        # This serializes execution though!
        #
        # Better: fan-in reduction tree.
        # Or: all blocks output to a single merge, merge outputs to result.
        #
        # Simplest valid approach for TALM:
        # Use the same approach as the ORIGINAL gen_talm_input.py:
        # - N separate block supers (not reused)
        # - Dataflow adds their results
        # - print_checksum at the end
        pass

    # Let me reconsider. For TALM with file IO, the cleanest approach
    # is the ORIGINAL pattern: N independent block supers whose results
    # are summed by the dataflow graph, then printed.
    # But that requires N separate super definitions, which doesn't scale.
    #
    # Alternative: use global IORef for accumulation.
    # - init_super: allocate IORef 0
    # - block_super (x N): read files, compute, atomicModifyIORef
    # - result_super: read IORef, print
    #
    # For result_super to know all blocks are done, we need a barrier.
    # Use a reduction chain: blk0 + blk1 -> add01, add01 + blk2 -> add012, ...
    # But we don't have an "add" node in flowasm with superi...
    #
    # Actually, let's just do what LCS does but without the wavefront
    # dependency. All blocks depend on init. Result depends on all blocks.
    # We can express "result depends on all blocks" by having a chain of
    # dummy pass-through nodes.

    # Reset fl_lines and use the global-accumulator approach
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('block',  {SUPER_BLOCK},  1, False, True)",
        f"superinst('output', {SUPER_RESULT}, 1, False, False)",
        f"avgtime('block', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    # Blocks: all depend on init, each gets its index via superi
    for idx in range(nblocks):
        fl_lines.append(f"block blk{idx}, ini, {idx}")

    # Reduction chain to create barrier: result needs all blocks done
    # Use block_super as pass-through: chain blk0->chain0, chain0+blk1->chain1, ...
    # But block_super only takes 1 data input + immediate.
    # Instead, use a 2-input block_super variant... or just chain blocks.
    #
    # Simplest: linear chain. blk0 fires from ini.
    # blk1 fires from blk0 (but uses treb_get_tid, ignores input value).
    # This serializes blocks though.
    #
    # The REAL solution: use the original separate-supers approach for
    # the dataflow graph (like gen_talm_input.py does for pure compute).
    # Each block is a separate super in the .hsk, returns partial checksum,
    # dataflow sums them.
    #
    # But each super's implementation reads from files -- that's the IO part.
    # The .hsk has N separate block supers, each calling the SAME Haskell
    # function (mmBlock) but with different packed arguments.
    #
    # Wait -- the original gen_talm_input.py hardcodes packed=(lo*SHIFT+rows)
    # into each super. We can do the same but have the super read from files.

    # OK let me switch to the original pattern: separate supers per block.
    # This is simpler and proven to work.
    pass

    with open(fl_path, "w") as f:
        pass  # will be written below

    # ---- RESTART: Use original separate-supers pattern ----
    _emit_separate_supers(path, out_dir, N, blocks, data_dir)


def _emit_separate_supers(hsk_path, out_dir, N, blocks, data_dir):
    """Original pattern: one super per block, dataflow sums results."""
    nblocks = len(blocks)
    SHIFT = N + 1

    super_defs = []
    for idx, (lo, hi) in enumerate(blocks):
        rows = hi - lo
        packed = lo * SHIFT + rows
        super_defs.append(f"""-- SUPER block_{idx}: rows [{lo}..{hi}) of C = A * BT (file IO)
block_{idx} dummy =
  super single input (dummy) output (cs)
#BEGINSUPER
    cs = unsafePerformIO (mmBlock {packed})
#ENDSUPER
""")

    leaf_lets = []
    for i in range(nblocks):
        kw = "let" if i == 0 else "in let"
        leaf_lets.append(f"  {kw} b{i} = block_{i} 0")

    sum_expr = " + ".join(f"b{i}" for i in range(nblocks))

    hsk = f"""-- matmul_io.hsk  (auto-generated, file IO, separate supers)
-- N={N}  N_FUNCS={nblocks}

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
    with open(hsk_path, "w", encoding="utf-8") as f:
        f.write(hsk)
    print(f"[gen_matmul_talm_io] wrote {hsk_path} (N={N}, n_funcs={nblocks})")

    # ---- supers_inject.hs ----
    inject_path = os.path.join(out_dir, "supers_inject.hs")
    inject = f"""import Data.Int (Int64)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekElemOff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

mmN :: Int
mmN = {N}

mmShift :: Int
mmShift = {SHIFT}

mmAFile :: FilePath
mmAFile = "{data_dir}/A.bin"

mmBTFile :: FilePath
mmBTFile = "{data_dir}/BT.bin"

-- Dot product: A row i with BT row k (both from flat Ptr Double arrays)
mmDot :: Ptr Double -> Ptr Double -> Int -> Int -> IO Double
mmDot !aP !btP !i !k = go 0 0.0
  where
    !aBase = i * mmN
    !bBase = k * mmN
    go !j !acc
      | j >= mmN  = return acc
      | otherwise = do
          !a <- peekElemOff aP (aBase + j)
          !b <- peekElemOff btP (bBase + j)
          go (j + 1) (acc + a * b)

-- Block computation: BS.readFile for both matrices (GHC heap allocation)
mmBlock :: Int64 -> IO Int64
mmBlock packed64 = do
  let packed = fromIntegral packed64 :: Int
      lo   = packed `div` mmShift
      rows = packed `mod` mmShift
  aBS  <- BS.readFile mmAFile
  btBS <- BS.readFile mmBTFile
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
      | k >= mmN   = blockSum aP btP rowOff rows (ri + 1) 0 acc
      | otherwise  = do
          !d <- mmDot aP btP (rowOff + ri) k
          blockSum aP btP rowOff rows ri (k + 1) (acc + d)
"""
    with open(inject_path, "w", encoding="utf-8") as f:
        f.write(inject)
    print(f"[gen_matmul_talm_io] wrote {inject_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--n-funcs", type=int, default=14)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit(args.out, args.N, args.n_funcs, args.data_dir)


if __name__ == "__main__":
    main()
