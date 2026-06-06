#!/usr/bin/env python3
"""Generate Ribault CYK files with HASKELL-implemented supers.

Strategy mirrors lcs_wavefront/gen_lcs_hs.py exactly: pre-expand the DAG
at codegen time (the CYK topology is purely a function of N -- which cells
exist, which cells depend on which), emit a flat .fl with one TALM super
per chunk, and write a Haskell `supers_inject.hs` that implements the
init / compute_chunk / output supers using a shared mutable Word64 array
exposed via raw Ptr.

NO Hsub `.hss` recursion involved -- the recursion (wavefront expansion)
is unrolled at Python time and emitted as explicit TALM dataflow nodes.
This sidesteps the codegen recursion-with-state bugs that limited
N-queens to CUTOFF=2.

Outputs in <out-dir>:
  cyk.fl              Flat dataflow graph (one super per chunk).
  cyk.hsk             Minimal .hsk with super name declarations
                      (in reverse-declaration order matching the .fl).
  supers_inject.hs    Haskell super bodies + the kernel that mirrors
                      cyk_seq.hs.

Reads <data-dir>/dag.bin to learn the chunking, and <data-dir>/config.txt
for N / N_NT / N_SIGMA.
"""
import argparse, os, struct, sys


# Super numbers.  Four user supers (consecutive s10..s13).
# .hsk declaration order reverses super-number assignment:
#   init_super (1st declared)  -> s13
#   chunk_super (2nd)          -> s12
#   chunk_or_super (3rd)       -> s11   used as the inter-diagonal barrier
#   output_super (4th)         -> s10
SUPER_INIT     = 13
SUPER_CHUNK    = 12
SUPER_OR       = 11
SUPER_RESULT   = 10


def read_config(data_dir):
    cfg = {}
    with open(os.path.join(data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2:
                try: cfg[ws[0]] = int(ws[1])
                except ValueError: cfg[ws[0]] = ws[1]
    return cfg


def read_dag(data_dir):
    """Parse dag.bin and return [(chunk_id, diag, first_cell_i, n_cells)]."""
    path = os.path.join(data_dir, "dag.bin")
    with open(path, "rb") as f:
        data = f.read()
    chunks = []
    # header: u32 n_diagonals, u64 total_cells
    n_diag, total = struct.unpack_from("<IQ", data, 0)
    off = 12
    for d in range(n_diag):
        n_cells_in_diag, n_chunks = struct.unpack_from("<II", data, off); off += 8
        for _ in range(n_chunks):
            chunk_id, diag, first_i, n_c = struct.unpack_from("<IIII", data, off); off += 16
            chunks.append((chunk_id, diag, first_i, n_c))
    return chunks


def group_chunks_by_diag(chunks):
    """Returns dict: diag -> list of chunk_ids in that diagonal (in chunk-id order)."""
    by_diag = {}
    for (cid, diag, first_i, n_c) in chunks:
        by_diag.setdefault(diag, []).append(cid)
    return by_diag


def make_or_tree(node_name, dep_names, fl_lines, super_or_id, counter):
    """Emit a binary OR-tree of `chunk_or` nodes that reduces dep_names
    to a single token.  Returns the name of that single token.  Modifies
    fl_lines and counter (counter[0] is the next unique id)."""
    if len(dep_names) == 1:
        return dep_names[0]
    cur = list(dep_names)
    while len(cur) > 1:
        nxt = []
        i = 0
        while i + 1 < len(cur):
            tname = f"or_{counter[0]}"
            counter[0] += 1
            fl_lines.append(f"chunk_or {tname}, {cur[i]}, {cur[i+1]}")
            nxt.append(tname)
            i += 2
        if i < len(cur):
            nxt.append(cur[i])
        cur = nxt
    return cur[0]


def emit_fl(out_dir, chunks):
    """Emit cyk.fl using a BARRIER-PER-DIAGONAL model.

    CYK has all-to-all dependencies across diagonals (a cell in diagonal d
    reads from cells in every diagonal d' < d).  Trying to express this as
    explicit per-chunk dependencies blows up the .fl size (1.87M lines for
    N=500 due to OR-trees) and is semantically wasteful.  The natural
    expression is wavefront-synchronous: diagonal d+1 starts only after
    every chunk in diagonal d has finalized its cells in the shared d-table.

    Structure:
      init  -> barrier_0 (= ini)
      For each diagonal d in 1..N-1:
        each chunk in diag d depends on barrier_{d-1}
        barrier_d depends on all chunks in diag d (via OR-tree to keep
        fan-in <= max_or_fanin)
      output depends on barrier_{N-1}.
    """
    fl_lines = [
        f"superinst('init',     {SUPER_INIT},     1, False, False)",
        f"superinst('chunk',    {SUPER_CHUNK},    1, False, False)",
        f"superinst('chunk_or', {SUPER_OR},       1, False, False)",
        f"superinst('output',   {SUPER_RESULT},   1, False, False)",
        "avgtime('chunk', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    by_diag = group_chunks_by_diag(chunks)
    diags = sorted(by_diag.keys())
    counter = [0]
    prev_barrier = "ini"
    last_chunk_token = "ini"
    # Note: chunk node names use prefix "ck_" (not "c") to avoid collision
    # with the `const c0, 0` declared for the init seed.
    for d in diags:
        chunk_ids = by_diag[d]
        chunk_tokens = []
        for cid in chunk_ids:
            fl_lines.append(f"const kk_{cid}, {cid}")
            fl_lines.append(f"chunk ck_{cid}, {prev_barrier}, kk_{cid}")
            chunk_tokens.append(f"ck_{cid}")
        if len(chunk_tokens) == 1:
            barrier_token = chunk_tokens[0]
        else:
            barrier_token = make_or_tree(f"bar_{d}", chunk_tokens, fl_lines, SUPER_OR, counter)
        prev_barrier = barrier_token
        last_chunk_token = chunk_tokens[-1]
    # Last diagonal has exactly one chunk (cell (0, N-1)); feed its token to output.
    fl_lines.append(f"output out, {prev_barrier}")
    path = os.path.join(out_dir, "cyk.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


HSK_TEMPLATE = """-- cyk.hsk (auto-generated).  ASCII-only.

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO cykInit
  )

chunk_super dep idx =
  super chunkImpl dep idx (
    chunkImpl dep idx = unsafePerformIO (cykChunk (fromIntegral idx))
  )

chunk_or_super l r =
  super chunkOrImpl l r (
    chunkOrImpl l r = l + r
  )

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO cykOutput
  )

main =
  let s = init_super 0
      a = chunk_super s 0
      c = output_super a
  in c
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, castPtr, nullPtr)
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.), (.|.), countTrailingZeros)
import Control.Monad (forM_)
import Data.Int (Int64)


cykN, cykNNT, cykNSigma :: Int
cykN      = __N__
cykNNT    = __N_NT__
cykNSigma = __N_SIGMA__

cykDataDir :: FilePath
cykDataDir = "__DATA_DIR__"


{-# NOINLINE g_d_cyk #-}
g_d_cyk :: IORef (Ptr Word64)
g_d_cyk = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_s_cyk #-}
g_s_cyk :: IORef (Ptr Word8)
g_s_cyk = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_pb_cyk #-}
g_pb_cyk :: IORef (Ptr Word64)
g_pb_cyk = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_pt_cyk #-}
g_pt_cyk :: IORef (Ptr Word64)
g_pt_cyk = unsafePerformIO (newIORef nullPtr)

-- DAG chunk lookup arrays, indexed by chunk_id.
{-# NOINLINE g_chunk_diag #-}
g_chunk_diag :: IORef (Ptr Int64)
g_chunk_diag = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_chunk_first #-}
g_chunk_first :: IORef (Ptr Int64)
g_chunk_first = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_chunk_ncells #-}
g_chunk_ncells :: IORef (Ptr Int64)
g_chunk_ncells = unsafePerformIO (newIORef nullPtr)


readBytes :: FilePath -> Int -> IO (Ptr Word8)
readBytes path n = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes n :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src n
  return p

allocI64 :: Int -> IO (Ptr Int64)
allocI64 count = do
  p <- mallocBytes (count * 8) :: IO (Ptr Int64)
  forM_ [0 .. count - 1] $ \i -> pokeElemOff p i (0 :: Int64)
  return p


cykInit :: IO Int64
cykInit = do
  sP <- readBytes (cykDataDir ++ "/input.bin") cykN
  gP <- readBytes (cykDataDir ++ "/grammar.bin") (16 + cykNNT*cykNNT*8 + cykNSigma*8)
  let pbPtr = castPtr (gP `plusPtr` 16) :: Ptr Word64
      ptPtr = castPtr (gP `plusPtr` (16 + cykNNT*cykNNT*8)) :: Ptr Word64

  -- Allocate d[N*N] zeroed.
  dPtr <- mallocBytes (cykN * cykN * 8) :: IO (Ptr Word64)
  forM_ [0 .. cykN * cykN - 1] $ \i -> pokeElemOff dPtr i (0 :: Word64)
  forM_ [0 .. cykN - 1] $ \i -> do
    !sigma <- peekElemOff sP i
    !mask <- peekElemOff ptPtr (fromIntegral sigma)
    pokeElemOff dPtr (i * cykN + i) mask

  -- Parse dag.bin to populate chunk lookup arrays.
  dagBS <- BS.readFile (cykDataDir ++ "/dag.bin")
  let BSI.BS fpD _ = dagBS
  diags <- allocI64 __N_CHUNKS__
  firsts <- allocI64 __N_CHUNKS__
  ncs <- allocI64 __N_CHUNKS__
  withForeignPtr fpD $ \pD -> do
    let p32 = castPtr pD :: Ptr Word32
        -- header: u32 n_diags + u64 total_cells = 12 bytes, then per-diag
        -- u32 n_cells_in_diag + u32 n_chunks + n_chunks * (4 u32s).
        readDiags !off !cnt
          | cnt >= __N_DIAGS__ = return ()
          | otherwise = do
              !n_cells <- peekElemOff p32 (off + 0)
              !n_chunks <- peekElemOff p32 (off + 1)
              let !nC = fromIntegral n_chunks :: Int
              forM_ [0 .. nC - 1] $ \k -> do
                let base = off + 2 + k * 4
                !cid    <- peekElemOff p32 (base + 0)
                !diag   <- peekElemOff p32 (base + 1)
                !firstI <- peekElemOff p32 (base + 2)
                !nc     <- peekElemOff p32 (base + 3)
                pokeElemOff diags  (fromIntegral cid) (fromIntegral diag :: Int64)
                pokeElemOff firsts (fromIntegral cid) (fromIntegral firstI :: Int64)
                pokeElemOff ncs    (fromIntegral cid) (fromIntegral nc :: Int64)
              readDiags (off + 2 + nC * 4) (cnt + 1)
    -- p32 is in u32 units; header is 3 u32s (n_diags as u32, total_cells as u64 = 2 u32s).
    readDiags 3 0

  writeIORef g_d_cyk dPtr
  writeIORef g_s_cyk sP
  writeIORef g_pb_cyk pbPtr
  writeIORef g_pt_cyk ptPtr
  writeIORef g_chunk_diag diags
  writeIORef g_chunk_first firsts
  writeIORef g_chunk_ncells ncs
  return 0


-- Process one chunk: compute every cell (i, i + diag) for i in [first, first + ncells).
-- All reads from d are from cells in smaller diagonals which are already finalized
-- by the DAG dependencies (the super wouldn't have been fired otherwise).
cykChunk :: Int -> IO Int64
cykChunk !chunkId = do
  dPtr   <- readIORef g_d_cyk
  pbPtr  <- readIORef g_pb_cyk
  diagsP <- readIORef g_chunk_diag
  firstsP<- readIORef g_chunk_first
  ncsP   <- readIORef g_chunk_ncells
  !diag64 <- peekElemOff diagsP chunkId
  !first64 <- peekElemOff firstsP chunkId
  !ncs64 <- peekElemOff ncsP chunkId
  let !diag = fromIntegral diag64 :: Int
      !first = fromIntegral first64 :: Int
      !ncells = fromIntegral ncs64 :: Int
      !n = cykN
      !nnt = cykNNT
  forM_ [0 .. ncells - 1] $ \off -> do
    let !i = first + off
        !j = i + diag
    !acc <- compute dPtr pbPtr nnt n i j
    pokeElemOff dPtr (i * n + j) acc
  return 0


{-# INLINE compute #-}
compute :: Ptr Word64 -> Ptr Word64 -> Int -> Int -> Int -> Int -> IO Word64
compute !dPtr !pbPtr !nnt !n !i !j = goK i 0
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !left <- peekElemOff dPtr (i * n + k)
          !right <- peekElemOff dPtr ((k + 1) * n + j)
          if left == 0 || right == 0
            then goK (k + 1) acc
            else do
              !acc' <- accBits left right acc
              goK (k + 1) acc'
    accBits !lb !right !acc
      | lb == 0 = return acc
      | otherwise = do
          let !b = countTrailingZeros lb
              !lb' = lb .&. (lb - 1)
          !acc'' <- accBits2 right b acc
          accBits lb' right acc''
    accBits2 !rb !b !acc
      | rb == 0 = return acc
      | otherwise = do
          let !c = countTrailingZeros rb
              !rb' = rb .&. (rb - 1)
          !m <- peekElemOff pbPtr (b * nnt + c)
          accBits2 rb' b (acc .|. m)


cykBarrier :: IO Int64
cykBarrier = return 0


cykOutput :: IO Int64
cykOutput = do
  dPtr <- readIORef g_d_cyk
  let !idx = 0 * cykN + (cykN - 1)
  !top <- peekElemOff dPtr idx
  putStrLn ("CHECKSUM=" ++ wordToHex top)
  return (fromIntegral top :: Int64)
  where
    wordToHex w = let s = showHexUpper w "" in replicate (16 - length s) '0' ++ s
    showHexUpper 0 acc = if null acc then "0" else acc
    showHexUpper x acc =
      let (q, r) = x `divMod` 16
          d = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                        else toEnum (fromEnum 'A' + fromIntegral r - 10)
      in showHexUpper q (d : acc)
"""


def emit(out_dir, data_dir, max_fanin):
    os.makedirs(out_dir, exist_ok=True)
    cfg = read_config(data_dir)
    n = cfg["N"]
    nnt = cfg["N_NT"]
    nsigma = cfg["N_SIGMA"]
    chunks = read_dag(data_dir)

    emit_fl(out_dir, chunks)

    with open(os.path.join(out_dir, "cyk.hsk"), "w") as f:
        f.write(HSK_TEMPLATE)

    inj = (INJECT_TEMPLATE
           .replace("__N__", str(n))
           .replace("__N_NT__", str(nnt))
           .replace("__N_SIGMA__", str(nsigma))
           .replace("__DATA_DIR__", os.path.abspath(data_dir))
           .replace("__N_CHUNKS__", str(len(chunks)))
           .replace("__N_DIAGS__", str(n - 1)))
    with open(os.path.join(out_dir, "supers_inject.hs"), "w") as f:
        f.write(inj)

    by_diag = group_chunks_by_diag(chunks)
    max_chunks_per_diag = max((len(v) for v in by_diag.values()), default=0)
    print(f"[gen_cyk_hs] {out_dir}: {len(chunks)} chunks, {len(by_diag)} diagonals, "
          f"max chunks/diag = {max_chunks_per_diag}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--max-fanin", type=int, default=28)
    args = ap.parse_args()
    emit(args.out_dir, args.data_dir, args.max_fanin)


if __name__ == "__main__":
    main()
