#!/usr/bin/env python3
"""Ribault driver for CYK k-best parsing.  Emits .fl + .hsk + supers_inject.hs.

Three inegociable design rules (per user spec, post spec_decoding postmortem):

  1. NO MVar / IORef shared mutation in the super body.  The cell storage
     is a single raw IOArray (Ptr) initialized once in the init super and
     read/written via offset.  Each cell is written by exactly ONE super
     (its computing super) and read by N consumers; the TALM token system
     guarantees ordering.

  2. NO artificial barrier between antidiagonals.  Each chunk depends on
     the SPECIFIC chunks containing the cells it reads (computed from the
     real cell-level dependency graph).  Trebuchet discovers the
     parallelism from the firing rule.

  3. Fan-in is capped at MAX_FANIN via binary OR-tree decomposition.  The
     OR-tree nodes carry tokens (readiness signals); the actual data is
     read by each chunk super from the shared IOArray.

Output:
  cyk_kbest.fl              Dataflow graph (per-chunk supers + OR-trees)
  cyk_kbest.hsk             Super declarations
  supers_inject.hs          Haskell kernel implementing init/chunk/output
"""
import argparse, os, struct, sys


SUPER_INIT     = 13
SUPER_CHUNK    = 12
SUPER_OR       = 11
SUPER_RESULT   = 10
MAX_FANIN_DEFAULT = 28


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
    """Returns list of (chunk_id, diag, first_cell_i, n_cells) sorted by chunk_id."""
    path = os.path.join(data_dir, "dag.bin")
    with open(path, "rb") as f:
        data = f.read()
    chunks = []
    n_diag, total = struct.unpack_from("<IQ", data, 0)
    off = 12
    for d in range(n_diag):
        n_cells_in_diag, n_chunks = struct.unpack_from("<II", data, off); off += 8
        for _ in range(n_chunks):
            cid, diag, first_i, n_c = struct.unpack_from("<IIII", data, off); off += 16
            chunks.append((cid, diag, first_i, n_c))
    return chunks


def compute_chunk_deps(chunks):
    """For each chunk in diagonal d, find the chunks (in diag < d) that
    contain cells the chunk's cells read.

    Cell (i, j) reads d[i][k] and d[k+1][j] for k in [i, j-1].  We map
    each read to a chunk via the (i, j) -> chunk_id map and dedupe."""
    cell_chunk = {}
    for (cid, diag, first_i, n_c) in chunks:
        for off in range(n_c):
            i = first_i + off
            j = i + diag
            cell_chunk[(i, j)] = cid
    deps_by_chunk = {}
    for (cid, diag, first_i, n_c) in chunks:
        deps = set()
        for off in range(n_c):
            i = first_i + off
            j = i + diag
            for k in range(i, j):
                if (i, k) in cell_chunk:
                    deps.add(cell_chunk[(i, k)])
                if (k + 1, j) in cell_chunk:
                    deps.add(cell_chunk[(k + 1, j)])
        deps.discard(cid)
        deps_by_chunk[cid] = sorted(deps)
    return deps_by_chunk


def make_or_tree(dep_names, fl_lines, counter, max_fanin):
    """Binary OR-tree of `chunk_or` supers collapsing N dependency tokens
    to 1.  Each chunk_or has 2 inputs; the tree is balanced left-to-right."""
    if len(dep_names) == 1:
        return dep_names[0]
    cur = list(dep_names)
    while len(cur) > 1:
        nxt = []
        i = 0
        while i + 1 < len(cur):
            t = f"or_{counter[0]}"
            counter[0] += 1
            fl_lines.append(f"chunk_or {t}, {cur[i]}, {cur[i+1]}")
            nxt.append(t)
            i += 2
        if i < len(cur):
            nxt.append(cur[i])
        cur = nxt
    return cur[0]


def emit_fl(out_dir, chunks, deps_by_chunk, max_fanin):
    fl_lines = [
        f"superinst('init',     {SUPER_INIT},     1, False, False)",
        f"superinst('chunk',    {SUPER_CHUNK},    1, False, False)",
        f"superinst('chunk_or', {SUPER_OR},       1, False, False)",
        f"superinst('output',   {SUPER_RESULT},   1, False, False)",
        "avgtime('chunk', 50000)",
        "",
        "const seed_const, 0",
        "init ini, seed_const",
    ]
    counter = [0]
    final_token = "ini"
    for (cid, diag, first_i, n_c) in chunks:
        deps = deps_by_chunk.get(cid, [])
        if not deps:
            dep_token = "ini"
        else:
            dep_names = [f"ck_{d}" for d in deps]
            dep_token = make_or_tree(dep_names, fl_lines, counter, max_fanin)
        fl_lines.append(f"const kk_{cid}, {cid}")
        fl_lines.append(f"chunk ck_{cid}, {dep_token}, kk_{cid}")
        final_token = f"ck_{cid}"
    fl_lines.append(f"output out, {final_token}")
    path = os.path.join(out_dir, "cyk_kbest.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return len(fl_lines), counter[0]


HSK_TEMPLATE = """-- cyk_kbest.hsk (auto-generated).  ASCII-only.

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


INJECT_TEMPLATE = r"""import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, castPtr, nullPtr)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Array.IO as IOA
import Data.Array.IO (IOArray)
import Data.List (foldl')


cykN, cykNNT, cykNSigma :: Int
cykN      = __N__
cykNNT    = __N_NT__
cykNSigma = __N_SIGMA__

cykDataDir :: FilePath
cykDataDir = "__DATA_DIR__"


topK :: Int
topK = 8


data Derivation = Derivation
  { drvA     :: {-# UNPACK #-} !Word8
  , drvB     :: {-# UNPACK #-} !Word8
  , drvC     :: {-# UNPACK #-} !Word8
  , drvProd  :: {-# UNPACK #-} !Word16
  , drvSplit :: {-# UNPACK #-} !Word16
  , drvScore :: {-# UNPACK #-} !Double
  }


{-# INLINE insertTopK #-}
insertTopK :: Int -> Derivation -> [Derivation] -> [Derivation]
insertTopK k d ds = take k (insert d ds)
  where
    insert x [] = [x]
    insert x (y:ys)
      | drvScore x >= drvScore y = x : y : ys
      | otherwise = y : insert x ys


-- Global storage.  ONE IOArray (Int, Int) [Derivation] allocated by
-- cykInit and read+written by chunk supers.  Each cell is written by
-- exactly ONE super (the one computing it); TALM token ordering ensures
-- consumers see the write.  NO MVar / IORef synchronization needed
-- BEYOND the IORef pointing to the array (set once at init).
{-# NOINLINE g_d_cyk_kbest #-}
g_d_cyk_kbest :: IORef (IOArray (Int, Int) [Derivation])
g_d_cyk_kbest = unsafePerformIO (newIORef =<< IOA.newArray ((0, 0), (0, 0)) [])

-- Grammar lookup tables (loaded once).
{-# NOINLINE g_prods_bin_cyk_kbest #-}
g_prods_bin_cyk_kbest :: IORef (IOArray (Word8, Word8) [(Word8, Word16, Double)])
g_prods_bin_cyk_kbest = unsafePerformIO (newIORef =<< IOA.newArray ((0, 0), (0, 0)) [])

-- Chunk metadata: indexed by chunk_id.
{-# NOINLINE g_chunk_diag_cyk_kbest #-}
g_chunk_diag_cyk_kbest :: IORef (Ptr Int64)
g_chunk_diag_cyk_kbest = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_chunk_first_cyk_kbest #-}
g_chunk_first_cyk_kbest :: IORef (Ptr Int64)
g_chunk_first_cyk_kbest = unsafePerformIO (newIORef nullPtr)

{-# NOINLINE g_chunk_ncells_cyk_kbest #-}
g_chunk_ncells_cyk_kbest :: IORef (Ptr Int64)
g_chunk_ncells_cyk_kbest = unsafePerformIO (newIORef nullPtr)


readBytes :: FilePath -> Int -> IO (Ptr Word8)
readBytes path n = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes n :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src n
  return p


cykInit :: IO Int64
cykInit = do
  -- Allocate the cell array.
  d <- IOA.newArray ((0, 0), (cykN - 1, cykN - 1)) [] :: IO (IOArray (Int, Int) [Derivation])
  writeIORef g_d_cyk_kbest d

  -- Load grammar production tables.
  gBS <- BS.readFile (cykDataDir ++ "/grammar_prods.bin")
  let BSI.BS gFP _ = gBS
  withForeignPtr gFP $ \gP -> do
    let p32 = castPtr gP :: Ptr Word32
    nBinW <- peekElemOff p32 0
    nTermW <- peekElemOff p32 1
    let nBin = fromIntegral nBinW :: Int
        nTerm = fromIntegral nTermW :: Int
        pBin = gP `plusPtr` 8 :: Ptr Word8
    prodsBin <- IOA.newArray ((0, 0), (63, 63)) [] :: IO (IOArray (Word8, Word8) [(Word8, Word16, Double)])
    let goB !i
          | i >= nBin = return ()
          | otherwise = do
              let r = pBin `plusPtr` (i * 12)
              a <- peekElemOff (r :: Ptr Word8) 0
              b <- peekElemOff (r :: Ptr Word8) 1
              c <- peekElemOff (r :: Ptr Word8) 2
              lp <- peekElemOff (r `plusPtr` 4 :: Ptr Double) 0
              cur <- IOA.readArray prodsBin (b, c)
              IOA.writeArray prodsBin (b, c) ((a, fromIntegral i :: Word16, lp) : cur)
              goB (i + 1)
    goB 0
    writeIORef g_prods_bin_cyk_kbest prodsBin

    -- Initialize base diagonal from terminal productions.
    sP <- readBytes (cykDataDir ++ "/input.bin") cykN
    let pTermBase = pBin `plusPtr` (nBin * 12)
    prodsTerm <- IOA.newArray (0, 31) [] :: IO (IOArray Word8 [(Word8, Word16, Double)])
    let goT !i
          | i >= nTerm = return ()
          | otherwise = do
              let r = pTermBase `plusPtr` (i * 12)
              a <- peekElemOff (r :: Ptr Word8) 0
              sg <- peekElemOff (r :: Ptr Word8) 1
              lp <- peekElemOff (r `plusPtr` 4 :: Ptr Double) 0
              cur <- IOA.readArray prodsTerm sg
              IOA.writeArray prodsTerm sg ((a, fromIntegral (nBin + i) :: Word16, lp) : cur)
              goT (i + 1)
    goT 0
    let goCell !i
          | i >= cykN = return ()
          | otherwise = do
              sigma <- peekElemOff sP i
              prods <- IOA.readArray prodsTerm sigma
              let derivs = [ Derivation a 0 0 prodId 0 lp | (a, prodId, lp) <- prods ]
                  !sorted = foldl' (\acc d' -> insertTopK topK d' acc) [] derivs
              IOA.writeArray d (i, i) sorted
              goCell (i + 1)
    goCell 0

  -- Load chunk metadata from dag.bin into flat I64 arrays for fast lookup.
  dagBS <- BS.readFile (cykDataDir ++ "/dag.bin")
  let BSI.BS dagFP _ = dagBS
  diags <- mallocBytes (__N_CHUNKS__ * 8) :: IO (Ptr Int64)
  firsts <- mallocBytes (__N_CHUNKS__ * 8) :: IO (Ptr Int64)
  ncs <- mallocBytes (__N_CHUNKS__ * 8) :: IO (Ptr Int64)
  withForeignPtr dagFP $ \dagP -> do
    let p32d = castPtr dagP :: Ptr Word32
        loop !off !ndone
          | ndone >= __N_DIAGS__ = return ()
          | otherwise = do
              !n_cells <- peekElemOff p32d (off + 0)
              !n_chunks <- peekElemOff p32d (off + 1)
              let nC = fromIntegral n_chunks :: Int
                  goC !k
                    | k >= nC = return ()
                    | otherwise = do
                        let base = off + 2 + k * 4
                        cidW <- peekElemOff p32d (base + 0)
                        diagW <- peekElemOff p32d (base + 1)
                        firstW <- peekElemOff p32d (base + 2)
                        ncW <- peekElemOff p32d (base + 3)
                        pokeI64 diags  (fromIntegral cidW) (fromIntegral diagW)
                        pokeI64 firsts (fromIntegral cidW) (fromIntegral firstW)
                        pokeI64 ncs    (fromIntegral cidW) (fromIntegral ncW)
                        goC (k + 1)
              goC 0
              loop (off + 2 + nC * 4) (ndone + 1)
    loop 3 0
  writeIORef g_chunk_diag_cyk_kbest diags
  writeIORef g_chunk_first_cyk_kbest firsts
  writeIORef g_chunk_ncells_cyk_kbest ncs
  return 0
  where
    {-# INLINE pokeI64 #-}
    pokeI64 :: Ptr Int64 -> Int -> Int64 -> IO ()
    pokeI64 = pokeElemOff


cykChunk :: Int -> IO Int64
cykChunk !chunkId = do
  d <- readIORef g_d_cyk_kbest
  prodsBin <- readIORef g_prods_bin_cyk_kbest
  diagsP <- readIORef g_chunk_diag_cyk_kbest
  firstsP <- readIORef g_chunk_first_cyk_kbest
  ncsP <- readIORef g_chunk_ncells_cyk_kbest
  !diag64 <- peekElemOff diagsP chunkId
  !first64 <- peekElemOff firstsP chunkId
  !nc64 <- peekElemOff ncsP chunkId
  let !diag = fromIntegral diag64 :: Int
      !first = fromIntegral first64 :: Int
      !nc = fromIntegral nc64 :: Int
  let goCell !ofs
        | ofs >= nc = return ()
        | otherwise = do
            let !i = first + ofs
                !j = i + diag
            !derivs <- computeCell prodsBin d i j
            IOA.writeArray d (i, j) derivs
            goCell (ofs + 1)
  goCell 0
  return 0


{-# INLINE computeCell #-}
computeCell :: IOArray (Word8, Word8) [(Word8, Word16, Double)]
            -> IOArray (Int, Int) [Derivation] -> Int -> Int -> IO [Derivation]
computeCell !prodsBin !d !i !j = goK i []
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !leftDerivs <- IOA.readArray d (i, k)
          !rightDerivs <- IOA.readArray d (k + 1, j)
          !acc' <- combineSplit (fromIntegral k :: Word16) prodsBin leftDerivs rightDerivs acc
          goK (k + 1) acc'


{-# INLINE combineSplit #-}
combineSplit :: Word16 -> IOArray (Word8, Word8) [(Word8, Word16, Double)]
             -> [Derivation] -> [Derivation] -> [Derivation] -> IO [Derivation]
combineSplit !kw !prodsBin !lefts !rights !acc0 = goL lefts acc0
  where
    goL [] !acc = return acc
    goL (dL:rest) !acc = do
      !acc' <- goR dL rights acc
      goL rest acc'
    goR _ [] !acc = return acc
    goR !dL (dR:rest) !acc = do
      !prods <- IOA.readArray prodsBin (drvA dL, drvA dR)
      let !acc' = foldl' (insOne dL dR) acc prods
      goR dL rest acc'
    {-# INLINE insOne #-}
    insOne !dL !dR !acc (!a, !prodId, !lp) =
      let !newScore = drvScore dL + drvScore dR + lp
          !newDrv = Derivation a (drvA dL) (drvA dR) prodId kw newScore
      in insertTopK topK newDrv acc


cykOutput :: IO Int64
cykOutput = do
  d <- readIORef g_d_cyk_kbest
  topDerivs <- IOA.readArray d (0, cykN - 1)
  let !top1 = case topDerivs of (x:_) -> x; _ -> Derivation 0 0 0 0 0 0
      !cs = derivHash top1
  putStrLn ("CHECKSUM=" ++ wordToHex cs)
  return 0
  where
    derivHash d =
      let a = fromIntegral (drvA d) :: Word64
          b = fromIntegral (drvB d) :: Word64
          c = fromIntegral (drvC d) :: Word64
          p = fromIntegral (drvProd d) :: Word64
          k = fromIntegral (drvSplit d) :: Word64
          sQ = round (drvScore d * 1e6) :: Word64
      in ((((a * 31 + b) * 31 + c) * 31 + p) * 31 + k) * 31 + sQ
    wordToHex w = let s = showHex w "" in replicate (16 - length s) '0' ++ s
    showHex 0 acc = if null acc then "0" else acc
    showHex x acc =
      let (q, r) = x `divMod` 16
          ch = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                         else toEnum (fromEnum 'A' + fromIntegral r - 10)
      in showHex q (ch : acc)
"""


def emit(out_dir, data_dir, max_fanin):
    os.makedirs(out_dir, exist_ok=True)
    cfg = read_config(data_dir)
    n = cfg["N"]; nnt = cfg["N_NT"]; nsigma = cfg["N_SIGMA"]
    chunks = read_dag(data_dir)
    deps = compute_chunk_deps(chunks)
    max_chunks_per_diag = 0
    for d in set(c[1] for c in chunks):
        max_chunks_per_diag = max(max_chunks_per_diag, sum(1 for c in chunks if c[1] == d))
    max_chunk_fanin = max((len(v) for v in deps.values()), default=0)
    nlines, n_or = emit_fl(out_dir, chunks, deps, max_fanin)
    with open(os.path.join(out_dir, "cyk_kbest.hsk"), "w") as f:
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
    print(f"[gen_cyk_kbest_hs] {out_dir}: {len(chunks)} chunks, max_fanin={max_chunk_fanin}, "
          f"or_nodes={n_or}, .fl={nlines} lines")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--max-fanin", type=int, default=MAX_FANIN_DEFAULT)
    args = ap.parse_args()
    emit(args.out_dir, args.data_dir, args.max_fanin)


if __name__ == "__main__":
    main()
