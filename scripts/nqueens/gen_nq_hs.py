#!/usr/bin/env python3
"""Generate Ribault N-Queens files with Haskell-implemented supers.

Mirror of gen_nq_c.py/gen_nq_rust.py with Haskell super bodies using
immutable Data.Vector.Unboxed (V.snoc per recursive call), so the
heap-allocation pressure is the same as the standalone nq_seq.hs and
nq_strat.hs variants.
"""

import argparse, os


SUPER_INIT   = 10
SUPER_SOLVE  = 11
SUPER_SYNC   = 15
SUPER_OUTPUT = 16
MAX_FANIN    = 30


HSK_TEMPLATE = """-- Auto-generated attn.hsk
output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO nqOutput
  )

sync_super c d =
  super syncImpl c d (
    syncImpl c d = unsafePerformIO nqSync
  )

solve_super sid d =
  super solveImpl sid d (
    solveImpl sid d = unsafePerformIO (nqSolve (fromIntegral sid))
  )

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO nqInit
  )

main =
  let s = init_super 0
      a = solve_super 0 s
      b = sync_super  0 a
  in output_super b
"""


INJECT_TEMPLATE = r"""import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, castPtr)
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word64)
import Control.Monad (forM_, when)

nqN, nqCutoff, nqNStates :: Int
nqN        = __N__
nqCutoff   = __CUTOFF__
nqNStates  = __N_STATES__

nqDataDir :: FilePath
nqDataDir = "__DATA_DIR__"

{-# NOINLINE g_states #-}
g_states :: IORef (Ptr Int32)
g_states = unsafePerformIO (newIORef nullPtr)
{-# NOINLINE g_counts #-}
g_counts :: IORef (Ptr Word64)
g_counts = unsafePerformIO (newIORef nullPtr)

nqInit :: IO Int64
nqInit = do
  bs <- BS.readFile (nqDataDir ++ "/states.bin")
  let !(BSI.BS fp _) = bs
  buf <- mallocBytes (nqNStates * nqCutoff * 4) :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes buf (src `plusPtr` 8) (nqNStates * nqCutoff * 4)
  writeIORef g_states (castPtr buf)
  cnts <- mallocBytes (nqNStates * 8) :: IO (Ptr Word64)
  forM_ [0 .. nqNStates - 1] $ \i -> pokeElemOff cnts i (0 :: Word64)
  writeIORef g_counts cnts
  return 0


{-# INLINE nqSafe #-}
nqSafe :: V.Vector Int -> Int -> Int -> Bool
nqSafe queens row col = go 0
  where
    go !r
      | r >= row  = True
      | otherwise =
          let !c = queens V.! r
          in if c == col then False
             else if c - r == col - row then False
             else if c + r == col + row then False
             else go (r + 1)

nqSolveSub :: V.Vector Int -> Int -> Word64
nqSolveSub !queens !row
  | row == nqN = 1
  | otherwise = go 0 0
  where
    go !c !acc
      | c >= nqN  = acc
      | nqSafe queens row c =
          let !q' = V.snoc queens c
              !sub = nqSolveSub q' (row + 1)
          in go (c + 1) (acc + sub)
      | otherwise = go (c + 1) acc


nqSolve :: Int -> IO Int64
nqSolve !si = do
  statesP <- readIORef g_states
  countsP <- readIORef g_counts
  prefix <- V.generateM nqCutoff $ \r -> do
    !v <- peekElemOff statesP (si * nqCutoff + r)
    return (fromIntegral v :: Int)
  let !cnt = nqSolveSub prefix nqCutoff
  pokeElemOff countsP si cnt
  return 0


nqSync :: IO Int64
nqSync = return 0


nqOutput :: IO Int64
nqOutput = do
  countsP <- readIORef g_counts
  let goSum !i !acc
        | i >= nqNStates = return acc
        | otherwise = do
            !v <- peekElemOff countsP i
            goSum (i + 1) (acc + v)
  !t <- goSum 0 0
  putStrLn ("CHECKSUM=" ++ show t)
  return (fromIntegral t :: Int64)
"""


def emit_fl(out_dir, n_states):
    fl = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('solve',  {SUPER_SOLVE},  1, False, False)",
        f"superinst('sync',   {SUPER_SYNC},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('solve', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for s in range(n_states):
        fl.append(f"const sid_{s}, {s}")
        fl.append(f"solve sv_{s}, sid_{s}, ini")
    current = [f"sv_{s}" for s in range(n_states)]
    sync_id = 0
    if len(current) == 1:
        root = current[0]
    else:
        while len(current) > 1:
            nxt = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl.append(f"const {cname}, 0")
                fl.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                nxt.append(sname); sync_id += 1
            current = nxt
        root = current[0]
    fl.append(f"output out, {root}")
    with open(os.path.join(out_dir, "attn.fl"), "w") as f:
        f.write("\n".join(fl) + "\n")


def emit(out_dir, data_dir, N, CUTOFF, n_states):
    os.makedirs(out_dir, exist_ok=True)
    emit_fl(out_dir, n_states)
    print(f"[gen_nq_hs] wrote {out_dir}/attn.fl  (n_states={n_states})")
    with open(os.path.join(out_dir, "attn.hsk"), "w") as f: f.write(HSK_TEMPLATE)
    print(f"[gen_nq_hs] wrote {out_dir}/attn.hsk")
    inj = (INJECT_TEMPLATE
           .replace("__N__", str(N))
           .replace("__CUTOFF__", str(CUTOFF))
           .replace("__N_STATES__", str(n_states))
           .replace("__DATA_DIR__", data_dir))
    with open(os.path.join(out_dir, "supers_inject.hs"), "w") as f: f.write(inj)
    print(f"[gen_nq_hs] wrote {out_dir}/supers_inject.hs")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.out_dir, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CUTOFF"]), int(cfg["N_STATES"]))


if __name__ == "__main__":
    main()
