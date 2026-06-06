#!/usr/bin/env python3
"""Ribault spec-decoding driver: emits .fl + .hsk + supers_inject.hs.

Structure mirrors scripts/mergesort/gen_ms_hs.py exactly:
  - Flat .fl with one `processTree` super call per (stepId, reqId) pair.
  - The dependency graph is linear within each step (all step-S tasks
    depend on init, gather into a step-S barrier) and the step barriers
    chain to the next step.  This matches the production model of
    speculative decoding where decode steps are sequential per request
    but the batch within a step is parallel.
  - Single .hsk super (processTree) plus init/output/barrier.  No `.hss`
    recursion (the tree expansion happens in Haskell inside the super).

Avoids the Couillard codegen bugs flagged for N-queens (list-cons
recursion + mutually recursive top-level functions) by keeping the
recursion inside the super body where it's just plain Haskell.
"""
import argparse, os


SUPER_INIT     = 13
SUPER_PROCESS  = 12
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


def make_or_tree(dep_names, fl_lines, counter):
    """Binary OR-tree collapsing N tokens to 1."""
    if len(dep_names) == 1:
        return dep_names[0]
    cur = list(dep_names)
    while len(cur) > 1:
        nxt = []
        i = 0
        while i + 1 < len(cur):
            t = f"or_{counter[0]}"
            counter[0] += 1
            fl_lines.append(f"step_or {t}, {cur[i]}, {cur[i+1]}")
            nxt.append(t)
            i += 2
        if i < len(cur):
            nxt.append(cur[i])
        cur = nxt
    return cur[0]


def emit_fl(out_dir, batch, steps):
    fl_lines = [
        f"superinst('init',         {SUPER_INIT},     1, False, False)",
        f"superinst('processTree',  {SUPER_PROCESS},  1, False, False)",
        f"superinst('step_or',      {SUPER_OR},       1, False, False)",
        f"superinst('output',       {SUPER_RESULT},   1, False, False)",
        "avgtime('processTree', 100000)",
        "",
        "const seed_const, 0",
        "init ini, seed_const",
    ]
    counter = [0]
    prev_barrier = "ini"
    for s in range(steps):
        step_tokens = []
        for r in range(batch):
            task_id = s * batch + r
            fl_lines.append(f"const kk_{task_id}, {task_id}")
            fl_lines.append(f"processTree pt_{task_id}, {prev_barrier}, kk_{task_id}")
            step_tokens.append(f"pt_{task_id}")
        if len(step_tokens) == 1:
            prev_barrier = step_tokens[0]
        else:
            prev_barrier = make_or_tree(step_tokens, fl_lines, counter)
    fl_lines.append(f"output out, {prev_barrier}")
    with open(os.path.join(out_dir, "spec.fl"), "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return len(fl_lines)


HSK_TEMPLATE = """-- spec.hsk (auto-generated).  ASCII-only.

init_super seed =
  super initImpl seed (
    initImpl seed = unsafePerformIO specInit
  )

processTree_super dep idx =
  super processImpl dep idx (
    processImpl dep idx = unsafePerformIO (specProcess (fromIntegral idx))
  )

step_or_super l r =
  super stepOrImpl l r (
    stepOrImpl l r = l + r
  )

output_super dep =
  super outputImpl dep (
    outputImpl dep = unsafePerformIO specOutput
  )

main =
  let s = init_super 0
      a = processTree_super s 0
      c = output_super a
  in c
"""


INJECT_TEMPLATE = r"""import Data.Word (Word64)
import Data.Bits ((.|.), shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Int (Int64)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)


specBatch, specDepth, specBranching, specSteps, specHidden :: Int
specBatch     = __BATCH__
specDepth     = __DEPTH__
specBranching = __BRANCHING__
specSteps     = __STEPS__
specHidden    = __HIDDEN_DIM__


data Node = Node !Int !Double ![Node]


-- LCG identical to spec_seq.hs / spec_strat.hs.
{-# INLINE lcgStep #-}
lcgStep :: Word64 -> Word64
lcgStep r = (6364136223846793005 * r + 1442695040888963407) .|. 0

{-# INLINE lcgToDouble #-}
lcgToDouble :: Word64 -> Double
lcgToDouble r =
  let !hi = fromIntegral (r `shiftR` 11) :: Double
  in hi / 9.007199254740992e15

{-# INLINE specScore #-}
specScore :: Int -> Word64 -> Double
specScore !hd !seed = go 0 0 seed
  where
    go !i !acc !r
      | i >= hd = acc
      | otherwise =
          let !r' = lcgStep r
              !x = lcgToDouble r'
          in go (i + 1) (acc + x * x) r'


specBuildTree :: Int -> Int -> Int -> Word64 -> Int -> Node
specBuildTree !hidden !depth !branching !seed !nodeId =
  let !s = specScore hidden seed
  in if depth <= 0
       then Node nodeId s []
       else
         let !children = [ specBuildTree hidden (depth - 1) branching
                             (lcgStep (seed + fromIntegral i))
                             (nodeId * branching + i + 1)
                         | i <- [0 .. branching - 1]
                         ]
         in Node nodeId s children


{-# INLINE specSumTree #-}
specSumTree :: Node -> Double
specSumTree (Node _ !s !cs) = s + go cs 0
  where
    go [] !acc = acc
    go (n:ns) !acc =
      let !x = specSumTree n
      in go ns (acc + x)


-- Shared accumulator (MVar-protected since supers may fire concurrently
-- on multiple workers and IORef writes aren't atomic w.r.t. read-modify-write).
{-# NOINLINE g_acc_spec #-}
g_acc_spec :: MVar Double
g_acc_spec = unsafePerformIO (newMVar 0.0)


specInit :: IO Int64
specInit = do
  cur <- takeMVar g_acc_spec
  putMVar g_acc_spec 0.0
  return 0


-- Process one (stepId, reqId) pair.  task_id = stepId * batch + reqId
-- (as packed by the .fl generator).
specProcess :: Int -> IO Int64
specProcess !taskId = do
  let !stepId = taskId `div` specBatch
      !reqId  = taskId `mod` specBatch
      !seed = lcgStep (fromIntegral (reqId * 100003 + stepId * 31337))
      !tree = specBuildTree specHidden specDepth specBranching seed 0
      !v = specSumTree tree
  cur <- takeMVar g_acc_spec
  putMVar g_acc_spec (cur + v)
  return 0


specOutput :: IO Int64
specOutput = do
  cur <- takeMVar g_acc_spec
  putMVar g_acc_spec cur
  putStrLn ("CHECKSUM=" ++ show cur)
  return 0
"""


def emit(out_dir, data_dir):
    os.makedirs(out_dir, exist_ok=True)
    cfg = read_config(data_dir)
    batch = cfg["BATCH"]
    depth = cfg["DEPTH"]
    branching = cfg["BRANCHING"]
    steps = cfg["STEPS"]
    hidden = cfg["HIDDEN_DIM"]
    nlines = emit_fl(out_dir, batch, steps)
    with open(os.path.join(out_dir, "spec.hsk"), "w") as f:
        f.write(HSK_TEMPLATE)
    inj = (INJECT_TEMPLATE
           .replace("__BATCH__", str(batch))
           .replace("__DEPTH__", str(depth))
           .replace("__BRANCHING__", str(branching))
           .replace("__STEPS__", str(steps))
           .replace("__HIDDEN_DIM__", str(hidden)))
    with open(os.path.join(out_dir, "supers_inject.hs"), "w") as f:
        f.write(inj)
    print(f"[gen_specdec_hs] {out_dir}: batch={batch} depth={depth} branching={branching} "
          f"steps={steps} -> {batch * steps} tasks, .fl={nlines} lines")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    emit(args.out_dir, args.data_dir)


if __name__ == "__main__":
    main()
