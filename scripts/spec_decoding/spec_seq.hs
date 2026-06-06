{-# LANGUAGE BangPatterns #-}
module Main where

-- Synthetic speculative-decoding draft-tree benchmark, sequential Haskell.
--
-- The data structure is deliberately HEAP-ALLOCATED:
--    data Node = Node !Int !Double ![Node]
-- with a Haskell list of children (cons cells) and a boxed Double score.
-- This is the same kind of churn EAGLE-3 / SpecInfer create per decode step
-- (Python list-of-dicts on the host side; the Haskell equivalent is records
-- + lists).  Strategies will fight GC on this; Trebuchet operates on
-- scalar operands by value through the work-stealing deque and avoids most
-- of the heap allocation associated with the spark pool.
--
-- The "scoring" inside each node does HIDDEN_DIM vector ops on LCG-derived
-- floats, giving each task ~10us-1ms of useful work depending on HIDDEN_DIM,
-- well inside the 100us-10ms window where TALM dispatch (~10-30us) amortizes.

import Data.Word (Word64)
import Data.Bits ((.|.), shiftR)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)


data Node = Node !Int !Double ![Node]


-- LCG identical to other benchmarks (gen_input.py LCG family).
{-# INLINE lcgStep #-}
lcgStep :: Word64 -> Word64
lcgStep r = (6364136223846793005 * r + 1442695040888963407) .|. 0
            -- mod 2^64 implicit in Word64; matches the Python `& (2^63-1)` modulo
            -- with one extra bit (sign), inconsequential for derived doubles.


-- Convert LCG state to a Double in [0, 1).
{-# INLINE lcgToDouble #-}
lcgToDouble :: Word64 -> Double
lcgToDouble r =
  let !hi = fromIntegral (r `shiftR` 11) :: Double
  in hi / 9.007199254740992e15  -- 2^53


-- Per-node "scoring" -- HIDDEN_DIM multiply-adds against an LCG-derived vector.
-- This is the synthetic compute that takes the place of the draft-LLM forward.
{-# INLINE score #-}
score :: Int -> Word64 -> Double
score !hiddenDim !seed = go 0 0 seed
  where
    go !i !acc !r
      | i >= hiddenDim = acc
      | otherwise =
          let !r' = lcgStep r
              !x = lcgToDouble r'
          in go (i + 1) (acc + x * x) r'


-- Build a tree of depth D, branching L.  Per node carries an Int token id
-- and the score from the synthetic scoring kernel.
buildTree :: Int -> Int -> Int -> Word64 -> Int -> Node
buildTree !hidden !depth !branching !seed !nodeId =
  let !s = score hidden seed
  in if depth <= 0
       then Node nodeId s []
       else
         let !children = [ buildTree hidden (depth - 1) branching
                             (lcgStep (seed + fromIntegral i))
                             (nodeId * branching + i + 1)
                         | i <- [0 .. branching - 1]
                         ]
         in Node nodeId s children


-- Sum scores over the tree -- proxy for verification reduction (root->leaf
-- path acceptance).  Forces every node to be evaluated.
{-# INLINE sumTree #-}
sumTree :: Node -> Double
sumTree (Node _ !s !cs) = s + go cs 0
  where
    go [] !acc = acc
    go (n:ns) !acc =
      let !x = sumTree n
      in go ns (acc + x)


-- One decode step over the whole batch.  Each request has its own seed
-- (request id mixed into LCG).
batchStep :: Int -> Int -> Int -> Int -> Int -> Double
batchStep !batch !depth !branching !hidden !stepId =
  go 0 0
  where
    go !r !acc
      | r >= batch = acc
      | otherwise =
          let !seed = lcgStep (fromIntegral (r * 100003 + stepId * 31337))
              !tree = buildTree hidden depth branching seed 0
              !v = sumTree tree
          in go (r + 1) (acc + v)


-- The full benchmark: K decode steps.
runBench :: Int -> Int -> Int -> Int -> Int -> Double
runBench !batch !depth !branching !hidden !steps =
  go 0 0
  where
    go !s !acc
      | s >= steps = acc
      | otherwise =
          let !v = batchStep batch depth branching hidden s
          in go (s + 1) (acc + v)


parseCfg :: String -> [(String, Int)]
parseCfg s =
  [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _ -> hPutStrLn stderr "usage: spec_seq DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  cfg <- parseCfg <$> readFile (dir ++ "/config.txt")
  let look k = maybe 0 id (lookup k cfg)
      !batch = look "BATCH"
      !depth = look "DEPTH"
      !branching = look "BRANCHING"
      !steps = look "STEPS"
      !hidden = look "HIDDEN_DIM"
  t0 <- getCurrentTime
  let !v = runBench batch depth branching hidden steps
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show v)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
