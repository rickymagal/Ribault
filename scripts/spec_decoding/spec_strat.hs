{-# LANGUAGE BangPatterns #-}
module Main where

-- Speculative-decoding draft-tree benchmark, GHC Strategies parallel.
--
-- Batch axis is parallelized: each of the B requests runs as an independent
-- spark.  Within a request, the tree expansion + sum reduction is sequential
-- (same as spec_seq.hs).  This matches the dominant production pattern
-- (batched decode, each request processed independently per step).
--
-- The synthetic scoring + heap-allocated Node tree create the same GC
-- pressure as the sequential baseline; the multi-request parallelism is
-- where the spark pool gets exercised.

import Data.Word (Word64)
import Data.Bits ((.|.), shiftR)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Parallel.Strategies (withStrategy, parList, rdeepseq, rseq)


data Node = Node !Int !Double ![Node]


{-# INLINE lcgStep #-}
lcgStep :: Word64 -> Word64
lcgStep r = (6364136223846793005 * r + 1442695040888963407) .|. 0


{-# INLINE lcgToDouble #-}
lcgToDouble :: Word64 -> Double
lcgToDouble r =
  let !hi = fromIntegral (r `shiftR` 11) :: Double
  in hi / 9.007199254740992e15


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


{-# INLINE sumTree #-}
sumTree :: Node -> Double
sumTree (Node _ !s !cs) = s + go cs 0
  where
    go [] !acc = acc
    go (n:ns) !acc =
      let !x = sumTree n
      in go ns (acc + x)


-- One request: build tree + sum.  Pure function -- safe to fork as spark.
{-# NOINLINE oneRequest #-}
oneRequest :: Int -> Int -> Int -> Int -> Int -> Double
oneRequest !hidden !depth !branching !stepId !reqId =
  let !seed = lcgStep (fromIntegral (reqId * 100003 + stepId * 31337))
      !tree = buildTree hidden depth branching seed 0
  in sumTree tree


-- Per-step: parallelize across the batch with parList rdeepseq.
batchStep :: Int -> Int -> Int -> Int -> Int -> Double
batchStep !batch !depth !branching !hidden !stepId =
  let results = withStrategy (parList rdeepseq)
                  [ oneRequest hidden depth branching stepId r
                  | r <- [0 .. batch - 1]
                  ]
  in sum results


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
    _ -> hPutStrLn stderr "usage: spec_strat DATA_DIR"


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
