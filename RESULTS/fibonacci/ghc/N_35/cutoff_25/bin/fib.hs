{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Fibonacci with cutoff (GHC Strategies)
-- N=35  CUTOFF=25

import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

fibSeq :: Int -> Int
fibSeq n = go n 0 1
  where go !i !a !b
          | i <= 0    = a
          | otherwise = go (i - 1) b (a + b)

fib :: Int -> Int -> Int
fib cutoff n
  | n <= 1      = n
  | n <= cutoff = fibSeq n
  | otherwise   = runEval $ do
      a <- rpar (fib cutoff (n - 1))
      b <- rseq (fib cutoff (n - 2))
      return (a + b)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let !r = fib 25 35
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show r
  putStrLn $ "RUNTIME_SEC=" ++ show secs
