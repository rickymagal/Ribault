{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Fibonacci with cutoff (GHC Strategies)
-- N=35  CUTOFF=20

import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

fibSeq :: Int -> Int
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n - 1) + fibSeq (n - 2)

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
  let !r = fib 20 35
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show r
  putStrLn $ "RUNTIME_SEC=" ++ show secs
