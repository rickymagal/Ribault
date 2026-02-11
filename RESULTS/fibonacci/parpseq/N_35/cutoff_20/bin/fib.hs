{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Fibonacci with cutoff (par/pseq, NO Strategies)
-- N=35  CUTOFF=20

import Control.Parallel (par, pseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

fibSeq :: Int -> Int
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n - 1) + fibSeq (n - 2)

fib :: Int -> Int -> Int
fib cutoff n
  | n <= 1      = n
  | n <= cutoff = fibSeq n
  | otherwise   =
      let a = fib cutoff (n - 1)
          b = fib cutoff (n - 2)
      in a `par` b `pseq` (a + b)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let !r = fib 20 35
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show r
  putStrLn $ "RUNTIME_SEC=" ++ show secs
