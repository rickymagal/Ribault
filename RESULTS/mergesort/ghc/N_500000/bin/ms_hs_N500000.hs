-- Auto-generated: parallel Merge Sort (par/pseq, NO Strategies)
{-# LANGUAGE BangPatterns #-}
import Control.Parallel (par, pseq)
import Control.DeepSeq
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- split em duas metades
split2 :: [Int] -> ([Int],[Int])
split2 []         = ([],[])
split2 [x]        = ([x],[])
split2 (x:y:zs)   = let (xs,ys) = split2 zs in (x:xs, y:ys)

-- merge (estável)
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- mergesort com par/pseq (sem Strategies)
msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  =
  let (a,b) = split2 xs
      goA   = force (msort a)
      goB   = force (msort b)
  in goA `par` goB `pseq` merge goA goB

-- garante avaliação total
forceList :: NFData a => [a] -> ()
forceList xs = xs `deepseq` ()

-- entrada (gerada pelo script)
xsInput :: [Int]
xsInput = [500000, 499999 .. 1]

main :: IO ()
main = do
  let !_ = length xsInput  -- force spine
  t0 <- getCurrentTime
  let ys = msort xsInput
  forceList ys `seq` return ()
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  let sorted = and (zipWith (<=) ys (tail ys))
  putStrLn $ "SORTED=" ++ show sorted
  putStrLn $ "SORTED_HEAD=" ++ show (take 10 ys)
  putStrLn $ "RUNTIME_SEC=" ++ show secs
