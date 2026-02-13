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

-- sequential mergesort (below cutoff)
msortSeq :: [Int] -> [Int]
msortSeq []  = []
msortSeq [x] = [x]
msortSeq xs  =
  let (a,b) = split2 xs
  in merge (msortSeq a) (msortSeq b)

-- mergesort com par/pseq (sparks only above cutoff=256)
msort :: [Int] -> [Int]
msort xs = msortGo (length xs) xs

msortGo :: Int -> [Int] -> [Int]
msortGo _ []  = []
msortGo _ [x] = [x]
msortGo n xs
  | n <= 256  = msortSeq xs
  | otherwise =
      let (a,b)  = split2 xs
          halfA  = (n + 1) `div` 2
          halfB  = n `div` 2
          goA    = force (msortGo halfA a)
          goB    = force (msortGo halfB b)
      in goA `par` goB `pseq` merge goA goB

-- garante avaliação total
forceList :: NFData a => [a] -> ()
forceList xs = xs `deepseq` ()

-- entrada (gerada pelo script)
xsInput :: [Int]
xsInput = [400000, 399999 .. 1]

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
