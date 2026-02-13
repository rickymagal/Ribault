{-# LANGUAGE BangPatterns #-}
-- Auto-generated: Matrix Multiply (GHC Strategies)
-- N=400

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Int (Int64)

type Matrix = [[Double]]

-- Deterministic LCG matching TALM implementation
lcg :: Int64 -> Int64 -> Double
lcg seed idx =
  let m = 2147483647 :: Int64
      a = 1103515245 :: Int64
      c = 12345 :: Int64
      val = (a * (seed + idx) + c) `mod` m
  in fromIntegral val / fromIntegral m

genMatrix :: Int64 -> Int -> Matrix
genMatrix seed n =
  [ [ lcg seed (fromIntegral i * fromIntegral n + fromIntegral j)
    | j <- [0..n-1] ] | i <- [0..n-1] ]

transpose' :: Matrix -> Matrix
transpose' [] = []
transpose' ([] : _) = []
transpose' xss = map head xss : transpose' (map tail xss)

dot :: [Double] -> [Double] -> Double
dot xs ys = sum (zipWith (*) xs ys)

matMul :: Matrix -> Matrix -> Matrix
matMul a b =
  let bt = transpose' b
  in parMap rdeepseq (\row -> map (dot row) bt) a

checksum :: Matrix -> Double
checksum = sum . map sum

main :: IO ()
main = do
  let n = 400
  t0 <- getCurrentTime
  let a = genMatrix 42 n
      b = genMatrix 137 n
  a `deepseq` b `deepseq` return ()
  let !c = force (matMul a b)
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
      cs   = truncate (checksum c * 1000000 :: Double) :: Int
  putStrLn $ "CHECKSUM=" ++ show cs
  putStrLn $ "RUNTIME_SEC=" ++ show secs
