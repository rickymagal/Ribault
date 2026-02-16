{-# LANGUAGE BangPatterns #-}
-- Auto-generated: N-Queens counting (par/pseq, NO Strategies)
-- N=14  CUTOFF=2  BITS_PER_QUEEN=4

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Int  (Int64)
import Control.Parallel (par, pseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

type Board = Int64

getQueen :: Board -> Int -> Int
getQueen cols row = fromIntegral ((cols `shiftR` (4 * row)) .&. 15)

packQueen :: Board -> Int -> Int -> Board
packQueen cols row col = cols .|. (fromIntegral col `shiftL` (4 * row))

isSafe :: Board -> Int -> Int -> Bool
isSafe cols row col = go 0
  where
    go r
      | r >= row  = True
      | otherwise =
          let c = getQueen cols r
          in  c /= col && abs (c - col) /= (row - r) && go (r + 1)

solveSeq :: Int -> Int -> Board -> Int
solveSeq !n !row !cols
  | row >= n  = 1
  | otherwise = sum [ solveSeq n (row + 1) (packQueen cols row col)
                    | col <- [0..n-1], isSafe cols row col ]

nqueens :: Int -> Int -> Int -> Board -> Int
nqueens !n !cutoff !row !cols
  | row >= n      = 1
  | row >= cutoff = solveSeq n row cols
  | otherwise     = parSum [ nqueens n cutoff (row + 1) (packQueen cols row col)
                           | col <- [0..n-1], isSafe cols row col ]

parSum :: [Int] -> Int
parSum []     = 0
parSum [x]    = x
parSum (x:xs) = let s = parSum xs in x `par` s `pseq` (x + s)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let !r = nqueens 14 2 0 0
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show r
  putStrLn $ "RUNTIME_SEC=" ++ show secs
