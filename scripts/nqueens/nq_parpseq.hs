{-# LANGUAGE BangPatterns #-}
module Main where

-- N-Queens par/pseq variant — same recursive structure as
-- nq_strat.hs but using raw Control.Parallel (par, pseq) sparks
-- instead of the Strategies parList combinator.  Above cutoff:
-- foldr par over children, foldr seq for the barrier, sum forces.

import Control.Parallel (par, pseq)
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as V
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)


readConfig :: FilePath -> IO (Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
  return (maybe 0 id (lookup "N" kv), maybe 5 id (lookup "CUTOFF" kv))


{-# INLINE safeQ #-}
safeQ :: V.Vector Int -> Int -> Int -> Bool
safeQ queens row col = go 0
  where
    go !r
      | r >= row  = True
      | otherwise =
          let !c = queens V.! r
          in if c == col then False
             else if c - r == col - row then False
             else if c + r == col + row then False
             else go (r + 1)

solveSeq :: Int -> V.Vector Int -> Int -> Word64
solveSeq !n !queens !row
  | row == n  = 1
  | otherwise = go 0 0
  where
    go !c !acc
      | c >= n    = acc
      | safeQ queens row c =
          let !q' = V.snoc queens c
              !sub = solveSeq n q' (row + 1)
          in go (c + 1) (acc + sub)
      | otherwise = go (c + 1) acc

solvePar :: Int -> Int -> V.Vector Int -> Int -> Word64
solvePar !cutoff !n !queens !row
  | row == n     = 1
  | row >= cutoff = solveSeq n queens row
  | otherwise =
      let children = [ solvePar cutoff n (V.snoc queens c) (row + 1)
                     | c <- [0 .. n - 1], safeQ queens row c ]
          -- Spark every child via par, force via foldr seq, then sum.
          forced  = foldr seq () children
          sparked = foldr par forced children
      in sparked `pseq` sum children


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: nq_parpseq DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  (n, cutoff) <- readConfig (dir ++ "/config.txt")
  t0 <- getCurrentTime
  let !total = solvePar cutoff n V.empty 0 :: Word64
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show total)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
