{-# LANGUAGE BangPatterns #-}
module Main where

-- N-Queens sequential baseline in Haskell.  Full recursion from row=0
-- inside this binary; uses IMMUTABLE `Data.Vector.Unboxed Int` for
-- the placed-queens prefix — `V.snoc queens col` allocates a new
-- vector on every recursive call.  This is deliberate: it is the
-- idiomatic Haskell representation and the resulting heap-allocation
-- pressure is what nq_strat.hs / nq_parpseq.hs / the ribault_hs
-- super body also pay.  A mutable representation would break the
-- comparison because parallel variants must pass `queens` snapshots
-- between sparks/supers, which requires immutability.

import Data.Word (Word64)
import qualified Data.Vector.Unboxed as V
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)


readConfig :: FilePath -> IO Int
readConfig path = do
  s <- readFile path
  let kv = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
  return $ maybe 0 id (lookup "N" kv)


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


solve :: Int -> V.Vector Int -> Int -> Word64
solve !n !queens !row
  | row == n  = 1
  | otherwise = go 0 0
  where
    go !c !acc
      | c >= n    = acc
      | safeQ queens row c =
          let !q' = V.snoc queens c
              !sub = solve n q' (row + 1)
          in go (c + 1) (acc + sub)
      | otherwise = go (c + 1) acc


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: nq_seq_hs DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  n <- readConfig (dir ++ "/config.txt")
  t0 <- getCurrentTime
  let !total = solve n V.empty 0 :: Word64
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show total)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
