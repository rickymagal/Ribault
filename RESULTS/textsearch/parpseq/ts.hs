{-# LANGUAGE BangPatterns #-}
-- Auto-generated: text search (par/pseq, NO Strategies)
-- N_FILES=50  KEYWORD="FINDME"  N_FUNCS=12

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Parallel (par, pseq)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

corpusDir :: String
corpusDir = "/home/ricardomag/Desktop/Ribault/RESULTS/textsearch/corpus"

kwBytes :: BS.ByteString
kwBytes = BS.pack [70, 73, 78, 68, 77, 69]

padInt :: Int -> Int -> String
padInt w n = let s = show n in replicate (w - length s) '0' ++ s

filePath :: Int -> FilePath
filePath i = corpusDir ++ "/file_" ++ padInt 4 i ++ ".txt"

countOcc :: BS.ByteString -> BS.ByteString -> Int
countOcc buf kw = go 0 0
  where
    kwLen  = BS.length kw
    bufLen = BS.length buf
    end    = bufLen - kwLen + 1
    go !i !acc
      | i >= end    = acc
      | matchAt i 0 = go (i + kwLen) (acc + 1)
      | otherwise   = go (i + 1) acc
    matchAt !off !k
      | k >= kwLen = True
      | BSU.unsafeIndex buf (off + k) /= BSU.unsafeIndex kw k = False
      | otherwise  = matchAt off (k + 1)

-- Process a range of files [lo, hi), returning sum of counts
processRange :: (Int, Int) -> Int
processRange (lo, hi) = unsafePerformIO $ go lo 0
  where
    go !i !acc
      | i >= hi   = return acc
      | otherwise = do
          contents <- BS.readFile (filePath i)
          let !c = countOcc contents kwBytes
          go (i + 1) (acc + c)

parSum :: [Int] -> Int
parSum []     = 0
parSum [x]    = x
parSum (x:xs) = let s = parSum xs in x `par` s `pseq` (x + s)

main :: IO ()
main = do
  t0 <- getCurrentTime
  let ranges = [(0, 4), (4, 8), (8, 12), (12, 16), (16, 20), (20, 25), (25, 29), (29, 33), (33, 37), (37, 41), (41, 45), (45, 50)]
      results = map processRange ranges
      !total = parSum results
  t1 <- getCurrentTime
  let secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "RESULT=" ++ show total
  putStrLn $ "RUNTIME_SEC=" ++ show secs
