{-# LANGUAGE BangPatterns #-}
module Main where

-- N-Queens par/pseq variant.  Same per-prefix sequential solver
-- as nq_strat.hs but uses raw Control.Parallel (par, pseq) sparks.

import Control.Monad (forM_)
import Control.Parallel (par, pseq)
import Data.List (foldl')
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekElemOff)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)


readConfig :: FilePath -> IO (Int, Int, Int)
readConfig path = do
  s <- readFile path
  let kv = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
  return ( maybe 0 id (lookup "N"        kv)
         , maybe 0 id (lookup "CUTOFF"   kv)
         , maybe 0 id (lookup "N_STATES" kv) )

-- See nq_seq.hs for why we copy into a malloc'd buffer.
readStates :: FilePath -> Int -> Int -> IO [V.Vector Int]
readStates path nStates cutoff = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
      nbytes = nStates * cutoff * 4
  buf <- mallocBytes nbytes :: IO (Ptr Int32)
  withForeignPtr fp $ \src -> copyBytes (castPtr buf) (src `plusPtr` 8) nbytes
  let stateAt !si = V.generate cutoff $ \r ->
        fromIntegral $ BSI.accursedUnutterablePerformIO $
          peekElemOff buf (si * cutoff + r)
  return [stateAt si | si <- [0 .. nStates - 1]]


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

solveSub :: V.Vector Int -> Int -> Int -> Word64
solveSub !queens !row !n
  | row == n  = 1
  | otherwise = go 0 0
  where
    go !c !acc
      | c >= n    = acc
      | safeQ queens row c =
          let !q' = V.snoc queens c
              !sub = solveSub q' (row + 1) n
          in go (c + 1) (acc + sub)
      | otherwise = go (c + 1) acc


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _       -> hPutStrLn stderr "usage: nq_strat DATA_DIR"

run :: FilePath -> IO ()
run dir = do
  (n, cutoff, nStates) <- readConfig (dir ++ "/config.txt")
  states <- readStates (dir ++ "/states.bin") nStates cutoff
  t0 <- getCurrentTime
  let !counts  = map (\s -> solveSub s cutoff n) states
      !forced  = foldr seq () counts
      !sparked = foldr par forced counts
      !total   = sparked `pseq` sum counts :: Word64
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show total)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
