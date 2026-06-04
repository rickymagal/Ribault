{-# LANGUAGE BangPatterns #-}
module Main where

-- N-Queens sequential baseline in Haskell.  Uses IMMUTABLE
-- `Data.Vector.Unboxed Int` for the placed-queens prefix —
-- `V.snoc queens col` allocates a new vector on every recursive
-- call.  This is the idiomatic Haskell representation; the
-- resulting heap allocation pressure is what the parallel
-- variants (nq_strat, nq_parpseq, the ribault_hs supers) also
-- pay.  A mutable representation would break the comparison
-- because parallel variants must pass `queens` snapshots between
-- sparks/supers, which requires immutability.

import Data.Bits ((.&.), shiftR)
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as V
import Foreign.ForeignPtr (withForeignPtr)
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


-- states.bin layout: [n_states i32, cutoff i32] then n_states*cutoff i32s.
readStates :: FilePath -> Int -> Int -> IO [V.Vector Int]
readStates path nStates cutoff = do
  bs <- BS.readFile path
  let !(BSI.BS fp _) = bs
  withForeignPtr fp $ \src8 -> do
    let src = castPtr (src8 `plusPtr` 8) :: Ptr Int32   -- skip 8-byte header
        stateAt si = V.generate cutoff $ \r ->
          fromIntegral $ BSI.accursedUnutterablePerformIO $
            peekElemOff src (si * cutoff + r)
    let go !si !acc
          | si >= nStates = return (reverse acc)
          | otherwise     = go (si + 1) (stateAt si : acc)
    go 0 []


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
    _       -> hPutStrLn stderr "usage: nq_seq_hs DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  (n, cutoff, nStates) <- readConfig (dir ++ "/config.txt")
  states <- readStates (dir ++ "/states.bin") nStates cutoff
  t0 <- getCurrentTime
  let !total = sum [solveSub s cutoff n | s <- states] :: Word64
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn ("CHECKSUM=" ++ show total)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout
