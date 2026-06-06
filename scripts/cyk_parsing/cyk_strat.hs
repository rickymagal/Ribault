{-# LANGUAGE BangPatterns #-}
module Main where

-- CYK parsing -- GHC Strategies (parList rseq) parallel variant.
--
-- Wavefront barrier per diagonal: within each diagonal d (span = d), all
-- cells are independent (depend only on cells in smaller diagonals).  We
-- compute the diagonal in parallel via `withStrategy (parList rseq)`.
-- Between diagonals there's an implicit barrier (must finish d before d+1
-- because cells in d+1 read from d and below).
--
-- Cells share the d-table as a mutable raw Ptr Word64 (allocated once at
-- start, passed to each spark via closure).  The base diagonal is filled
-- sequentially before any parallel work begins.
--
-- Inner cell kernel mirrors cyk_seq.hs / cyk_seq.c byte-for-byte.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Data.Word (Word8, Word64)
import Data.Bits ((.&.), (.|.), countTrailingZeros)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Monad (forM_)
import Control.Parallel.Strategies (withStrategy, parList, rseq)
import System.IO.Unsafe (unsafePerformIO)


data Cfg = Cfg { cfgN :: !Int, cfgNNT :: !Int, cfgNSigma :: !Int }


parseCfg :: String -> Cfg
parseCfg s =
  let pairs = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
      look k = maybe 0 id (lookup k pairs)
  in Cfg (look "N") (look "N_NT") (look "N_SIGMA")


readBytes :: FilePath -> Int -> IO (Ptr Word8)
readBytes path n = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes n :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src n
  return p


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _ -> hPutStrLn stderr "usage: cyk_strat DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  cfg <- parseCfg <$> readFile (dir ++ "/config.txt")
  let !n = cfgN cfg
      !nnt = cfgNNT cfg
      !nsigma = cfgNSigma cfg

  sP <- readBytes (dir ++ "/input.bin") n
  gP <- readBytes (dir ++ "/grammar.bin") (16 + nnt*nnt*8 + nsigma*8)
  let pbPtr = gP `plusPtr` 16 :: Ptr Word64
      ptPtr = gP `plusPtr` (16 + nnt*nnt*8) :: Ptr Word64

  dPtr <- mallocBytes (n * n * 8) :: IO (Ptr Word64)
  forM_ [0 .. n*n - 1] $ \i -> pokeElemOff dPtr i (0 :: Word64)
  forM_ [0 .. n - 1] $ \i -> do
    !sigma <- peekElemOff sP i
    !mask <- peekElemOff ptPtr (fromIntegral sigma)
    pokeElemOff dPtr (i * n + i) mask

  t0 <- getCurrentTime
  -- Outer loop over diagonals (must be sequential -- diagonal d+1 reads from d).
  -- Within a diagonal, the cells are independent: parList rseq forks sparks.
  forM_ [1 .. n - 1] $ \span_ -> do
    let !nCells = n - span_
        cells = [0 .. nCells - 1]
        results = withStrategy (parList rseq)
                    [ computeCell dPtr pbPtr nnt n i (i + span_) | i <- cells ]
    -- Write the computed accs back into the table.
    forM_ (zip cells results) $ \(i, !acc) ->
      pokeElemOff dPtr (i * n + (i + span_)) acc
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  top <- peekElemOff dPtr (0 * n + (n - 1))
  putStrLn ("CHECKSUM=" ++ wordToHex top)
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout


-- Compute one cell d[i][j].  Pure-from-the-outside (closes over read-only
-- bits of dPtr): the spark only reads dPtr/pbPtr and returns the new acc;
-- the writeback happens sequentially after the parList barrier so we never
-- read a cell that's being concurrently written.
{-# NOINLINE computeCell #-}
computeCell :: Ptr Word64 -> Ptr Word64 -> Int -> Int -> Int -> Int -> Word64
computeCell !dPtr !pbPtr !nnt !n !i !j = unsafePerformIO (goK i 0)
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !left  <- peekElemOff dPtr (i * n + k)
          !right <- peekElemOff dPtr ((k + 1) * n + j)
          if left == 0 || right == 0
            then goK (k + 1) acc
            else do
              !acc' <- accBits left right acc
              goK (k + 1) acc'
    accBits !lb !right !acc
      | lb == 0 = return acc
      | otherwise = do
          let !b = countTrailingZeros lb
              !lb' = lb .&. (lb - 1)
          !acc'' <- accBits2 right b acc
          accBits lb' right acc''
    accBits2 !rb !b !acc
      | rb == 0 = return acc
      | otherwise = do
          let !c = countTrailingZeros rb
              !rb' = rb .&. (rb - 1)
          !m <- peekElemOff pbPtr (b * nnt + c)
          accBits2 rb' b (acc .|. m)


wordToHex :: Word64 -> String
wordToHex w = pad (showHexUpper w "")
  where pad s = replicate (16 - length s) '0' ++ s

showHexUpper :: Word64 -> String -> String
showHexUpper 0 acc = if null acc then "0" else acc
showHexUpper n acc =
  let (q, r) = n `divMod` 16
      d = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                    else toEnum (fromEnum 'A' + fromIntegral r - 10)
  in showHexUpper q (d : acc)
