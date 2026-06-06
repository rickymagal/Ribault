{-# LANGUAGE BangPatterns #-}
module Main where

-- CYK k-best parsing, GHC Strategies parallel.
--
-- Within each antidiagonal, cells are independent (they only read from
-- smaller diagonals which are already finalized).  parList rseq forks one
-- spark per cell; the inner-cell kernel allocates O(span * |G| * K^2)
-- Derivation records, dropping all but top-K, exactly the kind of
-- fine-grain churn that satures the GHC spark pool and provokes gen-0 GC.
-- Each diagonal acts as an implicit barrier because the writeback
-- happens after the parList completes.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Monad (forM_)
import qualified Data.Array.IO as IOA
import Data.Array.IO (IOArray)
import Data.List (foldl')
import Control.Parallel.Strategies (withStrategy, parList, rseq, rdeepseq)
import Control.DeepSeq (NFData(..), deepseq)
import System.IO.Unsafe (unsafePerformIO)


instance NFData Derivation where
  rnf (Derivation a b c p k s) = a `seq` b `seq` c `seq` p `seq` k `seq` s `seq` ()


data Derivation = Derivation
  { drvA     :: {-# UNPACK #-} !Word8
  , drvB     :: {-# UNPACK #-} !Word8
  , drvC     :: {-# UNPACK #-} !Word8
  , drvProd  :: {-# UNPACK #-} !Word16
  , drvSplit :: {-# UNPACK #-} !Word16
  , drvScore :: {-# UNPACK #-} !Double
  }


topK :: Int
topK = 8


{-# INLINE insertTopK #-}
insertTopK :: Int -> Derivation -> [Derivation] -> [Derivation]
insertTopK k d ds = take k (insert d ds)
  where
    insert x [] = [x]
    insert x (y:ys)
      | drvScore x >= drvScore y = x : y : ys
      | otherwise = y : insert x ys


data Cfg = Cfg { cfgN :: !Int, cfgNNT :: !Int, cfgNSigma :: !Int }


parseCfg :: String -> Cfg
parseCfg s =
  let pairs = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
      look k = maybe 0 id (lookup k pairs)
  in Cfg (look "N") (look "N_NT") (look "N_SIGMA")


data Grammar = Grammar
  { gProdsBin  :: !(IOA.IOArray (Word8, Word8) [(Word8, Word16, Double)])
  , gProdsTerm :: !(IOA.IOArray Word8 [(Word8, Word16, Double)])
  }


loadGrammar :: FilePath -> IO Grammar
loadGrammar path = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes (BS.length bs) :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src (BS.length bs)
  let p32 = p `plusPtr` 0 :: Ptr Word32
  nBin  <- fmap fromIntegral (peekElemOff p32 0) :: IO Int
  nTerm <- fmap fromIntegral (peekElemOff p32 1) :: IO Int
  prodsBin <- IOA.newArray ((0, 0), (63, 63)) []
  prodsTerm <- IOA.newArray (0, 31) []
  let pBin = p `plusPtr` 8 :: Ptr Word8
  forM_ [0 .. nBin - 1] $ \i -> do
    let r = pBin `plusPtr` (i * 12)
    a <- peekElemOff (r :: Ptr Word8) 0
    b <- peekElemOff (r :: Ptr Word8) 1
    c <- peekElemOff (r :: Ptr Word8) 2
    lp <- peekElemOff (r `plusPtr` 4 :: Ptr Double) 0
    cur <- IOA.readArray prodsBin (b, c)
    IOA.writeArray prodsBin (b, c) ((a, fromIntegral i :: Word16, lp) : cur)
  let pTermBase = pBin `plusPtr` (nBin * 12)
  forM_ [0 .. nTerm - 1] $ \i -> do
    let r = pTermBase `plusPtr` (i * 12)
    a <- peekElemOff (r :: Ptr Word8) 0
    sg <- peekElemOff (r :: Ptr Word8) 1
    lp <- peekElemOff (r `plusPtr` 4 :: Ptr Double) 0
    cur <- IOA.readArray prodsTerm sg
    IOA.writeArray prodsTerm sg ((a, fromIntegral (nBin + i) :: Word16, lp) : cur)
  return (Grammar prodsBin prodsTerm)


readBytes :: FilePath -> Int -> IO (Ptr Word8)
readBytes path n = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes n :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src n
  return p


-- Per-cell computation, pure-faced via unsafePerformIO so it can be evaluated
-- inside a Strategy.  Each spark only READS from cells in smaller diagonals,
-- which are finalized by the time the parList for the current diagonal runs.
{-# NOINLINE computeCellPure #-}
computeCellPure :: Grammar -> IOArray (Int, Int) [Derivation] -> Int -> Int -> [Derivation]
computeCellPure !g !d !i !j = unsafePerformIO $ do
  !result <- goK i []
  -- IMPORTANT: force the full list spine *and* every Derivation record
  -- before returning, otherwise `parList rseq` only forces the outermost
  -- WHNF and the actual work spills into later sequential array reads.
  let forceAll [] = ()
      forceAll (x:xs) = x `seq` forceAll xs
  forceAll result `seq` return result
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !leftDerivs  <- IOA.readArray d (i, k)
          !rightDerivs <- IOA.readArray d (k + 1, j)
          !acc' <- combineSplit (fromIntegral k :: Word16) g leftDerivs rightDerivs acc
          goK (k + 1) acc'


{-# INLINE combineSplit #-}
combineSplit :: Word16 -> Grammar -> [Derivation] -> [Derivation] -> [Derivation] -> IO [Derivation]
combineSplit !kw !g !lefts !rights !acc0 = goL lefts acc0
  where
    goL [] !acc = return acc
    goL (dL:rest) !acc = do
      !acc' <- goR dL rights acc
      goL rest acc'
    goR _ [] !acc = return acc
    goR !dL (dR:rest) !acc = do
      !prods <- IOA.readArray (gProdsBin g) (drvA dL, drvA dR)
      let !acc' = foldl' (insOne dL dR) acc prods
      goR dL rest acc'
    {-# INLINE insOne #-}
    insOne !dL !dR !acc (!a, !prodId, !lp) =
      let !newScore = drvScore dL + drvScore dR + lp
          !newDrv = Derivation a (drvA dL) (drvA dR) prodId kw newScore
      in insertTopK topK newDrv acc


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _ -> hPutStrLn stderr "usage: cyk_kbest_strat DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  cfg <- parseCfg <$> readFile (dir ++ "/config.txt")
  let !n = cfgN cfg
  g <- loadGrammar (dir ++ "/grammar_prods.bin")
  sP <- readBytes (dir ++ "/input.bin") n

  d <- IOA.newArray ((0, 0), (n - 1, n - 1)) [] :: IO (IOArray (Int, Int) [Derivation])

  forM_ [0 .. n - 1] $ \i -> do
    sigma <- peekElemOff sP i
    prods <- IOA.readArray (gProdsTerm g) sigma
    let derivs = [ Derivation a 0 0 prodId 0 lp | (a, prodId, lp) <- prods ]
        !sorted = foldl' (\acc d' -> insertTopK topK d' acc) [] derivs
    IOA.writeArray d (i, i) sorted

  t0 <- getCurrentTime
  forM_ [1 .. n - 1] $ \span_ -> do
    let !nCells = n - span_
        cells = [0 .. nCells - 1]
        results = withStrategy (parList rdeepseq)
                    [ computeCellPure g d i (i + span_) | i <- cells ]
    forM_ (zip cells results) $ \(i, !ds) ->
      IOA.writeArray d (i, i + span_) ds
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  topDerivs <- IOA.readArray d (0, n - 1)
  let !top1 = case topDerivs of (x:_) -> x; _ -> Derivation 0 0 0 0 0 0
      !cs = derivHash top1
  putStrLn ("CHECKSUM=" ++ wordToHex cs)
  putStrLn ("TOP1=" ++ show (drvA top1, drvB top1, drvC top1, drvProd top1, drvSplit top1, drvScore top1))
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout


derivHash :: Derivation -> Word64
derivHash d =
  let a = fromIntegral (drvA d) :: Word64
      b = fromIntegral (drvB d) :: Word64
      c = fromIntegral (drvC d) :: Word64
      p = fromIntegral (drvProd d) :: Word64
      k = fromIntegral (drvSplit d) :: Word64
      sQ = round (drvScore d * 1e6) :: Word64
  in ((((a * 31 + b) * 31 + c) * 31 + p) * 31 + k) * 31 + sQ


wordToHex :: Word64 -> String
wordToHex w = pad (showHexUpper w "")
  where pad s = replicate (16 - length s) '0' ++ s

showHexUpper :: Word64 -> String -> String
showHexUpper 0 acc = if null acc then "0" else acc
showHexUpper n acc =
  let (q, r) = n `divMod` 16
      ch = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                     else toEnum (fromEnum 'A' + fromIntegral r - 10)
  in showHexUpper q (ch : acc)
