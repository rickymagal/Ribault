{-# LANGUAGE BangPatterns #-}
module Main where

-- CYK parsing (k-best variant), sequential reference.
--
-- Cell representation: [Derivation], a Haskell list of records of the
-- top-K derivations of s[i..j], sorted by score descending.  This is the
-- representation production parsers (Lark, XGrammar-2, etc.) use when they
-- want diverse outputs for downstream reranking, in contrast to the
-- bitmask form which only tracks "which non-terminals can derive this
-- substring".
--
-- Heap allocation profile per cell:
--   - For each split point k in [i, j-1]:
--     - For each (B, C, prodId, lp) production:
--       - For each leftDeriv with drvA=B in d[i][k]:
--         - For each rightDeriv with drvA=C in d[k+1][j]:
--           - Allocate one new Derivation record
--           - Insert into top-K queue (if better than worst kept)
-- This generates O(N^3 * |G| * K^2) record allocations across the table,
-- with O(N^2 * K) records retained -- the kind of fine-grain GC pressure
-- that pushed Ribault to a 5x win on LCS small-N.

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Monad (forM_, when)
import Data.IORef
import qualified Data.Array.IO as IOA
import Data.Array.IO (IOArray)
import Data.List (foldl')


data Derivation = Derivation
  { drvA     :: {-# UNPACK #-} !Word8     -- A non-terminal
  , drvB     :: {-# UNPACK #-} !Word8     -- left child NT (0 for terminal)
  , drvC     :: {-# UNPACK #-} !Word8     -- right child NT (0 for terminal)
  , drvProd  :: {-# UNPACK #-} !Word16    -- production id
  , drvSplit :: {-# UNPACK #-} !Word16    -- split point k (0 for terminal)
  , drvScore :: {-# UNPACK #-} !Double
  }


-- Top-K limit per cell.  Higher K -> more heap allocation per cell, more
-- per-cell work.  K=8 is the production default of XGrammar / Lark.
topK :: Int
topK = 8


-- Insert a derivation into a top-K-sorted list (descending by score).
-- Drops the worst if list exceeds K.  O(K) per insert; K is small.
{-# INLINE insertTopK #-}
insertTopK :: Int -> Derivation -> [Derivation] -> [Derivation]
insertTopK k d ds = take k (insert d ds)
  where
    insert x [] = [x]
    insert x (y:ys)
      | drvScore x >= drvScore y = x : y : ys
      | otherwise = y : insert x ys


data Cfg = Cfg
  { cfgN :: !Int
  , cfgNNT :: !Int
  , cfgNSigma :: !Int
  }


parseCfg :: String -> Cfg
parseCfg s =
  let pairs = [(head ws, read (ws !! 1) :: Int) | l <- lines s, let ws = words l, length ws >= 2]
      look k = maybe 0 id (lookup k pairs)
  in Cfg (look "N") (look "N_NT") (look "N_SIGMA")


-- Production tables, both binary and terminal, indexed for fast lookup.
data Grammar = Grammar
  { gProdsBin :: !(IOA.IOArray (Word8, Word8) [(Word8, Word16, Double)])
                  -- prodsByBC[(B,C)] = [(A, prodId, log_prob)]
  , gProdsTerm :: !(IOA.IOArray Word8 [(Word8, Word16, Double)])
                   -- prodsTerm[sigma] = [(A, prodId, log_prob)]
  }


loadGrammar :: FilePath -> IO Grammar
loadGrammar path = do
  bs <- BS.readFile path
  let BSI.BS fp _ = bs
  p <- mallocBytes (BS.length bs) :: IO (Ptr Word8)
  withForeignPtr fp $ \src -> copyBytes p src (BS.length bs)
  -- header: 2 u32s
  let p32 = p `plusPtr` 0 :: Ptr Word32
  nBin  <- fmap fromIntegral (peekElemOff p32 0) :: IO Int
  nTerm <- fmap fromIntegral (peekElemOff p32 1) :: IO Int
  prodsBin <- IOA.newArray ((0, 0), (63, 63)) []
  prodsTerm <- IOA.newArray (0, 31) []
  -- bin prods: starting at offset 8, each record is 12 bytes (A,B,C,pad,double)
  let pBin = p `plusPtr` 8 :: Ptr Word8
  forM_ [0 .. nBin - 1] $ \i -> do
    let r = pBin `plusPtr` (i * 12)
    a <- peekElemOff (r :: Ptr Word8) 0
    b <- peekElemOff (r :: Ptr Word8) 1
    c <- peekElemOff (r :: Ptr Word8) 2
    lp <- peekElemOff (r `plusPtr` 4 :: Ptr Double) 0
    cur <- IOA.readArray prodsBin (b, c)
    IOA.writeArray prodsBin (b, c) ((a, fromIntegral i :: Word16, lp) : cur)
  -- term prods: starting at offset 8 + nBin*12, each record is 12 bytes (A,sigma,pad u16,double)
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


main :: IO ()
main = do
  args <- getArgs
  case args of
    (dir:_) -> run dir
    _ -> hPutStrLn stderr "usage: cyk_kbest_seq DATA_DIR"


run :: FilePath -> IO ()
run dir = do
  cfg <- parseCfg <$> readFile (dir ++ "/config.txt")
  let !n = cfgN cfg
  g <- loadGrammar (dir ++ "/grammar_prods.bin")
  sP <- readBytes (dir ++ "/input.bin") n

  -- d :: IOArray (Int, Int) [Derivation], indexed by (i, j).
  d <- IOA.newArray ((0, 0), (n - 1, n - 1)) [] :: IO (IOArray (Int, Int) [Derivation])

  -- Initialize base diagonal d[i][i] from terminal productions.
  forM_ [0 .. n - 1] $ \i -> do
    sigma <- peekElemOff sP i
    prods <- IOA.readArray (gProdsTerm g) sigma
    let derivs = [ Derivation a 0 0 prodId 0 lp | (a, prodId, lp) <- prods ]
    -- Sort desc by score, take top-K
    let !sorted = foldl' (\acc d' -> insertTopK topK d' acc) [] derivs
    IOA.writeArray d (i, i) sorted

  t0 <- getCurrentTime
  -- Wavefront: span = 1..N-1
  forM_ [1 .. n - 1] $ \span_ -> do
    forM_ [0 .. n - 1 - span_] $ \i -> do
      let !j = i + span_
      !derivs <- computeCell g d i j
      IOA.writeArray d (i, j) derivs
  t1 <- getCurrentTime
  let !secs = realToFrac (diffUTCTime t1 t0) :: Double

  topDerivs <- IOA.readArray d (0, n - 1)
  let !top1 = case topDerivs of
                (x:_) -> x
                _ -> Derivation 0 0 0 0 0 0
      !cs = derivHash top1
  putStrLn ("CHECKSUM=" ++ wordToHex cs)
  putStrLn ("TOP1=" ++ show (drvA top1, drvB top1, drvC top1, drvProd top1, drvSplit top1, drvScore top1))
  putStrLn ("RUNTIME_SEC=" ++ show secs)
  hFlush stdout


-- Compute one cell d[i][j] for i < j: top-K derivations.
{-# INLINE computeCell #-}
computeCell :: Grammar -> IOArray (Int, Int) [Derivation] -> Int -> Int -> IO [Derivation]
computeCell !g !d !i !j = goK i []
  where
    goK !k !acc
      | k >= j = return acc
      | otherwise = do
          !leftDerivs  <- IOA.readArray d (i, k)
          !rightDerivs <- IOA.readArray d (k + 1, j)
          !acc' <- combineSplit (fromIntegral k :: Word16) g leftDerivs rightDerivs acc
          goK (k + 1) acc'


{-# INLINE combineSplit #-}
combineSplit :: Word16 -> Grammar -> [Derivation] -> [Derivation]
             -> [Derivation] -> IO [Derivation]
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


-- Stable hash of top-1 derivation: a polynomial mix of (A, B, C, prod, k)
-- plus the score quantized to 1e6 precision.  (A, B, C, prod, k) is
-- deterministic across seq/strat/Ribault for the same grammar+input.
-- Quantized score is too IF the reduction order is consistent
-- (Strategies + cell-level kernels match seq within bit-identical scores).
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
