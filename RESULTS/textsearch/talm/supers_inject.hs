
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

-- Text-search helpers (injected by gen_talm_input.py)
tsCorpusDir :: String
tsCorpusDir = "/home/ricardomag/Desktop/Ribault/RESULTS/textsearch/corpus"

tsKwBytes :: BS.ByteString
tsKwBytes = BS.pack [70, 73, 78, 68, 77, 69]

tsPadInt :: Int -> Int -> String
tsPadInt w n = let s = show n in replicate (w - length s) '0' ++ s

-- Zero-allocation keyword counting (same algorithm as GHC benchmarks)
tsCountOcc :: BS.ByteString -> BS.ByteString -> Int
tsCountOcc buf kw = go 0 0
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
tsProcessRange :: Int -> Int -> IO Int64
tsProcessRange lo hi = go lo 0
  where
    go !i !acc
      | i >= hi   = return acc
      | otherwise = do
          let path = tsCorpusDir ++ "/file_" ++ tsPadInt 4 i ++ ".txt"
          contents <- BS.readFile path
          let !c = tsCountOcc contents tsKwBytes
          go (i + 1) (acc + fromIntegral c)
