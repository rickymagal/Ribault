{-# LANGUAGE BangPatterns #-}
-- Auto-generated Dyck (GHC parallel baseline, index-based)
-- N=10000  P=4  IMB=80  DELTA=0

import Control.DeepSeq (NFData(..), force)
import Control.Parallel.Strategies (parTuple2, rdeepseq, using)
import Data.Int (Int64)

-- Parameters (compile-time)
n0, p0, imb0, delta0, totalLen0, threshold0 :: Int64
n0     = 10000
p0     = 4
imb0   = 80
delta0 = 0
totalLen0 = n0 + abs delta0
threshold0 = totalLen0 `div` p0

-- Generate element at index i of the Dyck sequence
gen :: Int64 -> Int64
gen i
  | i < n0    = if mod i 2 == 0 then 1 else -1
  | otherwise = if delta0 > 0 then 1 else -1

-- Analyse a range [start .. start+count-1]
analyseRange :: Int64 -> Int64 -> (Int64, Int64)
analyseRange start count = go 0 0 start
  where
    endIdx = start + count
    go !s !mn i
      | i >= endIdx = (s, mn)
      | otherwise   =
          let x   = gen i
              s1  = s + x
              mn1 = if s1 < mn then s1 else mn
          in go s1 mn1 (i + 1)

-- Integer split with clamping
splitK :: Int64 -> Int64
splitK count
  | count <= 1 = 1
  | kRaw < 1   = 1
  | kRaw >= count = count - 1
  | otherwise  = kRaw
  where kRaw = (count * (100 + imb0)) `div` 200

-- Recursive parallel validation
checkRec :: Int64 -> Int64 -> (Int64, Int64)
checkRec start count
  | count <= threshold0 = analyseRange start count
  | otherwise =
      let k = splitK count
          (lr, rr) = (checkRec start k, checkRec (start + k) (count - k))
                     `using` parTuple2 rdeepseq rdeepseq
          (s1, m1) = lr
          (s2, m2) = rr
      in ( s1 + s2
         , let v = s1 + m2 in if m1 < v then m1 else v )

validateDyck :: Bool
validateDyck =
  let (tot, mn) = checkRec 0 totalLen0
  in (tot == 0) && (mn >= 0)

main :: IO ()
main = do
  let ok = validateDyck
  ok `seq` putStrLn (if ok then "1" else "0")
