-- Auto-generated Dyck (GHC parallel baseline)
import Control.DeepSeq (NFData(..), force)
import Control.Parallel.Strategies (parTuple2, rdeepseq, using)

-- Parameters (compile-time)
n0     :: Int
n0     = 10000
p0     :: Int
p0     = 2
imb0   :: Int
imb0   = 75
delta0 :: Int
delta0 = -2

-- Utilities
repeatDyck :: Int -> [Int] -> [Int]
repeatDyck m acc = if m == 0 then acc else repeatDyck (m - 1) (1 : (-1) : acc)

generateDyck :: Int -> Int -> [Int]
generateDyck len d =
  let base = repeatDyck (len `div` 2) [] in
  if d == 0 then base
  else if d > 0 then base ++ replicate d 1
  else base ++ replicate (abs d) (-1)

append :: [a] -> [a] -> [a]
append [] ys     = ys
append (h:t) ys  = h : append t ys

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

takeN :: Int -> [a] -> [a]
takeN k _  | k <= 0 = []
takeN _ []          = []
takeN k (y:ys)      = y : takeN (k-1) ys

dropN :: Int -> [a] -> [a]
dropN k xs | k <= 0 = xs
dropN _ []          = []
dropN k (_:ys)      = dropN (k-1) ys

splitAtN :: Int -> [a] -> ([a],[a])
splitAtN k xs = (takeN k xs, dropN k xs)

-- analyseChunk (sum, min-prefix)
analyseChunk :: [Int] -> (Int, Int)
analyseChunk = go 0 0
  where
    go s mn []     = (s, mn)
    go s mn (x:xs) = let s1  = s + x
                         mn1 = if s1 < mn then s1 else mn
                     in go s1 mn1 xs

chunkPair :: [Int] -> (Int, Int)
chunkPair = analyseChunk

-- Work-skewed split with clamping: 1 <= k <= n-1
splitImb :: [a] -> ([a],[a])
splitImb xs =
  let n    = len xs
      kRaw = (n * (100 + imb0)) `div` 200
      k    = if n <= 1 then 1 else max 1 (min (n-1) kRaw)
  in splitAtN k xs

threshold :: Int
threshold = n0 `div` p0

checkRec :: [Int] -> (Int, Int)
checkRec lst
  | len lst <= threshold = chunkPair lst
  | otherwise =
      let (lft, rgt) = splitImb lst
          -- parallel evaluation of both halves
          (lr, rr) = (checkRec lft, checkRec rgt) `using` parTuple2 rdeepseq rdeepseq
          (s1,m1) = lr
          (s2,m2) = rr
      in ( s1 + s2
         , let v = s1 + m2 in if m1 < v then m1 else v )

validateDyck :: [Int] -> Bool
validateDyck xs = let (tot, mn) = checkRec xs in (tot == 0) && (mn >= 0)

main :: IO ()
main = do
  let inputSeq = generateDyck n0 delta0
  let ok = validateDyck inputSeq
  ok `seq` putStrLn (if ok then "OK" else "BAD")
