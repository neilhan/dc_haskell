
module Week3 where

import Data.Ord
import qualified Data.Map as Map

skips :: [a] -> [[a]]
skips xs =
  map (skips_stride xs) [0..(length xs - 1)]
  where
      skips_stride xs' stride =
          if stride < (length xs')
          then( head $ drop stride xs') : skips_stride (drop (stride + 1) xs') stride
          else []

-- finding local max, [1,2,0] -> [2]
-- [1,0,0] -> []
-- [1,2,1,2] -> [2] etc
localMax :: Ord a => [a] -> [a]
localMax (a:b:c:xs) =
  if a < b && c < b then b : (localMax (b:c:xs))
  else localMax (b:c:xs)
localMax _ = []

-- histogram
-- take a list of numbers, and count how many each 0-9 digits appeared in the list
-- to test run:
--    mapM_ print $ (formatHistogram . histogram) [1,2,3,3, 4, 9,4,4,4,5,6,7]
histogram :: (Num a, Ord a, Enum a) => [a]-> Map.Map a a
histogram xs =
  foldl accumulate Map.empty xs
  where accumulate cuMap b = Map.insert b ((Map.findWithDefault 0 b cuMap) +1) cuMap

histogramMax :: (Num a, Ord a, Enum a) => Map.Map a a -> a
histogramMax = Map.foldl (\a b -> if a>b then a else b) 0

-- get printable list of strings
formatHistogram :: (Num a, Ord a, Enum a) => Map.Map a a -> [String]
formatHistogram m =
  [([starOrSpace m x y| x<-[0..9]]) | y <- [(histogramMax m), ((histogramMax m) -1) .. 1]]
  where starOrSpace m x y
          | (Map.findWithDefault 0 x m) >= y = '*'
          | otherwise                        = ' '
