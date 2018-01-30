
module Week7.JoinList where

import Data.Monoid
import Week7.Sized

data JoinList m a =
    Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1 --------------------------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append ((tag j1) <> (tag j2)) j1 j2
-- (+++) j1 j2 = Append (mappend (tag j1) (tag j2)) j1 j2

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty


-- Exercise 2 ------------------------
-- indexJ returnes the "a" at the [idx] starts at 0.
-- In the JoinList b a, "b" is Sized, so can help
-- Note: Not sure about this solution. How to test this?
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

indexJ i j
  | i < 0          = Nothing
  | i >= sizeOfJ j = Nothing

indexJ i (Single m a)
  | i == 0    = Just a
  | otherwise = Nothing

indexJ i (Append m a b)
  | i < sizeOfJ a = indexJ i a
  | otherwise     = indexJ (i - sizeOfJ a) b

indexJ _ _ = Nothing

-- dropJ
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i j
  | i <= 0                       = j
  | i >= sizeOfJ j = Empty

dropJ i j@(Single b a)
  | i == 0    = j
  | otherwise = Empty

dropJ i (Append m a b)
    | i < sizeOfJ a = let newA = dropJ i a
                      in newA +++ b
    | otherwise     = dropJ (i - sizeOfJ a) b

sizeOfJ :: (Sized m, Monoid m) => JoinList m a -> Int
sizeOfJ = (getSize . size . tag)

-- To test in ghci
-- a = Append (Size 3) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')) (Single (Size 1) 'c')
-- dropJ 1 a
-- indexJ 1 a

