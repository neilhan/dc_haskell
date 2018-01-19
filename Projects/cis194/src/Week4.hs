module Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' =
  sum
  . filter even
  . takeWhile (/=1)
  . iterate (\n -> if even n then div n 2 else 3 * n + 1)


-- folding a list to a balanced binary tree

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree =
  foldr addToTree Leaf
  where addToTree a Leaf
            = Node 0 Leaf a Leaf
        addToTree a (Node lv tl v tr)
            = if (level tl) <= (level tr) then  -- add to left
                  let new_tl = addToTree a tl in
                  Node (level new_tl + 1) new_tl v tr
              else -- add to right
                  let new_tr = addToTree a tr in
                  Node (level new_tr + 1) tl v new_tr
        level Leaf = -1
        level (Node lv _ _ _) = lv

-- xor [True, False, True] => False
-- xor [True, False, True, True ] => True
-- when bool array has odd number of Trues, return true
xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

-- map as fold, map behave as standard map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\v cu-> [f v] ++ cu) []

-- foldl implemented with foldr
foldl' f x = foldr (flip f) x . reverse


-- exercise 4, finding odd prime numbers
-- Read about the Sieve of Sundaram. Implement the algorithm using function composition.
-- Given an integer n, your function should generate all the odd prime numbers up to 2n + 2.
-- http://en.wikipedia.org/wiki/Sieve_of_Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n
  | n > 0  =
       let excludes n = [(i + j + 2 * i * j) | i<-[1..n], j<-[i..n], (i+j+2*i*j) <=n ]
       in map (\x->x*2+1) $ filter (\n->not (elem n (excludes n))) [1..n]
  | n <= 0 = []
