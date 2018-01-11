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