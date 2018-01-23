{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


module Week6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib i = (fib (i-1)) + (fib (i-2))

fibs1 :: [Integer]
fibs1 = [fib i | i <- [0,1..]]


-- this doesn't work as asked, although it works, but the running time is not meeting the requirements.
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2' i| i<-[0,1..]]
        where
            fibs2' i = let [a, b] = take 2 (drop i fibs2)
                      in a + b

-- this is learned from https://github.com/bschwb/cis194-solutions/blob/master/06-lazy/Fibonacci.hs
fibs2a :: [Integer]
fibs2a = fibs2v 0 1
        where fibs2v a b = a : fibs2v b (a + b)


-- exercise 3 ---------------------------------------
-- polymorphic stream
-- streamToList:: Stream a -> [a]

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . (take 20) . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- this is a testing tool
listToStream :: Show a => [a] -> Stream a
listToStream (a : as) = Cons a (listToStream as)


-- exercise 4 ---------------------------------------

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- exercise 5 ---------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- this is not an elegant impl
-- To see the interleaveStream impl: https://github.com/bschwb/cis194-solutions/blob/master/06-lazy/Fibonacci.hs
-- ruler :: Stream Integer
ruler =
  let
      find (i, p) ans theInt
          | i > theInt || p > theInt = ans
          | mod theInt p == 0 = find (i + 1, p * 2) i theInt
          | otherwise = find (i + 1, p * 2) ans theInt

  in
    streamMap (find (0, 1) 0) (listToStream [1,2..])

