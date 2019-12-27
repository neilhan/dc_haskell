{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Ch06.TypeClass where

class Equal a where
  {-# MINIMAL equal | notEqual #-}
  equal :: a -> a -> Bool
  equal x y = not (notEqual x y)

  notEqual :: a -> a -> Bool
  notEqual x y = not (equal x y)

instance Equal String where
  equal (x:xs) (y:ys) = x == y && equal xs ys
  equal [] [] = True
  equal _ _ = False


data Color = Red | Green | Blue
  deriving (Show)

instance Equal Color where
  equal Red Red = True
  equal Green Green = True
  equal Blue Blue = True
  equal _ _ = False


instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where tryParse [] = []
          tryParse ((attempt, result): xs) =
            if (take (length attempt) value) == attempt
               then [(result, drop (length attempt) value)]
               else tryParse xs


main = do
  putStrLn "Enter double:"
  inStr <- getLine
  let inDouble = (read inStr) :: Double
  putStrLn $ "2 x " ++ inStr ++ " is " ++ show (inDouble * 2)



{-|

:l app/Ch06/TypeClass.hs

main

-}
