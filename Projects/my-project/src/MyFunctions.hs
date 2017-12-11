{-# LANGUAGE OverloadedStrings #-}

module MyFunctions where

greet name = "Hello " ++ name ++ "!"

doPrintMsg msg n 
  | n > 0 = do 
              putStrLn msg
              doPrintMsg msg (n - 1)
  | otherwise = return ()

class HasDef a where
  defValue :: a -> a

instance HasDef Bool where
  defValue _ = False


