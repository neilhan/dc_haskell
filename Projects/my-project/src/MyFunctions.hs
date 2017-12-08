{-# LANGUAGE OverloadedStrings #-}

module MyFunctions (
  greet
  , doPrintMsg
) where

greet name = "Hello " ++ name ++ "!"

doPrintMsg msg n 
  | n > 0 = do 
              putStrLn msg
              doPrintMsg msg (n - 1)
  | otherwise = return ()
