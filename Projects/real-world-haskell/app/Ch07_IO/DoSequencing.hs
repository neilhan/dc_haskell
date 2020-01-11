module Ch07_IO.DoSequencing
  where

import Data.Char (toUpper)

main :: IO ()
-- main = do putStrLn "name?"
--   inStr <- getLine
--   putStrLn $ "Hello, " ++ inStr
main = putStrLn "name?" >>
  getLine >>=
  (\inStr -> putStrLn $ "hello, " ++ inStr)


isGreen :: IO Bool
isGreen = do
  putStrLn "Green?"
  inStr <- getLine
  return ((toUpper . head $ inStr) == 'Y')
  return $ isYes inStr

isYes :: String -> Bool
isYes s = ((toUpper . head) s) == 'Y'


-- Return returns a Monad a
