module Ch07_IO.BasicIO
  where
-- main = do
--   putStrLn "Greetings, name?"
--   inStr <- getLine
--   putStrLn $ "Hello, " ++ inStr ++ " welcome"

name2replay name = "Hello, " ++ name ++ ", welcome. " ++ charCount
  where charCount = show (length name)

main = do
  putStrLn "Greetings, name?"
  inStr <- getLine
  let outStr = name2replay inStr
  putStrLn outStr
  -- where name2replay name = "Hello, " ++ name ++ ", welcome. " ++ charCount
  --         where charCount = show (length name)

{-|
runghc app/Ch07_IO/BasicIO.hs
echo Neil | runghc app/Ch07_IO/BasicIO.hs
-}
