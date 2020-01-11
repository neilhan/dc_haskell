module Ch07_IO.ToUpper where

import System.IO
import Data.Char(toUpper)

-- 1 ------
-- main :: IO ()
-- main = do
--   inHandle <- openFile "app/Ch07_IO/ToUpper.hs" ReadMode
--   outHandle <- openFile "output.txt" WriteMode
--   transform inHandle outHandle
--   hClose inHandle
--   hClose outHandle

-- transform :: Handle -> Handle -> IO ()
-- transform inHandle outHandle = do
--   isEof <- hIsEOF inHandle
--   if isEof
--     then return ()
--     else do inStr <- hGetLine inHandle
--             hPutStrLn outHandle (map toUpper inStr)
--             transform inHandle outHandle

-- 2 ------
-- main :: IO ()
-- main = do
--   inHandle <- openFile "app/Ch07_IO/ToUpper.hs" ReadMode
--   outHandle <- openFile "output.txt" WriteMode
--   inStr <- hGetContents inHandle
--   hPutStr outHandle (map toUpper inStr)
--   hClose inHandle
--   hClose outHandle

-- -- 3 ------
-- main :: IO ()
-- main = do
--   inStr <- readFile "app/Ch07_IO/ToUpper.hs"
--   writeFile "output.txt" (map toUpper inStr)

-- 4 ------
-- main :: IO ()
-- main = interact (map toUpper)
-- -- runghc ToUpper.hs < input.txt > output.txt

-- 5 filter  ------
import Control.Monad
import Control.Monad.Reader

main :: IO ()
main = interact (unlines . filter (\s -> (elem 'A' s) || (elem 'a' s)) . lines . (++) "Data follows:\n" . map toUpper)
-- main = interact (unlines . filter (liftM2 (||) (elem 'A')  (elem 'a')) . lines . (++) "Data follows:\n" . map toUpper)
-- runghc ToUpper.hs < input.txt > output.txt

{-|
runghc app/Ch07_IO/ToUpper.hs
-}
