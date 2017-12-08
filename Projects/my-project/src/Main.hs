{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.ByteString.Lazy.Char8 as Char8
import Data.Aeson (encode)

import MyFunctions (greet, doPrintMsg)
import Control.Monad (replicateM_)

main :: IO ()
main = main3

-- ask for message, 
-- ask for how many times
-- print
main3 = do
  Prelude.putStrLn "Provide a message:"
  msg <- getLine
  Prelude.putStrLn "Provide an integer:"
  n <- readLn :: IO Int
  Prelude.putStrLn $ "Repetting message " ++ show n ++ " times:"
  -- sequence_ [Prelude.putStrLn msg | i <- [1..n]]
  replicateM_ n (Prelude.putStrLn msg)
  -- doPrintMsg msg n

main2 = do
  let action = Prelude.putStrLn "Hello World"
  action
  return ()

main1 = do
  Prelude.putStrLn (greet "Neil")
  Prelude.putStrLn (greet "World")
  printNumbers
  -- printConfig
  Prelude.putStrLn "Time:"
  printTime
  -- encode [Int]
  printIntList


printIntList = do
  Prelude.putStrLn getEncodedIntList

getEncodedIntList = Char8.unpack $ encode [1::Int, 2, 3]


printNumbers = do
  Prelude.putStrLn (show (3 + 4 ))

printConfig = do
  content <- Prelude.readFile "stack.yaml"
  Prelude.putStrLn content

printTime = do
  time <- getCurrentTime
  Prelude.putStrLn (show time)
