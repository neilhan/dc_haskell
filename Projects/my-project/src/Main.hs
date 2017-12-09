{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude -- hiding () -- (Int, foldl, fromInteger, (+), (.), ($), IO, show, getLine, readLn, (++))

import Data.Time (getCurrentTime)
import Data.ByteString.Lazy.Char8 as Char8 hiding (putStrLn,readFile)
import Data.Aeson (encode)

import MyFunctions (greet, doPrintMsg)
import Control.Monad (replicateM_)

main :: IO ()
main = main3

-- ask for message, 
-- ask for how many times
-- print
main3 = do
  putStrLn "Provide a message:"
  msg <- getLine
  putStrLn "Provide an integer:"
  n <- readLn :: IO Int
  putStrLn $ "Repetting message " ++ show n ++ " times:"
  -- sequence_ [putStrLn msg | i <- [1..n]]
  replicateM_ n (putStrLn msg)
  -- doPrintMsg msg n

main2 = do
  let action = putStrLn "Hello World"
  action
  return ()

main1 = do
  putStrLn (greet "Neil")
  putStrLn (greet "World")
  printNumbers
  -- printConfig
  putStrLn "Time:"
  printTime
  -- encode [Int]
  printIntList


printIntList = do
  putStrLn getEncodedIntList

getEncodedIntList = Char8.unpack $ encode [1::Int, 2, 3]


printNumbers = do
  putStrLn (show (3 + 4 ))

printConfig = do
  content <- readFile "stack.yaml"
  putStrLn content

printTime = do
  time <- getCurrentTime
  putStrLn (show time)
