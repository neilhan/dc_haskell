{-# OPTIONS_GHC -Wall #-}

module Week2.LogAnalysis where

import Week2.Log
import Text.ParserCombinators.ReadP
import Data.List.Split (splitOn)


parse :: String -> [LogMessage]
parse lines = fmap parseMessage (splitOn "\n" lines )


parseMessage :: String -> LogMessage
parseMessage line =
  case (readP_to_S logMessageParser line) of
    (a,_):_ -> a
    _ -> LogMessage Info 0 ("---failed---: " ++ line)


logMessageParser :: ReadP LogMessage
logMessageParser = do
  choice [logMsgParserInfo, logMsgParserWarning, logMsgParserError]


logMsgParserInfo = do
  code <- char 'I'
  ignoreSpaces
  ts <- number
  ignoreSpaces
  rest <- many1 (satisfy (\x->True))
  eof
  return (LogMessage Info (fromInteger ts) rest)


logMsgParserWarning = do
  code <- char 'W'
  ignoreSpaces
  ts <- number
  ignoreSpaces
  rest <- many1 (satisfy (\x->True))
  eof
  return (LogMessage Warning (fromInteger ts) rest)


logMsgParserError = do
  code <- char 'E'
  ignoreSpaces
  level <- number
  ignoreSpaces
  ts <- number
  ignoreSpaces
  rest <- many1 (satisfy (\x->True))
  eof
  return (LogMessage (Error (fromInteger level)) (fromInteger ts) rest)


ignoreSpaces =
  skipMany1 (satisfy (== ' '))

digit :: ReadP Char
digit = satisfy (`elem` ['0'..'9'])

number :: ReadP Integer
number = fmap read (many1 digit)