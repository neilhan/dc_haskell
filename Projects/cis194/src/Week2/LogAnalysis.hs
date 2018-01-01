{-# OPTIONS_GHC -Wall #-}

module Week2.LogAnalysis where

import Week2.Log
import Text.ParserCombinators.ReadP
import Data.List.Split (splitOn)

-- filter error,
-- sort; filter E >50; return [string]
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  fmap (\(LogMessage _ _ str)-> str)
    [e | e <- (inOrder (buildTree msgs)),
      case e of
        (LogMessage (Error level) _ _) -> if level > 50 then True else False
        _ -> False]

-- sort all messages
inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    Node l msg r -> (inOrder l) ++ [msg] ++ (inOrder r)
    Leaf -> []


-- build a sorted tree
buildTree :: [LogMessage] -> MessageTree
buildTree = foldl (\tree l -> insert l tree) Leaf

-- MessageTree operation
insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree =
  case (msg, tree) of
    (Unknown _, _ )               -> tree
    (m@(LogMessage _ ts _), Leaf) -> Node Leaf m Leaf
    (m@(LogMessage _ ts _), (Node l nm@(LogMessage _ nmts _) r)) ->
        case ts < nmts of
          True  -> Node (insert m l) nm r
          False -> Node l nm (insert m r)

-- parse message
parse :: String -> [LogMessage]
parse lines = fmap parseMessage (splitOn "\n" lines )


parseMessage :: String -> LogMessage
parseMessage line =
  case (readP_to_S logMessageParser line) of
    (a,_):_ -> a
    _ -> Unknown line


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