{-# OPTIONS_GHC -Wall #-}

module Week2.Parser where

import Week2.Log
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))


isVowel :: Char -> Bool
isVowel c = any (c == ) "aoeui"


vowel :: ReadP Char
vowel = satisfy isVowel


atLeastOne :: ReadP Char -> String -> [(String, String)]
atLeastOne parser input =
  case readP_to_S parser input of
    []                  -> []  -- failed pase, return fail.
    [(char, remainder)] ->
      case atLeastOne parser remainder of
        []                      -> [(char:"", remainder)]
        [(str, finalRemainder)] -> [(char:str, finalRemainder)]


atLeastOneVowel :: ReadP [Char]
atLeastOneVowel = many1 vowel


airport :: ReadP String
airport = do
  ap <- many1 (satisfy (`elem` ['A'..'Z']))
  satisfy (== ' ')
  return ap
-- airport = many1 (satisfy (`elem` ['A'..'Z']))


airport2 :: ReadP String
airport2 = do
  ap <- many1 (satisfy (`elem` ['A'..'Z']))
  return ap
-- airport = many1 (satisfy (`elem` ['A'..'Z']))

-- ddHHmmZ
timestamp :: ReadP (Int, Int, Int)
timestamp = do
  day <- numbers 2
  hour <- numbers 2
  minute <- numbers 2
  string "Z "
  if day<1 || day>31 || hour >23 || minute >59
  then pfail
  else return (day, hour, minute)

windSpeed :: String -> Maybe Int
windSpeed windInfo = parseMaybe windSpeedParser windInfo


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    [] -> Nothing
    ((result, _):_) -> Just result


windInfo :: ReadP (Int, Int, Maybe Int)
windInfo = do
  direction <- numbers 3
  speed <- numbers 2 <|> numbers 3
  gusts <- option Nothing (fmap Just gustParser)
  unit <- string "KT" <|> string "MPS"
  string " "
  return (direction, toMPS unit speed, fmap (toMPS unit) gusts)


windSpeedParser :: ReadP Int
windSpeedParser = do
  direction <- numbers 3
  speed <- numbers 2 <|> numbers 3
  unit <- string "KT" <|> string "MPS"
  return speed


gustParser :: ReadP Int
gustParser = do
  satisfy (== 'G')
  numbers 2 <|> numbers 3


toMPS unit speed =
  case unit of
    "KT" -> div speed 2
    "MPS" -> speed


numbers :: Int -> ReadP Int
numbers numOfDigits = fmap read (count numOfDigits digit)


digit :: ReadP Char
digit = satisfy (`elem` ['0'..'9'])

