module Ch08.SortPrice where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

-- readPrice :: L.ByteString -> Maybe Int
readPrice str =
  case LC.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case LC.readInt (LC.tail rest) of
        Nothing -> Nothing
        Just (cents, _) ->
          Just (dollars * 100 + cents)

closingPrice = readPrice . (!!4) . LC.split ','

-- highestClosingPrice :: L.ByteString -> Maybe Int
highestClosingPrice = maximum . (Nothing :) . map closingPrice . LC.lines


highestFromFile :: FilePath -> IO ()
highestFromFile filePath = do
  content <- LC.readFile filePath
  print (highestClosingPrice content)


{-|

LC.pack "1.2" -- String to Data.ByteString.Lazy.ByteString
(Nothing :) -> function take list, append Nothing to the head.
  so that maximum receives a non-empty list

Ch08.SortPrice.highestFromFile "resources/prices.csv"
Ch08.SortPrice.highestClosingPrice LC.empty

-}
