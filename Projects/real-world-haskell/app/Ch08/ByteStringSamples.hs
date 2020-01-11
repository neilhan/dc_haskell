module Ch08.ByteStringSamples where

import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)

mockElfMagicByteString = L.pack [0x7f, 0x45, 0x4c, 0x46, 0x41, 0x42]

main = do
  L.putStr mockElfMagicByteString

{-|
 putStr =<< readFile "prices.csv"
 readFile "prices.csv" >>= putStr
-}
