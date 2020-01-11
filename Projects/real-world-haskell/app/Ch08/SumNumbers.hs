module Ch08.SumNumbers (sumNumbers) where

sumNumbers = do
  contents <- getContents
  print (sumFile contents)
  where sumFile = sum . map read . words

main = sumNumbers


{-|
runghc app/Ch08/SumNumbers.hs
runghc app/Ch08/SumNumbers.hs <<-eof
-}
