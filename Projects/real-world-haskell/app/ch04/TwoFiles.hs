
import System.Environment (getArgs)

interactWith fn inFile outFile =
  do
    input <- readFile inFile
    writeFile outFile (fn input)


main = mainWith fn
  where
    mainWith fn = do
            args <- getArgs
            case args of
              [inFile, outFile] -> interactWith fn inFile outFile
              _ -> putStrLn "Error: exactly two arguments needed"

    fn = id
