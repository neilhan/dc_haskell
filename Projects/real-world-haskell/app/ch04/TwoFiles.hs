
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

    fn = unlines . splitLines


splitLines [] = []
splitLines chars =
  let (pref, suff) = break isLineTerm chars
  in pref : case suff of
    ('\r': '\n': rest) -> splitLines rest
    ('\r': rest) -> splitLines rest
    ('\n': rest) -> splitLines rest
    _ -> []

isLineTerm c =
  c == '\r' || c == '\n'
