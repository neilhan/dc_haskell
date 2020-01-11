module Ch07_IO.TempFile where

import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catchIOError)
import Control.Exception (finally)


main :: IO ()
main = withTempFile "mytemp.txt" act

-- the task
-- parameter: path of the temp files, tempFile.
act :: FilePath -> Handle -> IO ()
act tempFileName tempHandle = do
  putStrLn "TempFile.hs"
  putStrLn $ "temp file at: " ++ tempFileName

  pos <- hTell tempHandle -- get the initial position
  putStrLn $ "Initial position is: " ++ show pos

  let tempData = show [1..10]
  putStrLn "write 1..10"
  hPutStrLn tempHandle tempData

  pos <- hTell tempHandle
  putStrLn $ "New position is: " ++ show pos

  hSeek tempHandle AbsoluteSeek 0

  c <- hGetContents tempHandle -- read entire file
  putStrLn c


-- take the temp file and perform the action
-- openTempFile use temp dir, or ./ when not available
-- when function terminates, file will be close.
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tempFileName actFn = do
  tempDir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
  (tempFile, tempHandle) <- openTempFile tempDir tempFileName

  finally (actFn tempFile tempHandle)
          (do hClose tempHandle)
              -- removeFile tempFile)
