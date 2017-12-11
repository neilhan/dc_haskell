{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = do
  putStrLn "starting server at port 3000 ..."
  scotty 3000 $ do
    get "/hello" $ do
      text "hello world!"

--  get "/:word" $ do
--    beam <- param "word"
--    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
