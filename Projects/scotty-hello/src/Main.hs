{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics


main :: IO ()
main = do
  putStrLn "starting server at port 3000 ..."
  scotty 3000 routes

-- defines routes
routes = do
  get "/hello" $ text "hello world!"
  
  get "/hello/:name" $ do
    name <- param "name"
    text ("hello, " <> name <> "!")

  get "/users" $ json allUsers

  get "/user/:id" $ do
    id <- param "id"
    json (filter (matchesId id) allUsers)

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

matchesId id User {userId=uid} = uid == id

-- data --------------
data User = User {userId::Int, userName:: String} deriving (Show, Generic)

bob ::User
bob = User {userId = 1, userName = "bob"}

jenny :: User
jenny = User {userId = 2, userName = "jenny"}

allUsers :: [User]
allUsers = [bob, jenny]

instance ToJSON User
instance FromJSON User
