{-# LANGUAGE OverloadedStrings, DeriveGeneric,
  FlexibleContexts,
  GeneralizedNewtypeDeriving
 #-}

import Web.Scotty
import qualified Web.Scotty as S
-- import Web.Scotty (ScottyM, ActionM, json, html, get, text, scotty, post, param, setHeader)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, object, (.=), json)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status

-- import for blaze html
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

-- request logger
import qualified Network.Wai.Middleware.RequestLogger as WaiLogger
-- static content service
import qualified Network.Wai.Middleware.Static as Static
import Network.Wai.Middleware.Static ((>->))

-- BlazeHtml tutorial
import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- import Control.Applicative (Applicative)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
-- import Control.Monad.Reader (MonadReader, ReaderT, asks,
--   runReaderT)
-- import Control.Monad.Trans.Class (MonadTrans, lift)
-- import Data.Aeson (Value (Null), (.=), object)
-- import Data.Default (def)
-- import qualified Data.Text as T
-- import Data.Text.Encoding (encodeUtf8)
-- import Data.Text.Lazy (Text)
-- import qualified Database.Persist as DB
-- import qualified Database.Persist.Postgresql as DB
-- import Network.HTTP.Types.Status (created201,
--   internalServerError500, notFound404)
-- import Network.Wai (Middleware)
-- import Network.Wai.Handler.Warp (Settings, defaultSettings,
--   setFdCacheDuration, setPort)
-- import Network.Wai.Middleware.RequestLogger (logStdout,
--   logStdoutDev)
-- import System.Environment (lookupEnv)
-- import Web.Heroku (parseDatabaseUrl)
-- import Web.Scotty.Trans (ActionT, Options, ScottyT,
--   defaultHandler, delete, get, json, jsonData, middleware,
--   notFound, param, post, put, scottyOptsT, settings,
--   showError, status, verbose)


main = scotty 3000 $ do
  middleware WaiLogger.logStdoutDev
  middleware $ Static.staticPolicy (Static.noDots >-> Static.addBase "content")
  get "/" $ do
    S.html "hello world"
  get "/a" $ S.text "get a"
  post "/a" $ S.text "post a"
  get "/todo" $ do
    S.html . renderHtml $ do
      H.h3 "Haskell todo list"
      H.ul $ do
        H.li "[] logging"
        H.li "[] exception"
        H.li"[] db transaction"
  get "/p" $ do
    p <- S.param "p"
    contentMaybe <- S.header "content-type"
    let content = case contentMaybe of
                    Just c -> c
                    Nothing -> "not-set"
    S.html $ mconcat ["parameter: p=", p, " content-type:  content"]
  get "/blaze/numbers" $ S.html . renderHtml $ numbers 20

-- ------------------------------------------------
-- blazeHtml https://jaspervdj.be/blaze/tutorial.html
numbers :: Int -> H.Html
numbers n = H.docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
  H.body $ do
    H.p "A list of natural numbers:"
    H.ul $ forM_ [1..n] (H.li . H.toHtml)
    userInfo $ Just $ User {userName="Neil",userId=1000}

userInfo :: Maybe User -> H.Html
userInfo u = H.div H.! A.id  "user-info" $ case u of
                Nothing ->
                    H.a H.! A.href "/login" $ "please login"
                Just user -> do
                    "User "
                    H.toHtml $ userName user
                    ". Id: "
                    H.toHtml $ userId user

-- ------------------------------------------
scotty_hello = do
  putStrLn "starting server at port 3000 ..."
  scotty 3000 routes

-- defines routes
routes :: S.ScottyM ()
routes = do
  get "/" showLandingPage

  post "/register" register

  get "/hello" $ S.text "hello world!"
  
  get "/hello/:name" $ do
    name <- S.param "name"
    S.text ("hello, " <> name <> "!")

  get "/users" $ S.json allUsers

  get "/user/:id" $ do
    id <- S.param "id"
    S.json (filter (matchesId id) allUsers)

  get "/:word" $ do
    beam <- S.param "word"
    S.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


showLandingPage :: S.ActionM ()
showLandingPage = do
  setHeader "Content-Type" "text/html"
  S.text ("<html><body>showLandingPage says: hello</body><html>")


register :: S.ActionM ()
register = do
  emailAddress <- S.param "email"
  registered <- liftIO (registerInterest emailAddress)
  case registered of
    Just errorMessage -> do
      S.json $ Data.Aeson.object ["error" .= errorMessage]
      S.status internalServerError500
    Nothing -> do
      S.json $ Data.Aeson.object ["ok" .= ("ok"::String)]


registrationFailure :: S.ActionM ()
registrationFailure = do
  S.json $ Data.Aeson.object ["error" .= ("Invalid request" :: String)]
  S.status badRequest400


registerInterest :: String -> IO (Maybe String)
registerInterest "a@b.com" = putStrLn "Registered!" >> return Nothing
registerInterest _ = return (Just "Yes!")


matchesId id User {userId=uid} =
  uid == id


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

