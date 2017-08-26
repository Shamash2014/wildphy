{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}
module Server (runApp, app) where

import           Control.Monad.IO.Class (liftIO)
import           GHC.Generics
import           Data.Aeson (Value(..), object, (.=), FromJSON, ToJSON, encode)
import           Data.Monoid ((<>))
import           Data.DateTime (getCurrentTime, formatDateTime, DateTime)
import           Network.Wai (Application)
import qualified Web.Scotty as S
import qualified Data.Text.Lazy as T
import           Database.MongoDB
import           Network
import           System.Environment (lookupEnv)

data BotResponse = BotResponse {
                                  text :: T.Text,
                                  response_type :: T.Text
                               } deriving (Show, Generic)

data Timer = Timer {
                      author :: T.Text
                   , ts :: DateTime
                   , project :: T.Text
                   } deriving (Show, Generic)

instance FromJSON BotResponse
instance ToJSON BotResponse

verifyToken :: T.Text -> Bool
verifyToken token =
  token == "wYD16EOydM0eSmlvzRg5T5EV"

convertTime :: DateTime -> T.Text
convertTime time = T.pack $ formatDateTime "%s" time

makeBotResponse :: T.Text -> BotResponse
makeBotResponse resp = BotResponse { text = resp , response_type = "ephemeral"}

startTimer :: Pipe -> S.ActionM ()
startTimer pipe = do
  request <- S.param "text"
  token <- S.param "token"
  name <- S.param "user_name"
  time <- liftIO (getCurrentTime)
  res <- liftIO $ (run "wildphy"  pipe) $ insert "timers" [ "author" =: (T.unpack name)
                                                             , "ts" =: time
                                                             , "project" =: (T.unpack request)  ]
  liftIO $ print =<< ((run "wildphy"  pipe) allCollections) 
  if verifyToken token then
        S.json $ makeBotResponse $ "Hi, " <> name
                                    <> "! Time tracking started for *"
                                    <> request <> "*. _Starting_ time is "
                                    <> convertTime time
    else S.text $ "Error"

app' :: Pipe -> S.ScottyM ()
app' pipe  = do
  S.get "/" $ do
    S.text "hello"

  S.post "/timers/start" $ startTimer $ pipe

app :: Pipe -> IO Application
app pipe = S.scottyApp $ (app' pipe)

-- dbUrl = "mongodb://@ds052649.mlab.com:52649/wildphy" :: String

run name pipe act = access pipe master name act

runApp :: IO ()
runApp = do
  port <- lookupEnv "PORT"
  hostUrl <- readHostPortM "ds052649.mlab.com:52649" 
  pipe <- connect hostUrl
  case port of
    Just (value) -> flip S.scotty (app' pipe) $ read value
    Nothing   -> S.scotty 8080 (app' pipe)
