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
import           System.Environment (lookupEnv)

data BotResponse = BotResponse {
                                  text :: T.Text,
                                  response_type :: T.Text
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

startTimer :: S.ActionM ()
startTimer = do
  request <- S.param "text"
  token <- S.param "token"
  name <- S.param "user_name"
  time <- liftIO (getCurrentTime)

  if verifyToken token then
        S.json $ makeBotResponse $ "Hi, " <> name
                                    <> "! Time tracking started for *"
                                    <> request <> "*. _Starting_ time is "
                                    <> convertTime time
  else S.text $ "Error"

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.post "/timers/start" $ startTimer

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = do
  port <- lookupEnv "PORT"
  case port of
    Just (value) -> flip S.scotty app' $ read value
    Nothing   -> S.scotty 8080 app'
