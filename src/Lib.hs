{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (withObject)
import Data.String (IsString (fromString))
import Data.Time (UTCTime (utctDayTime), defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), timeToTimeOfDay)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql
import Network.HTTP.Simple
import System.Environment (lookupEnv)
import Text.Blaze (ToMarkup)
import Yesod

newtype Username = Username String
  deriving (Show, Read, Eq, PersistField, PersistFieldSql, PathPiece, ToMarkup)

-- Models
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  username Username
  deriving Show
  UniqueUsername username
FollowerCountEntry
  userId UserId
  count Int
  -- time stamp with timzone
  timestamp UTCTime sqltype=timestamptz
  deriving Show
|]

-- DB functions

defaultConnectionStr :: ConnectionString
defaultConnectionStr = "host=localhost port=5432 user=root password=mysecretpassword dbname=db"

getConnectionStr :: IO ConnectionString
getConnectionStr = do
  mConnectionStr <- lookupEnv "DB_CONNECTION_STRING"
  case mConnectionStr of
    Nothing -> return defaultConnectionStr
    Just "" -> return defaultConnectionStr
    Just s -> return $ fromString s

createDbPool :: IO ConnectionPool
createDbPool = do
  connectionStr <- getConnectionStr
  runStderrLoggingT $ createPostgresqlPool connectionStr 10

insertFollowerCountEntry :: (MonadIO m) => UserId -> Int -> ReaderT SqlBackend m ()
insertFollowerCountEntry userId followerCount = do
  timestamp <- liftIO getCurrentTime
  insert_ $ FollowerCountEntry userId followerCount timestamp

insertUser :: (MonadIO m) => Username -> Int -> ReaderT SqlBackend m (Entity User)
insertUser username followersCount = do
  newUserId <- insert $ User username
  insertFollowerCountEntry newUserId followersCount
  getJustEntity newUserId

-- BlueSky API

getProfile :: Username -> IO (Either JSONException GetProfileResponse)
getProfile (Username username) = do
  request <- parseRequest $ "GET https://public.api.bsky.app/xrpc/app.bsky.actor.getProfile?actor=" <> username
  response <- httpJSONEither request
  return $ getResponseBody response

data GetProfileResponse = GetProfileResponse
  { displayName :: String,
    avatar :: String,
    followersCount :: Int,
    followsCount :: Int,
    postsCount :: Int
  }

instance FromJSON GetProfileResponse where
  parseJSON = withObject "GetProfileResponse" $ \v ->
    GetProfileResponse
      <$> v .: "displayName"
      <*> v .: "avatar"
      <*> v .: "followersCount"
      <*> v .: "followsCount"
      <*> v .: "postsCount"

-- Formatting

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%Y-%m-%d %H:00:00"

isAtMidnight :: UTCTime -> Bool
isAtMidnight time =
  let (TimeOfDay hour _ _) = timeToTimeOfDay (utctDayTime time)
   in hour == 0
