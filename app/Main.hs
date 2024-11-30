{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (runResourceT)
import Data.String (IsString (fromString))
import Database.Persist.Sql
import Lib
import Network.Wai.EventSource (ServerEvent (ServerEvent), eventSourceAppIO)
import Yesod

data App = App
  { appConnectionPool :: ConnectionPool
  }

mkYesod
  "App"
  [parseRoutes|
/user/#Username UserR GET
/user/#Username/live LiveR GET
|]

getUserR :: Username -> Handler Html
getUserR username = do
  mUser <- runDB $ do
    mUserFromDB <- getBy $ UniqueUsername username
    case mUserFromDB of
      Nothing -> insertUserFromBlueSky username
      Just user -> return $ Just user
  case mUser of
    Nothing -> notFound
    Just user -> do
      followerCountEntries <- runDB $ selectList [FollowerCountEntryUserId ==. entityKey user] [Desc FollowerCountEntryTimestamp]
      defaultLayout $ do
        setTitle $ toHtml username <> "'s profile"
        [whamlet|
          <h1>#{username}
          <p>Live follower count: <span id="live-follower-count">Fetching...</span>
          <table>
            <tr>
              <th>Count
              <th>Timestamp
            $forall Entity _ (FollowerCountEntry _ count timestamp) <- followerCountEntries
              <tr>
                <td>#{count}
                <td>#{formatUTCTime timestamp}
        |]
        toWidgetBody
          [julius|
            var eventSource = new EventSource("@{LiveR username}");
            eventSource.onmessage = function(event) {
              document.getElementById("live-follower-count").innerText = event.data;
            };
            eventSource.onerror = function(event) {
              console.error(event);
            };
          |]

getLiveR :: Username -> Handler ()
getLiveR username = do
  let generateEvent = do
        threadDelay $ 1000000 * 5 -- 5 seconds
        mProfile <- liftIO $ getProfile username
        case mProfile of
          Left e -> return $ ServerEvent (Just "error") Nothing [fromString $ show e]
          Right GetProfileResponse {followersCount} -> do
            return $ ServerEvent (Just "message") Nothing [fromString $ show followersCount]
  sendWaiApplication $ eventSourceAppIO generateEvent

instance Yesod App where
  makeSessionBackend _ = return Nothing -- no session needed

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    app <- getYesod
    runSqlPool action (appConnectionPool app)

main :: IO ()
main = do
  pool <- createDbPool
  runResourceT $ flip runSqlPool pool $ do
    runMigration migrateAll
  warp 3000 $ App pool
