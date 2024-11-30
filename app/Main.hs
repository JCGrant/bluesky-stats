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
import Text.Julius (RawJS (rawJS))
import Yesod

runIntervalSeconds :: Int
runIntervalSeconds = 5

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
        setTitle $ "Bluesky Stats | " <> toHtml username <> "'s follower count"
        toWidgetHead
          [hamlet|
          <script src="https://cdn.tailwindcss.com"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js" integrity="sha512-CQBWl4fJHWbryGE+Pc7UAxWMUMNMWzWxF4SQo9CgkJIN1kx6djDQZjh3Y8SZ1d+6I+1zze6Z7kHXO7q3UyZAWw==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>
        |]
        toWidgetHead
          [cassius|
            body
              background-color: #020617
              color: #cbd5e1
          |]
        [whamlet|
          <div class="flex flex-col gap-4 p-4">
            <h1 class="text-3xl font-bold">Bluesky Stats
            <h2 class="text-2xl font-bold">#{username}'s follower count
            <p>Live follower count: <span id="live-follower-count">Fetching...</span>
            <div class="w-full h-[320px]">
              <canvas id="live-follower-chart">
            <h3 class="text-xl font-bold">Follower count history
            <table class="w-full text-left table-auto">
              <thead class="uppercase bg-slate-800">
                <tr>
                  <th scope="col" class="px-6 py-3">Time
                  <th scope="col" class="px-6 py-3">Count
              <tbody class="bg-slate-900">
                $forall Entity _ (FollowerCountEntry _ count timestamp) <- followerCountEntries
                  <tr class="border-t border-slate-800">
                    <td class="px-6 py-4">#{formatUTCTime timestamp}
                    <td class="px-6 py-4">#{count}
        |]
        toWidgetBody
          [julius|
            const liveFollowerChart = new Chart(
              document.getElementById("live-follower-chart"),
              {
                type: 'line',
                options: {
                  responsive: true,
                  maintainAspectRatio: false,
                },
                data: {
                  labels: [],
                  datasets: [
                    {
                      label: 'Live Follower Count',
                      data: [],
                    }
                  ]
                }
              }
            );

            let seconds = 0;

            const eventSource = new EventSource("@{LiveR username}");
            eventSource.onmessage = function(event) {
              const count = parseInt(event.data);
              // Update Text
              document.getElementById("live-follower-count").innerText = count;
              // Update Chart
              liveFollowerChart.data.labels.push(seconds);
              seconds += #{rawJS (show runIntervalSeconds)};
              liveFollowerChart.data.datasets[0].data.push(count);
              liveFollowerChart.update();
            };
            eventSource.onerror = function(event) {
              console.error(event);
            };
          |]

getLiveR :: Username -> Handler ()
getLiveR username = do
  let generateEvent = do
        threadDelay $ 1000000 * runIntervalSeconds -- 5 seconds
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
