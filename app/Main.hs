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

import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sql
import Lib
import Yesod

data App = App
  { appConnectionPool :: ConnectionPool
  }

mkYesod
  "App"
  [parseRoutes|
/user/#Username UserR GET
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
        [whamlet|
          <p>#{username}
          <table>
            <tr>
              <th>Count
              <th>Timestamp
            $forall Entity _ (FollowerCountEntry _ count timestamp) <- followerCountEntries
              <tr>
                <td>#{count}
                <td>#{formatUTCTime timestamp}
        |]

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
