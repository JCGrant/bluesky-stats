{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Sql
import Lib

updateUsers :: ReaderT SqlBackend (ResourceT IO) ()
updateUsers = do
  users <- selectList [] []
  forM_ users updateFollowerCount

updateFollowerCount :: Entity User -> ReaderT SqlBackend (ResourceT IO) ()
updateFollowerCount (Entity userId user) = do
  let Username username = userUsername user
  liftIO $ putStrLn $ "Updating follower count for " <> username
  eProfile <- liftIO $ getProfile $ userUsername user
  case eProfile of
    Left _ -> return ()
    Right GetProfileResponse {followersCount} -> insertFollowerCountEntry userId followersCount

main :: IO ()
main = do
  pool <- createDbPool
  runResourceT $ flip runSqlPool pool $ do
    updateUsers
    return ()
