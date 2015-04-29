{-# LANGUAGE OverloadedStrings #-}
module Site where

import Application
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple
import Control.Monad.Trans (liftIO)
import Api.PlayerSite
import qualified DB.PlayerDb as Db
import Control.Lens
import Control.Concurrent

initApplication :: SnapletInit App App
initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    user <- nestSnaplet "user" userSnaplet $ apiInit
    database <- nestSnaplet "db" db sqliteInit
    
    let c = sqliteConn $ database ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    return $ App user database