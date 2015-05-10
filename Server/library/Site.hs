{-# LANGUAGE OverloadedStrings #-}

module Site where

import           Application
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Control.Monad.Trans (liftIO)
import           Api.PlayerSite
import qualified DB.PlayerDb as Db
import qualified DB.Dictionary as Dict
import           Control.Lens
import           Control.Concurrent

initApplication :: SnapletInit App App
initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    player <- nestSnaplet "player" playerSnaplet $ apiInit
    playerDb <- nestSnaplet "playerDb" playerDb sqliteInit
    dictionary <- nestSnaplet "dictionary" dictionary sqliteInit
    
    let c = sqliteConn $ playerDb ^# snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn
    liftIO $ withMVar c $ \conn -> Dict.createTables conn
    return $ App player playerDb dictionary