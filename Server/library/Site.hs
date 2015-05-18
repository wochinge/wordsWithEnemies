{-# LANGUAGE OverloadedStrings #-}

-- | Module, which nests all the used sub snaplets, defined bei the module Application,
-- | and initializes them.
module Site (initApplication) where

import           Application
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Control.Monad.Trans (liftIO)
import           Api.PlayerSite
import qualified DB.PlayerDb as P_Db
import qualified DB.Dictionary as Dict
import qualified DB.GameDb as G_Db
import qualified DB.RoundDb as R_Db
import qualified DB.ScoreDb as Sc_Db
import qualified DB.SolutionDb as Sol_Db
import           Control.Lens
import           Control.Concurrent

-- | Initializes the main application by initializing the sub snaplets.
-- | It also establishes the db connection.
initApplication :: SnapletInit App App -- ^ Snaplet initializer
initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    player <- nestSnaplet "player" playerSnaplet $ apiInit
    playerDb <- nestSnaplet "playerDb" playerDb sqliteInit
    dictionary <- nestSnaplet "dictionary" dictionary sqliteInit
    gameDb <- nestSnaplet "gameDb" gameDb sqliteInit
    roundDb <- nestSnaplet "roundDb" roundDb sqliteInit
    solutionDb <- nestSnaplet "solutionDb" solutionDb sqliteInit
    scoreDb <- nestSnaplet "scoreDb" scoreDb sqliteInit
    
    let c = sqliteConn $ playerDb ^# snapletValue
    liftIO $ withMVar c $ \conn -> P_Db.createTables conn
    liftIO $ withMVar c $ \conn -> Dict.createTables conn
    liftIO $ withMVar c $ \conn -> G_Db.createTables conn
    liftIO $ withMVar c $ \conn -> R_Db.createTables conn
    liftIO $ withMVar c $ \conn -> Sc_Db.createTables conn
    liftIO $ withMVar c $ \conn -> Sol_Db.createTables conn
    return $ App player playerDb dictionary gameDb roundDb scoreDb solutionDb