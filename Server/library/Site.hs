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
import qualified DB.PlayerDAO as P_DAO
import qualified DB.Dictionary as Dict
import qualified DB.GameDAO as G_DAO
import qualified DB.RoundDAO as R_DAO
import qualified DB.ScoreDAO as Sc_DAO
import qualified DB.SolutionDAO as Sol_DAO
import           Control.Lens
import           Control.Concurrent

-- | Initializes the main application by initializing the sub snaplets.
-- | It also establishes the db connection.
initApplication :: SnapletInit App App -- ^ Snaplet initializer
initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    player <- nestSnaplet "player" playerSnaplet $ apiInit
    playerDAO <- nestSnaplet "playerDAO" playerDb sqliteInit
    dictionary <- nestSnaplet "dictionary" dictionary sqliteInit
    gameDAO <- nestSnaplet "gameDAO" gameDb sqliteInit
    roundDAO <- nestSnaplet "roundDAO" roundDb sqliteInit
    solutionDAO <- nestSnaplet "solutionDAO" solutionDb sqliteInit
    scoreDAO <- nestSnaplet "scoreDAO" scoreDb sqliteInit
    
    let c = sqliteConn $ playerDAO ^# snapletValue
    liftIO $ withMVar c $ \conn -> P_DAO.createTables conn
    liftIO $ withMVar c $ \conn -> Dict.createTables conn
    liftIO $ withMVar c $ \conn -> G_DAO.createTables conn
    liftIO $ withMVar c $ \conn -> R_DAO.createTables conn
    liftIO $ withMVar c $ \conn -> Sc_DAO.createTables conn
    liftIO $ withMVar c $ \conn -> Sol_DAO.createTables conn
    return $ App player playerDAO dictionary gameDAO roundDAO scoreDAO solutionDAO