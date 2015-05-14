{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.RoundDb where

import qualified Database.SQLite.Simple as SQL
import           Data.Maybe
import qualified DBAccess.RoundDAO as RoundDAO
import           Types.Round
import           DB.ScoreDb
import           DB.SolutionDb
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils
import           Control.Monad

createTables :: SQL.Connection -- ^ database connection
             -> IO ()        -- ^ nothing
createTables conn = do
    createTable conn "round" $
        T.concat [ "CREATE TABLE round ("
                 , "round_id INTEGER PRIMARY KEY, "
                 , "round_nr INTEGER, "
                 , "game_id INTEGER NOT NULL, "
                 , "letters TEXT NOT NULL, "
                 , "FOREIGN KEY(game_id) REFERENCES game(game_id))"
                 ]             

getRounds :: Integer -> Handler App Sqlite [Round]
getRounds gameId = do
    results <- query "SELECT * FROM round WHERE game_id = ?" (Only (gameId))
    mapM buildRound results

buildRound :: RoundDAO.RoundDAO -> Handler App Sqlite Round
buildRound dao = do
    score <- getScore $ RoundDAO.roundid dao
    solutions <- getSolutions $ RoundDAO.roundid dao
    return $ RoundDAO.getRound dao score solutions

insertRound :: Integer -> Round -> Handler App Sqlite ()
insertRound gameId newRound = do
    let values = (gameId, gameId, letters newRound)
    execute "INSERT INTO round (round_nr, game_id, letters) VALUES ((SELECT IFNULL(MAX(round_r), 0) + 1 FROM round WHERE game_id = ?), ?, ?)" values
    inserted <- query "SELECT * FROM round WHERE round_nr = (SELECT MAX(round_r) FROM round where game_id = ?)" (Only (gameId))
    let roundId = RoundDAO.roundid $ head inserted
    mapM_ (\s -> insertSolution roundId s) $ solutions newRound
    let score = roundScore newRound
    when (isJust score) $ insertScore roundId (fromJust score)