{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module, which provides database operations for rounds.
module DB.RoundDb 
( DB.RoundDb.createTables
, getRounds
, insertRound
) where

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

-- | Creates the round table.
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
-- | Returns all the rounds of a game.
getRounds :: DatabaseId                 -- ^ database id of the game
          -> Handler App Sqlite [Round] -- ^ rounds of the game
getRounds gameId = do
    results <- query "SELECT * FROM round WHERE game_id = ?" (Only (gameId))
    mapM buildRound results

-- | Builds one single round out of a the database row.
buildRound :: RoundDAO.RoundDAO        -- ^ dao which represents a row in the db
           -> Handler App Sqlite Round -- ^ normal round object
buildRound dao = do
    score <- getScore $ RoundDAO.roundid dao
    solutions <- getSolutions $ RoundDAO.roundid dao
    return $ RoundDAO.getRound dao score solutions

-- | Inserts a round in the database.
insertRound :: DatabaseId            -- ^ database id of the game of the round
            -> Round                 -- ^ round to insert
            -> Handler App Sqlite () -- ^ nothing
insertRound gameId newRound = do
    let values = (gameId, gameId, letters newRound)
    execute "INSERT INTO round (round_nr, game_id, letters) VALUES ((SELECT IFNULL(MAX(round_r), 0) + 1 FROM round WHERE game_id = ?), ?, ?)" values
    inserted <- query "SELECT * FROM round WHERE round_nr = (SELECT MAX(round_r) FROM round where game_id = ?)" (Only (gameId))
    let roundId = RoundDAO.roundid $ head inserted
    mapM_ (\s -> insertSolution roundId s) $ solutions newRound
    let score = roundScore newRound
    when (isJust score) $ insertScore roundId (fromJust score)