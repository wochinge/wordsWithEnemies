{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for database operations for Score objects.
module DB.ScoreDb 
( createTables
, getScore
, insertScore
) where

import qualified Database.SQLite.Simple as SQL
import qualified Types.Player as P
import qualified DB.PlayerDb as PlayerDb
import           Types.Score
import qualified DBAccess.ScoreDAO as DAO
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils

-- | Creates score table.
createTables :: SQL.Connection -- ^ database connection
             -> IO ()          -- ^ nothing
createTables conn = do
    createTable conn "roundscore" $
        T.concat [ "CREATE TABLE roundscore ("
                 , "score_id INTEGER PRIMARY KEY, "
                 , "round_id INTEGER, "
                 , "winner INTEGER, "
                 , "score INTEGER, "
                 , "FOREIGN KEY(winner) REFERENCES player(player_id), "
                 , "FOREIGN KEY(round_id) REFERENCES round(round_id))"
                 ]

-- | Returns score of a round out of the database.                 
getScore :: DatabaseId                       -- ^ id of the round
         -> Handler App Sqlite (Maybe Score) -- ^ Score of the round or nothing
getScore roundId = do
    results <- query "SELECT * FROM score WHERE round_id = ? LIMIT 1" (Only (roundId))
    let score = head results
    if null results
        then do
            player <- PlayerDb.getPlayer $ DAO.winnerid score
            return $ Just $ DAO.getScore score player
        else return Nothing

-- | Inserts a score into the database.
insertScore :: DatabaseId            -- ^ database id of the round
            -> Score                 -- ^ Score to insert
            -> Handler App Sqlite () -- ^ Nothing
insertScore roundId newScore = do
    let values = (roundId, P.playerId $ player newScore, score newScore)
    execute "INSERT INTO roundscore (round_id, winner, score) VALUES (?, ?, ?)" values