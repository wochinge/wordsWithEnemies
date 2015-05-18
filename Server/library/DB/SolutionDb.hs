{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for database operations for solution objects.
module DB.SolutionDb 
( createTables
, getSolutions
, insertSolution
) where

import qualified Database.SQLite.Simple as SQL
import           DBAccess.SolutionDAO
import           Types.Solution
import qualified Types.Player as P
import qualified DB.PlayerDb as PlayerDb
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils

-- | Create solution table.
createTables :: SQL.Connection -- ^ database connection
             -> IO ()          -- ^ nothing
createTables conn = do
    createTable conn "solution" $
        T.concat [ "CREATE TABLE solution ("
                 , "solution_id INTEGER PRIMARY KEY, "
                 , "solution TEXT ," 
                 , "player_id INTEGER NOT NULL, "
                 , "round_id INTEGER NOT NULL, "
                 , "FOREIGN KEY(round_id) REFERENCES round(round_id), "
                 , "FOREIGN KEY(player_id) REFERENCES player(player_id))"
                 ]

-- | Returns solutions of round.                 
getSolutions :: DatabaseId                    -- ^ database id of the round
             -> Handler App Sqlite [Solution] -- ^ 0 to 2 solutions
getSolutions roundId = do
    results <- query "SELECT * FROM solution WHERE round_id = ? LIMIT 2" (Only (roundId))
    player <- mapM (\dao -> PlayerDb.getPlayer $ playerOfSolution dao) results
    return $ zipWith getSolution results player

-- | Inserts a solution in the database.
insertSolution :: DatabaseId            -- ^ database id of the round
               -> Solution              -- ^ Solution to insert
               -> Handler App Sqlite () -- ^ nothing
insertSolution roundId newSolution = do
    let values = (solution newSolution, P.playerId $ player newSolution, roundId)
    execute "INSERT INTO solution (solution, player_id, round_id) VALUES (?)" values