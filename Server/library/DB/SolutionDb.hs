{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.SolutionDb where

import qualified Database.SQLite.Simple as SQL
import           DBAccess.SolutionDAO
import qualified Types.Solution as S
import qualified Types.Player as P
import qualified DB.PlayerDb as PlayerDb
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils

createTables :: SQL.Connection -- ^ database connection
             -> IO ()        -- ^ nothing
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
                 
getSolutions :: Integer -> Handler App Sqlite [S.Solution]
getSolutions roundId = do
    results <- query "SELECT * FROM solution WHERE round_id = ? LIMIT 2" (Only (roundId))
    player <- mapM (\dao -> PlayerDb.getPlayer $ playerOfSolution dao) results
    return $ zipWith getSolution results player

insertSolution :: Integer -> S.Solution -> Handler App Sqlite ()
insertSolution roundId newSolution = do
    let values = (S.solution newSolution, P.playerId $ S.player newSolution, roundId)
    execute "INSERT INTO solution (solution, player_id, round_id) VALUES (?)" values