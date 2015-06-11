{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for database operations for solution objects.
module DB.SolutionDAO
( createTables
, getSolutions
, insertSolution
) where

import qualified Database.SQLite.Simple as SQL
import qualified Types.Player as P
import qualified DB.PlayerDAO as PlayerDb
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils
import 			 Control.Applicative
import qualified Types.Solution as S
import           Data.Maybe

-- | Represents one row of the table solution.
data SolutionDAO = SolutionDAO { solutionId :: DatabaseId
                               , solutionText :: String
                               , playerOfSolution :: DatabaseId
                               , roundid :: DatabaseId
                               }

instance FromRow SolutionDAO where
  fromRow = SolutionDAO <$> field <*> field <*> field <*> field

-- | Parses one database row to a nice solution model.  
parseSolution :: SolutionDAO -- ^ database row
              -> P.Player    -- ^ player of the solution
              -> S.Solution  -- ^ pretty solution object
parseSolution solution = S.Solution (Just $ solutionId solution) (solutionText solution)

-- | Create solution table.
createTables :: SQL.Connection -- ^ database connection
             -> IO ()          -- ^ nothing
createTables conn =
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
getSolutions :: DatabaseId                      -- ^ database id of the round
             -> Handler App Sqlite [S.Solution] -- ^ 0 to 2 solutions
getSolutions roundId = do
    results <- query "SELECT * FROM solution WHERE round_id = ? LIMIT 2" (Only roundId)
    player <- mapM (PlayerDb.getPlayer . playerOfSolution) results
    return $ zipWith parseSolution results $ map fromJust player

-- | Inserts a solution in the database.
insertSolution :: DatabaseId            -- ^ database id of the round
               -> S.Solution            -- ^ Solution to insert
               -> Handler App Sqlite () -- ^ nothing
insertSolution roundId newSolution = do
    let values = (S.solution newSolution, P.playerId $ S.player newSolution, roundId)
    execute "INSERT INTO solution (solution, player_id, round_id) VALUES (?, ?, ?)" values