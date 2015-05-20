{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for database operations for Score objects.
module DB.ScoreDAO 
( createTables
, getScore
, insertScore
) where

import qualified Database.SQLite.Simple as SQL
import qualified Types.Player as P
import qualified DB.PlayerDAO as PlayerDb
import qualified Types.Score as S
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet
import           Application
import qualified Data.Text as T
import           DB.Utils
import 			 Control.Applicative
import           Data.Maybe

-- | Represents one row of the table score.
data ScoreDAO = ScoreDAO { scoreid :: DatabaseId
                         , roundid :: DatabaseId
                         , winnerid :: DatabaseId
                         , roundScore :: Integer
                         }

instance FromRow ScoreDAO where
  fromRow = ScoreDAO <$> field <*> field <*> field <*> field

-- | Parses a score row to a nice score object.
parseScore :: ScoreDAO -- ^ score database row
           -> P.Player -- ^ player of the score
           -> S.Score  -- ^ score object
parseScore score player = S.Score (Just $ scoreid score) (roundScore score) player

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
         -> Handler App Sqlite (Maybe S.Score) -- ^ Score of the round or nothing
getScore roundId = do
    results <- query "SELECT * FROM roundscore WHERE round_id = ? LIMIT 1" (Only (roundId))
    let score = head results
    if null results
        then
           return Nothing
        else do  
            player <- PlayerDb.getPlayer $ winnerid score
            return $ Just $ parseScore score $ fromJust player

-- | Inserts a score into the database.
insertScore :: DatabaseId            -- ^ database id of the round
            -> S.Score                 -- ^ Score to insert
            -> Handler App Sqlite () -- ^ Nothing
insertScore roundId newScore = do
    let values = (roundId, P.playerId $ S.player newScore, S.score newScore)
    execute "INSERT INTO roundscore (round_id, winner, score) VALUES (?, ?, ?)" values