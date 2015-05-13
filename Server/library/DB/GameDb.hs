{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.GameDb (createTables) where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as SQL
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Application
import           DB.Utils
import qualified Data.List as L
import qualified DB.PlayerDb as PlayerDb
import           DBAccess.SolutionDAO
import qualified DBAccess.ScoreDAO as ScoreDAO
import qualified DBAccess.RoundDAO as RoundDAO
import qualified Types.Solution as S
import           Types.Score
import           Types.Round
import           Types.Game
import qualified Types.Player as P

-- | Creates the Game table.
createTables :: SQL.Connection -- ^ database connection
             -> IO ()        -- ^ nothing
createTables conn = do
    createTable conn "game" $
        T.concat [ "CREATE TABLE game ("
                 , "game_id INTEGER PRIMARY KEY, "
                 , "player1_id INTEGER, "
                 , "player2_id INTEGER, "
                 , "status BOOLEAN, "
                 , "FOREIGN KEY(player1_id) REFERENCES player(player_id), "
                 , "FOREIGN KEY(player2_id) REFERENCES player(player_id))"
                 ]
    
    createTable conn "roundscore" $
        T.concat [ "CREATE TABLE roundscore ("
                 , "score_id INTEGER PRIMARY KEY, "
                 , "round_id INTEGER, "
                 , "winner INTEGER, "
                 , "score INTEGER, "
                 , "FOREIGN KEY(winner) REFERENCES player(player_id), "
                 , "FOREIGN KEY(round_id) REFERENCES round(round_id))"
                 ]

    createTable conn "totalscore" $
        T.concat [ "CREATE TABLE totalscore ("
                 , "score_id INTEGER PRIMARY KEY, "
                 , "game_id INTEGER"
                 , "score INTEGER, "
                 , "player_id INTEGER NOT NULL, "
                 , "FOREIGN KEY(player_id) REFERENCES player(player_id), "
                 , "FOREIGN KEY(game_id) REFERENCES game(game_id))"
                 ]
  
    createTable conn "solution" $
        T.concat [ "CREATE TABLE solution ("
                 , "solution_id INTEGER PRIMARY KEY, "
                 , "solution TEXT ," 
                 , "player_id INTEGER NOT NULL, "
                 , "round_id INTEGER NOT NULL, "
                 , "FOREIGN KEY(round_id) REFERENCES round(round_id), "
                 , "FOREIGN KEY(player_id) REFERENCES player(player_id))"
                 ]
    -- INSERT INTO Log (id, rev_no, description
    -- VALUES ((SELECT IFNULL(MAX(id), 0) + 1 FROM Log), 'rev_Id', 'some description')          
    createTable conn "round" $
        T.concat [ "CREATE TABLE round ("
                 , "round_id INTEGER PRIMARY KEY, "
                 , "round_nr INTEGER, "
                 , "game_id INTEGER NOT NULL, "
                 , "letters TEXT NOT NULL, "
                 , "FOREIGN KEY(game_id) REFERENCES game(game_id))"
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

getScore :: Integer -> Handler App Sqlite (Maybe Score)
getScore roundId = do
    results <- query "SELECT * FROM score WHERE round_id = ? LIMIT 1" (Only (roundId))
    let score = head results
    if null results
        then do
            player <- PlayerDb.getPlayer $ ScoreDAO.winnerid score
            return $ Just $ ScoreDAO.getScore score player
        else return Nothing

getRounds :: Integer -> Handler App Sqlite [Round]
getRounds gameId = do
    results <- query "SELECT * FROM round WHERE game_id = ?" (Only (gameId))
    mapM buildRound results

buildRound :: RoundDAO.RoundDAO -> Handler App Sqlite Round
buildRound dao = do
    score <- getScore $ RoundDAO.roundid dao
    solutions <- getSolutions $ RoundDAO.roundid dao
    return $ RoundDAO.getRound dao score solutions