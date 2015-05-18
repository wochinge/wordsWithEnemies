{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module, which provides database operations for Games.
module DB.GameDb 
( DB.GameDb.createTables 
, insertGame
) where

import           Control.Monad
import qualified Database.SQLite.Simple as SQL
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Application
import           DB.Utils
import           Types.Round
import           Types.Game
import qualified DBAccess.GameDAO as GameDAO
import qualified Types.Player as P
import           DB.RoundDb

-- | Creates the Game table.
createTables :: SQL.Connection -- ^ database connection
             -> IO ()          -- ^ nothing
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

-- | Inserts a game into the datebase.                 
insertGame :: Game                    -- ^ the game to insert
           -> Handler App Sqlite Game -- ^ inserted game including id
insertGame game = do
    let values = (P.playerId $ head $ player game, P.playerId $ last $ player game, status game)
    execute_ "BEGIN"
    execute "INSERT INTO game (player1_id, player2_id, status) VALUES (?, ?, ?)" values
    inserted <- query_ "SELECT * FROM round WHERE game_id = MAX(game_id)"
    let id = GameDAO.gameid $ head inserted
    mapM_ (insertRound id) $ rounds game
    execute_ "COMMIT"
    return $ game { gameId = Just id }