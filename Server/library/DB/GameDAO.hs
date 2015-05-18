{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module, which provides database operations for Games.
module DB.GameDAO 
( DB.GameDAO.createTables 
, insertGame
) where

import 			 Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as SQL
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Application
import           DB.Utils
import           Types.Round
import qualified Types.Game as G
import qualified Types.Player as P
import           DB.RoundDAO
import           Types.Score

-- | Represents one row of the table game.
data GameDAO = GameDAO { gameid    :: DatabaseId
                       , player1id :: DatabaseId
                       , player2id :: DatabaseId
                       , status    :: Bool
                       }

instance FromRow GameDAO where
  fromRow = GameDAO <$> field <*> field <*> field <*> field
  
-- | Parses one game row to a nice game model.
parseGame :: GameDAO    -- ^ database row
          -> [P.Player] -- ^ players of the game
          -> [Round]    -- ^ rounds of a game
          -> G.Game     -- ^ pretty game object
parseGame game players rounds = G.Game (Just $ gameid game) players (status game) rounds

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
insertGame :: G.Game                    -- ^ the game to insert
           -> Handler App Sqlite G.Game -- ^ inserted game including id
insertGame game = do
    let values = (P.playerId $ head $ G.player game, P.playerId $ last $ G.player game, G.status game)
    execute_ "BEGIN"
    execute "INSERT INTO game (player1_id, player2_id, status) VALUES (?, ?, ?)" values
    inserted <- query_ "SELECT * FROM round WHERE game_id = MAX(game_id)"
    let id = gameid $ head inserted
    mapM_ (insertRound id) $ G.rounds game
    execute_ "COMMIT"
    return $ game { G.gameId = Just id }