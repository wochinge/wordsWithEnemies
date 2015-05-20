{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module, which provides database operations for Games.
module DB.GameDAO 
( DB.GameDAO.createTables 
, insertGame
, getGame
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
import qualified DB.RoundDAO as RoundDb
import qualified DB.PlayerDAO as PlayerDb
import           Types.Score
import           Data.Maybe

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
                 
-- | Returns the game searched for by id               
getGame :: DatabaseId                    -- ^ database id of the game
             -> Handler App Sqlite (Maybe G.Game)
getGame gameId = do
    results <- query "SELECT * FROM game WHERE game_id = ? LIMIT 2" (Only (gameId))
    let game = head results
    if null results
        then do
            return Nothing
        else fmap Just $ buildGame game

 -- | Builds one single game out of a the database row.
buildGame :: GameDAO        -- ^ dao which represents a row in the db
           -> Handler App Sqlite G.Game -- ^ normal game object
buildGame dao = do
    player1 <- PlayerDb.getPlayer $ player1id dao
    player2 <- PlayerDb.getPlayer $ player2id dao
    rounds  <- RoundDb.getRounds $ gameid dao
    return $ parseGame dao [fromJust player1, fromJust player2] rounds

-- | Returns a running game of a specific player.
getGameWithPlayer :: DatabaseId                        -- ^ Id of the player
                  -> Handler App Sqlite (Maybe G.Game) -- ^ A game if the players has one, else Nothing
getGameWithPlayer playerId = do
    results <- query "SELECT game_id FROM game WHERE (player1_id = ? OR player2_id = ?) AND status = False" (playerId, playerId)
    case results of
      [] -> return Nothing
      [Only _] -> getGame $ fromOnly $ head results 

-- | Inserts a game into the datebase.                 
insertGame :: G.Game                    -- ^ the game to insert
           -> Handler App Sqlite G.Game -- ^ inserted game including id
insertGame game = do
    let values = (P.playerId $ head $ G.player game, P.playerId $ last $ G.player game, G.status game)
    execute_ "BEGIN"
    execute "INSERT INTO game (player1_id, player2_id, status) VALUES (?, ?, ?)" values
    inserted <- query_ "SELECT * FROM game ORDER BY game_id DESC LIMIT 1"
    let id = gameid $ head inserted
    mapM_ (RoundDb.insertRound id) $ G.rounds game
    execute_ "COMMIT"
    return $ game { G.gameId = Just id }