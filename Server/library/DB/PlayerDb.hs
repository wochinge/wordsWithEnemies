{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.PlayerDb where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Types.Player
import           DB.Utils
import           Application
import           Data.Maybe

createTables :: S.Connection -> IO ()
createTables conn = do
  tablePlayerCreated <- tableExists conn "player"
  unless tablePlayerCreated $
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE player ("
                , "player_id INTEGER PRIMARY KEY, "
                , "nickname TEXT NOT NULL)"])
  tableQueueCreated <- tableExists conn "queue"
  unless tableQueueCreated $
   S.execute_ conn
     (S.Query $
      T.concat [ "CREATE TABLE queue ("
               , "id INTEGER PRIMARY KEY, "
               , "waiting_player INTEGER, "
               , "FOREIGN KEY(waiting_player) REFERENCES player(player_id))"])
           
savePlayer :: Player -> Handler App Sqlite Player
savePlayer (Player _ name) = do
     execute "INSERT INTO player (nickname) VALUES (?)" (Only (name))
     result <- query "SELECT * FROM player WHERE nickname = ? AND player_id = (SELECT max(player_id) FROM player WHERE nickname = ?)" (name, name)
     let savedPlayer = head result
     insertWaitingPlayer $ fromJust $ playerId savedPlayer
     return savedPlayer

getPlayer :: Integer -> Handler App Sqlite Player
getPlayer id = do
     result <- query "SELECT * FROM player WHERE player_id = ?" (Only (id))
     return $ head result

insertWaitingPlayer :: Integer -> Handler App Sqlite ()
insertWaitingPlayer player_id = 
     execute "INSERT INTO queue (waiting_player) VALUES (?)" (Only (player_id))

getTwoWaitingPlayers :: Handler App Sqlite [Player]
getTwoWaitingPlayers = do
     result <- query_ "SELECT waiting_player FROM queue LIMIT 2"
     mapM getPlayer $ extracted result
     where 
        extracted listOfOnlys= map fromOnly listOfOnlys
