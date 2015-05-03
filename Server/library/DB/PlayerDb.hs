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

createTables :: S.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "player"
  unless schemaCreated $
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE player ("
                , "player_id INTEGER PRIMARY KEY, "
                , "nickname TEXT NOT NULL)"])

                
savePlayer :: Player -> Handler App Sqlite [Player]
savePlayer (Player _ name) = do
     execute "INSERT INTO player (nickname) VALUES (?)" (Only (name))
     result <- query "SELECT * FROM player WHERE nickname = ? AND player_id = (SELECT max(player_id) FROM player WHERE nickname = ?)" (name, name)
     return result
