{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.GameDb (createTables, wordExists, getRandomWord) where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Application
import           DB.Utils
import qualified Data.List as L

-- | Creates the Game table.
createTables :: S.Connection -- ^ database connection
             -> IO ()        -- ^ nothing
createTables conn = do
    S.execute_ conn "BEGIN"
    tableGameCreated <- tableExists conn "game"
    unless schemaCreated $
      S.execute_ conn
        (S.Query $
         T.concat [ "CREATE TABLE game ("
                  , "game_id INTEGER PRIMARY KEY, "
                  , "player1_id INTEGER, "
                  , "player2_id INTEGER, "
                  , "status BOOLEAN, "
                  , "FOREIGN KEY(player1_id) REFERENCES player(player_id), "
                  , "FOREIGN KEY(player2_id) REFERENCES player(player_id))"
                  ])
                
    tableTotalscoreCreated <- tableExists conn "totalscore"
    unless schemaCreated $
      S.execute_ conn
        (S.Query $
         T.concat [ "CREATE TABLE totalscore ("
                  , "score_id INTEGER PRIMARY KEY, "
                  , "score INTEGER, "
                  , "player_id INTEGER NOT NULL, "
                  , "FOREIGN KEY(player_id) REFERENCES player(player_id))"
                  ])
  
    tableSolutionCreated <- tableExists conn "solution"
    unless schemaCreated $
      S.execute_ conn
        (S.Query $
         T.concat [ "CREATE TABLE solution ("
                  , "solution TEXT, "
                  , "solution_id INTEGER PRIMARY KEY, "
                  , "player_id INTEGER NOT NULL, "
                  , "round_id INTEGER NOT NULL, "
                  , "FOREIGN KEY(round_id) REFERENCES round(round_id), "
                  , "FOREIGN KEY(player_id) REFERENCES player(player_id))"
                ])            
                
    tableRoundCreated <- tableExists conn "round"
    unless schemaCreated $
      S.execute_ conn
        (S.Query $
         T.concat [ "CREATE TABLE round ("
                  , "round_id INTEGER PRIMARY KEY, "
                  , "round_nr INTEGER AUTOINCREMENT, "
                  , "game_id INTEGER NOT NULL, "
                  , "letters TEXT NOT NULL, "
                  , "FOREIGN KEY(game_id) REFERENCES game(game_id))"
                  ])               
    S.execute_ conn "COMMIT"