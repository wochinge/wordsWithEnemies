{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.GameDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Game
import           Types.Score
import           Types.Round
import           Types.Player

data GameDAO = GameDAO { gameid    :: Integer
                       , player1id :: Integer
                       , player2id :: Integer
                       , status    :: Boolean
                       }

instance FromRow GameDAO where
  fromRow = GameDAO <$> field <*> field <*> field <*> field
  
getScore :: GameDAO -> [Player] -> [Score] -> [Round] -> Game
getScore game players scores rounds = Game (gameid game) players (status game) rounds