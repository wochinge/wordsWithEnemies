{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.GameDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import qualified Types.Game as G
import           Types.Score
import           Types.Round
import           Types.Player

data GameDAO = GameDAO { gameid    :: Integer
                       , player1id :: Integer
                       , player2id :: Integer
                       , status    :: Bool
                       }

instance FromRow GameDAO where
  fromRow = GameDAO <$> field <*> field <*> field <*> field
  
getScore :: GameDAO -> [Player] -> [Score] -> [Round] -> G.Game
getScore game players scores rounds = G.Game (Just $ gameid game) players (status game) rounds