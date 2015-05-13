{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.ScoreDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Player
import           Types.Score

data ScoreDAO = ScoreDAO { scoreid :: Integer
                         , roundid :: Integer
                         , winnerid :: Integer
                         , roundScore :: Integer
                         }

instance FromRow ScoreDAO where
  fromRow = ScoreDAO <$> field <*> field <*> field <*> field
  
getScore :: ScoreDAO -> Player -> Score
getScore score player = Score $ roundScore score $ player