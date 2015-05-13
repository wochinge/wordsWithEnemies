{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.RoundDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Score
import           Types.Round
import           Types.Solution

data RoundDAO = RoundDAO { roundid :: Integer
                         , roundnr :: Integer
                         , gameid  :: Integer
                         , letters :: String
                         }

instance FromRow RoundDAO where
  fromRow = RoundDAO <$> field <*> field <*> field <*> field
  
getRound :: RoundDAO -> Score -> [Solution] -> Round
getRound round winnerScore solutions = Round $ letters round $ winnerScore $ solutions