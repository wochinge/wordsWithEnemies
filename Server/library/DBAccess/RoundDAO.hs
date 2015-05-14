{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.RoundDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Score
import qualified Types.Round as R
import           Types.Solution

data RoundDAO = RoundDAO { roundid :: Integer
                         , roundnr :: Integer
                         , gameid  :: Integer
                         , letters :: String
                         }

instance FromRow RoundDAO where
  fromRow = RoundDAO <$> field <*> field <*> field <*> field
  
getRound :: RoundDAO -> Maybe Score -> [Solution] -> R.Round
getRound dao winnerScore solutions = R.Round (Just $ roundid dao) (Just $ roundnr dao) (letters dao) winnerScore solutions