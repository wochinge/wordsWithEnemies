{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Score where

import 			 Data.Aeson
import 			 Data.Aeson.TH
import 			 Database.SQLite.Simple
import 			 Control.Applicative
import 			 Types.Player

data Score = Score { scoreId :: Maybe Integer
                   , score :: Integer
                   , player :: Player 
                   } deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Score)