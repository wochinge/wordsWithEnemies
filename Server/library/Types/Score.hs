{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types.Score where

import Data.Aeson
import Data.Aeson.TH
import Database.SQLite.Simple
import Control.Applicative
import Types.Player

data Score = Score { score :: Integer
                   , player :: Player 
                   } deriving (Show, Eq)


instance FromRow Score where
  fromRow = Score <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Score)