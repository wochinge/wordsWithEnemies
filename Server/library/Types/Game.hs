{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types.Game where

import Data.Aeson
import Data.Aeson.TH
import Database.SQLite.Simple
import Control.Applicative
import Types.Player
import Types.Score
import Types.Round

data Game = Game { gameId :: Maybe Integer -- ID kann auch leer sein
                 , player :: Player
                 , status :: Boolean
                 , totalScores :: [Score]
                 , rounds :: [Round]
                 } deriving (Show, Eq)


instance FromRow Game where
  fromRow = Game <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Game)