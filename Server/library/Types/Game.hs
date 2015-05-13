{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Game where

import 			 Data.Aeson
import 			 Data.Aeson.TH
import 			 Database.SQLite.Simple
import 			 Control.Applicative
import 			 Types.Player
import 			 Types.Score
import 			 Types.Round

data Game = Game { gameId :: Maybe Integer -- ID kann auch leer sein
                 , player :: [Player]
                 , status :: Bool
                 , totalScores :: Maybe [Score]
                 , rounds :: Maybe [Round]
                 } deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Game)