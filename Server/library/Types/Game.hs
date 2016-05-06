{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model game.
module Types.Game where

import        Data.Aeson.TH
import        Types.Player
import        Types.Round

-- | Data type to represent a game.
data Game = Game { gameId :: Maybe Integer
                 , player :: [Player]
                 , status :: Bool
                 , rounds :: [Round]
                 } deriving (Show, Eq)

-- | Game can be converted to and from a JSON.
$(deriveJSON defaultOptions{omitNothingFields = True} ''Game)
