{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model score.
module Types.Score where

import        Data.Aeson.TH
import        Types.Player

-- | Data type to represent a round score.
data Score = Score { scoreId :: Maybe Integer
                   , score :: Int
                   , player :: Player
                   } deriving (Show, Eq)

-- | Score can be converted to and from a JSON.
$(deriveJSON defaultOptions{omitNothingFields = True} ''Score)
