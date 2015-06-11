{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model round.
module Types.Round where

import           Data.Aeson.TH
import           Types.Score
import           Types.Solution

-- | Data type to represent a round of a game.
data Round = Round { roundId :: Maybe Integer
                   , roundNr :: Maybe Integer
                   , letters :: String
                   , roundScore :: Maybe Score
                   , solutions :: [Solution]
                   } deriving (Show, Eq)

-- | Round can be converted to and from a JSON.                         
$(deriveJSON defaultOptions{omitNothingFields = True} ''Round)