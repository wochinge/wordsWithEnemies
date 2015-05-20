{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model round.
module Types.Round where

import           Data.Aeson
import           Data.Aeson.TH
import           Types.Score
import           Types.Solution

data Round = Round { roundId :: Maybe Integer
                   , roundNr :: Maybe Integer
                   , letters :: [Char]
                   , roundScore :: Maybe Score
                   , solutions :: [Solution]
                   } deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Round)