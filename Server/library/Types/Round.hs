{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Round where

import           Data.Aeson
import           Data.Aeson.TH
import           Database.SQLite.Simple
import           Control.Applicative
import           Types.Score
import           Types.Solution

data Round = Round { letters :: [Char]
                   , roundScore :: Score
                   , solutions :: [Solution]
                   } deriving (Show)


instance FromRow Round where
    fromRow = Round <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Round)