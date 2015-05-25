{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model score.
module Types.Score where

import 			 Data.Aeson
import 			 Data.Aeson.TH
import 			 Types.Player

data Score = Score { scoreId :: Maybe Integer
                   , score :: Int
                   , player :: Player 
                   } deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Score)