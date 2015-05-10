{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Solution where

import 			 Data.Aeson
import 			 Data.Aeson.TH
import 			 Database.SQLite.Simple
import 			 Control.Applicative
import 			 Types.Player

data Solution = Solution { solution :: String
                         , player :: Player
                         } deriving (Show, Eq)


instance FromRow Solution where
    fromRow = Solution <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Solution)