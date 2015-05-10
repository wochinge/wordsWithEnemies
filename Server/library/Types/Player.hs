{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Player where

import           Data.Aeson
import 			 Data.Aeson.TH
import 			 Database.SQLite.Simple
import 			 Control.Applicative

data Player = Player { playerId :: Maybe Integer -- ID kann auch leer sein
                     , name :: String
                     } deriving (Show, Eq)


instance FromRow Player where
    fromRow = Player <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Player)