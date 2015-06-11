{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model player.
module Types.Player where

import 			 Data.Aeson.TH
import 			 Control.Applicative
import           Database.SQLite.Simple

-- | Data type to represent a player.
data Player = Player { playerId :: Maybe Integer
                     , name :: String
                     } deriving (Show, Eq)


-- | Can be converted from a sql result row.
instance FromRow Player where
    fromRow = Player <$> field <*> field

-- | Player can be converted to and from a JSON.          
$(deriveJSON defaultOptions{omitNothingFields = True} ''Player)