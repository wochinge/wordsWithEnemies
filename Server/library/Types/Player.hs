{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model player.
module Types.Player where

import 			 Data.Aeson.TH
import 			 Control.Applicative
import           Database.SQLite.Simple

data Player = Player { playerId :: Maybe Integer -- ID kann auch leer sein
                     , name :: String
                     } deriving (Show, Eq)


instance FromRow Player where
    fromRow = Player <$> field <*> field
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Player)