{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model player.
module Types.Player where

import 			 Data.Aeson.TH
import 			 Control.Applicative

-- | Data type to represent a player.
data Player = Player { playerId :: Maybe Integer
                     , name :: String
                     } deriving (Show, Eq)

-- | Player can be converted to and from a JSON.          
$(deriveJSON defaultOptions{omitNothingFields = True} ''Player)