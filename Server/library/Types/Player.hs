{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types.Player where

import Data.Aeson
import Data.Aeson.TH

data Player = Player {
      id :: Maybe Integer, -- ID kann auch leer sein
      name :: String} deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Player)