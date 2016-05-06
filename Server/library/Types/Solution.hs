{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model solution.
module Types.Solution where

import        Data.Aeson.TH
import        Types.Player

-- | Data type to represent a player.
data Solution = Solution { solutionId :: Maybe Integer
                         , solution :: String
                         , player :: Player
                         } deriving (Show, Eq)

-- | Solution can be converted to and from a JSON.
$(deriveJSON defaultOptions{omitNothingFields = True} ''Solution)
