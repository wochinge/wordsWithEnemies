{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module for the model solution.
module Types.Solution where

import 			 Data.Aeson.TH
import 			 Types.Player

data Solution = Solution { solutionId :: Maybe Integer
                         , solution :: String
                         , player :: Player
                         } deriving (Show, Eq)
    
$(deriveJSON defaultOptions{omitNothingFields = True} ''Solution)