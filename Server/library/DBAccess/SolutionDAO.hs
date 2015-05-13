{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.SolutionDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Player
import           Types.Solution

data SolutionDAO = SolutionDAO { solutionId :: Integer
                               , solutionText :: String
                               , playerOfSolution :: Integer
                               , roundid :: Integer
                               }

instance FromRow SolutionDAO where
  fromRow = SolutionDAO <$> field <*> field <*> field <*> field
  
getSolution :: SolutionDAO -> Player -> Solution
getSolution solution player = Solution (solutionText solution) player