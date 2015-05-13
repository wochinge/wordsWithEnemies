{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module DBAccess.SolutionDAO where

import 			 Database.SQLite.Simple
import 			 Control.Applicative
import           Types.Player
import qualified Types.Solution as S

data SolutionDAO = SolutionDAO { solutionId :: Integer
                               , solutionText :: String
                               , playerOfSolution :: Integer
                               , roundid :: Integer
                               }

instance FromRow SolutionDAO where
  fromRow = SolutionDAO <$> field <*> field <*> field <*> field
  
getSolution :: SolutionDAO -> Player -> S.Solution
getSolution solution player = S.Solution (Just $ solutionId solution) (solutionText solution) player