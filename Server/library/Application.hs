{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- | Snaplet auf oberster Ebene
module Application where

import Control.Lens
import Snap.Snaplet
import Snap

data App = App

--makeLenses ''App

initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    --nested <- nestSnaplet "test" testApi apiInit
    return $ App


