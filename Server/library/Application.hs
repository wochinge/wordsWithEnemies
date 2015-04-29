{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- | Snaplet auf oberster Ebene
module Application where

import Control.Lens
import Snap.Snaplet
import Snap
import Api.UserApi

data App = App {_userSnaplet :: Snaplet UserApp}

makeLenses ''App

initApplication :: SnapletInit App App
initApplication = makeSnaplet "wordsWithEnemies" "Web api for Words with Enemies" Nothing $ do
    nest <- nestSnaplet "user" userSnaplet $ apiInit
    return $ App nest


