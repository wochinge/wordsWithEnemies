{-# LANGUAGE OverloadedStrings #-}

module Api.PlayerSite where

import Snap.PrettySnap
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Types.Player
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple
import Snap.Util.FileServe
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.SqliteSimple
import DB.PlayerDb
import Api.PlayerApp
import Data.Maybe (fromJust)
import Application

routes :: [(B.ByteString, Handler App UserApp())]
routes = [
            ("", method POST createPlayer)
         ]
         
apiInit :: SnapletInit App UserApp
apiInit = makeSnaplet "userApi" "handles users" Nothing $ do
          addRoutes routes
          return UserApp
         
createPlayer :: Handler App UserApp ()
createPlayer = do
             body <- readRequestBody 2048
             setStatusCode 201
             dBResult <- withTop db (savePlayer $ fromJust (user body))
             setBody $ head dBResult
             where
                  user body = decode body :: Maybe Player