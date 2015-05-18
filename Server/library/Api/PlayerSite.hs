{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with players.
module Api.PlayerSite (apiInit) where

import 			     Snap.PrettySnap
import 			     Data.Aeson
import qualified Data.ByteString.Char8 as B
import 			     Types.Player
import 			     Snap.Core
import 			     Snap.Snaplet
import 			     Snap.Snaplet.SqliteSimple
import 			     Snap.Util.FileServe
import 			     Snap.Snaplet.Auth
import 			     Snap.Snaplet.Auth.Backends.SqliteSimple
import 			     DB.PlayerDAO
import 			     DB.Dictionary
import 			     Api.PlayerApp
import 			     Data.Maybe (fromJust)
import 			     Application

-- | Defines, which handler is used for which http call and route.
routes :: [(B.ByteString,               -- ^ route
            Handler App PlayerApp())]   -- ^ handler for this route and http call
routes = [ (""          , method POST createPlayer)
         , (":id/status", method GET  getStatus)
         ]
         
-- | Initializes the snaplet.         
apiInit :: SnapletInit App PlayerApp -- ^ Snaplet initializer
apiInit = makeSnaplet "playerApi" "handles users" Nothing $ do
    addRoutes routes
    return PlayerApp

-- | Handler, which takes care of creating players.   
createPlayer :: Handler App PlayerApp () -- ^ nothing
createPlayer = do
    body <- readRequestBody 2048
    setStatusCode 201
    dBResult <- withTop playerDb (savePlayer $ decodeBody body)
    setBody $ dBResult

-- | Handler, which provides information for a player whether a opponent was found.
getStatus :: Handler App PlayerApp () -- ^ nothing
getStatus = do
		userId <- getParam "id"
		--  game <- getGame userId
		  -- setBody game
		setStatusCode 200