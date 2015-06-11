{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with players.
module Api.PlayerSite (apiInit) where

import 			 Snap.PrettySnap (setStatusCode, setBody, getIdParam)
import qualified Data.ByteString.Char8 as B
import 			 Snap.Core
import 			 Snap.Snaplet
import 			 DB.PlayerDAO (savePlayer)
import 			 Api.PlayerApp
import 			 Application
import           DB.GameDAO (createGame, getGameWithPlayer)
import           Control.Monad (when)
import           Api.GameSite (createGame)
import qualified Data.Foldable as F (mapM_)

-- | Defines, which handler is used for which http call and route.
routes :: [(B.ByteString, Handler App PlayerApp ())]   -- ^ route, handler for this route and http call
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
    player <- getJSONBody
    setStatusCode 201
    dBResult <- withTop playerDAO (savePlayer player)
    setBody dBResult
    waitingPlayers <- withTop playerDAO getTwoWaitingPlayers
    when (length waitingPlayers > 1) $
        withTop gameSnaplet $ createGame waitingPlayers

-- | Handler, which provides information for a player whether a opponent was found.
getStatus :: Handler App PlayerApp () -- ^ nothing
getStatus = do
    userId <- getIdParam "id"
    game <- withTop gameDAO $ getGameWithPlayer userId
    F.mapM_ setBody game
    setStatusCode 200