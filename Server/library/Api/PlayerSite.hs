{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with players.
module Api.PlayerSite (apiInit) where

import 			 Snap.PrettySnap
import qualified Data.ByteString.Char8 as B
import 			 Snap.Core
import 			 Snap.Snaplet
import 			 DB.PlayerDAO
import 			 Api.PlayerApp
import 			 Data.Maybe
import 			 Application
import           DB.GameDAO
import           Control.Monad.Trans
import           Control.Monad
import           Api.GameSite (createGame)

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
    when (length waitingPlayers > 1)
        withTop gameSnaplet $ createGame waitingPlayers

-- | Handler, which provides information for a player whether a opponent was found.
getStatus :: Handler App PlayerApp () -- ^ nothing
getStatus = do
    userId <- getIdParam "id"
    liftIO $ print userId
    game <- withTop gameDAO $ getGameWithPlayer userId
    liftIO $ print game
    when (isJust game) $ setBody $ fromJust game
    setStatusCode 200