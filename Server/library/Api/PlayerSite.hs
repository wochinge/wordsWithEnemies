{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with players.
module Api.PlayerSite (apiInit) where

import 			 Snap.PrettySnap
import 			 Data.Aeson
import qualified Data.ByteString.Char8 as B
import 			 Types.Player
import 			 Snap.Core
import 			 Snap.Snaplet
import 			 Snap.Snaplet.SqliteSimple
import 			 Snap.Util.FileServe
import 			 Snap.Snaplet.Auth
import 			 Snap.Snaplet.Auth.Backends.SqliteSimple
import 			 DB.PlayerDAO
import 			 DB.Dictionary
import 			 Api.PlayerApp
import 			 Data.Maybe (fromJust)
import 			 Application
import           DB.Dictionary
import           DB.GameDAO
import           System.Random
import           Types.Game
import           Types.Round
import           Data.List


-- | Defines, which handler is used for which http call and route.
routes :: [(B.ByteString, Handler App PlayerApp())]   -- ^ route, handler for this route and http call
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
    waitingPlayers <- withTop playerDb getTwoWaitingPlayers
    if length waitingPlayers > 1
        then do 
            createGame waitingPlayers
        else return ()
        
-- | Creates a game with two waiting players and a first round.       
createGame :: [Player]                 -- ^ two waiting players
           -> Handler App PlayerApp () -- ^ nothing
createGame players = do
    letters <- withTop dictionary getRandomWord
    random <- shuffle letters
    let game = Game Nothing players False [Round Nothing Nothing random Nothing []]
    withTop gameDb $ insertGame game
    return ()

shuffle :: String -> Handler App PlayerApp String 
shuffle xs = do
    gen <- getStdGen
    let (permNum, newGen) = randomR (1, fac (length xs) -1) gen
    return $ permutations xs !! permNum

fac n = product [n, n-1 .. 1]

-- | Handler, which provides information for a player whether a opponent was found.
getStatus :: Handler App PlayerApp () -- ^ nothing
getStatus = do
		userId <- getParam "id"
		--  game <- getGame userId
		  -- setBody game
		setStatusCode 200