{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with games.
module Api.GameSite (apiInit) where

import 			 Snap.PrettySnap
import 			 Data.Aeson
import qualified Data.ByteString.Char8 as B
import 			 Types.Player
import 			 Snap.Core
import 			 Snap.Snaplet
import 			 Snap.Snaplet.SqliteSimple
import           Application
import           Api.GameApp
import           DB.GameDAO
import           DB.SolutionDAO (insertSolution)
import           Control.Monad.Trans
import           Control.Monad
import           Data.Maybe

-- | Defines, which handler is used for which http call and route.
routes :: [(B.ByteString, Handler App GameApp ())]   -- ^ route, handler for this route and http call
routes = [ (":id", method GET retrieveGame)
         , (":id/round/:roundId/solution", method POST createSolution)
         ]
         
-- | Initializes the snaplet.         
apiInit :: SnapletInit App GameApp -- ^ Snaplet initializer
apiInit = makeSnaplet "game api" "handles games" Nothing $ do
    addRoutes routes
    return GameApp

retrieveGame :: Handler App GameApp () -- ^ nothing
retrieveGame = do
    gameId <- getIdParam "id"
    game <- withTop gameDAO $ getGame gameId
    when (isJust game) $ setBody game
    setStatusCode 200

createSolution :: Handler App GameApp () -- ^ nothing
createSolution = do
    roundId <- getIdParam "roundId"
    solution <- getJSONBody
    setStatusCode 201
    withTop solutionDAO $ insertSolution roundId solution
