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
import           Types.Score
import           Types.Solution
import           Types.Round
import           Data.List (delete)
import           DB.GameDAO
import           DB.Dictionary
import           DB.ScoreDAO
import           DB.RoundDAO
import           DB.SolutionDAO (insertSolution, getSolutions)
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
    round <- withTop roundDAO $ getRound roundId
    let task = letters $ fromJust round

    playerSolution <- getJSONBody
    setStatusCode 201
    solutionExists <- withTop dictionary $ wordExists $ solution playerSolution

    let cleanSolution = deleteFromText task $ solution playerSolution
    let solutionFitsLetters = (length cleanSolution) == 0

    if (solutionExists && solutionFitsLetters)
        then
            withTop solutionDAO $ insertSolution roundId playerSolution
        else
            withTop solutionDAO $ insertSolution roundId $ playerSolution {solution = ""}
    
    solutions <- withTop solutionDAO $ getSolutions roundId
    when ((length solutions) == 2) $ do
        saveScore roundId (head solutions) (last solutions)

saveScore :: Integer -> Solution -> Solution -> Handler App GameApp ()
saveScore roundId (Solution _  letters1 player1) (Solution _ letters2 player2) 
    | letters1L > letters2L = withTop scoreDAO $ insertScore roundId $ Score Nothing (letters1L - letters2L) player1
    | letters1L < letters2L= withTop scoreDAO $ insertScore roundId $ Score Nothing (letters2L - letters1L) player2
    | otherwise = return ()
    where
        letters1L = length letters1
        letters2L = length letters2

deleteFromText :: String -> String -> String
deleteFromText _ [] = []
deleteFromText [] text = text
deleteFromText (x:xs) text = deleteFromText xs $ delete x text 




