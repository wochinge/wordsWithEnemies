{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with games.
module Api.GameSite 
( apiInit
, createGame
) where

import 			 Snap.PrettySnap
import qualified Data.ByteString.Char8 as B
import 			 Types.Player
import 			 Snap.Core
import 			 Snap.Snaplet
import 			 Snap.Snaplet.SqliteSimple
import           Application
import           Api.GameApp
import           Data.List (delete)
import           DB.GameDAO (getGame, insertGame)
import           DB.Dictionary (wordExists, getRandomWord)
import           DB.ScoreDAO (insertScore)
import           DB.RoundDAO (insertRound, getRound)
import           DB.SolutionDAO (insertSolution, getSolutions)
import           DB.PlayerDAO (dropFromQueue)
import           Control.Monad.Trans
import           Control.Monad
import           Data.Maybe (fromJust, isJust)
import           System.Random (getStdGen, randomR)
import           Data.List (permutations)
import           Types.Game
import           Types.Score
import           Types.Solution
import           Types.Round

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

    playerSolution <- getJSONBody
    setStatusCode 201
    
    solutionExists <- withTop dictionary $ wordExists $ solution playerSolution
    let solutionFitsLetters = doesSolutionFitLetters playerSolution $ fromJust round

    if (solutionExists && solutionFitsLetters)
        then
            withTop solutionDAO $ insertSolution roundId playerSolution
        else
            withTop solutionDAO $ insertSolution roundId $ playerSolution {solution = ""}
    
    solutions <- withTop solutionDAO $ getSolutions roundId
    when ((length solutions) == 2) $ do
        saveScore roundId (head solutions) (last solutions)
        gameId <- getIdParam "id"
        createRound gameId

doesSolutionFitLetters :: Solution -> Round -> Bool
doesSolutionFitLetters (Solution _ solutionText _) round =
    length cleanSolution == 0
    where 
        cleanSolution = deleteFromText challengeText solutionText
        challengeText = letters round

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

-- | Creates a game with two waiting players and a first round.       
createGame :: [Player]                 -- ^ two waiting players
           -> Handler App GameApp () -- ^ nothing
createGame players = do
    let game = Game Nothing players False []
    game <- withTop gameDAO $ insertGame game
    withTop playerDAO $ dropFromQueue players
    createRound $ fromJust $ gameId game
    liftIO $ putStrLn $ show game
    
createRound :: Integer -> Handler App GameApp ()
createRound gameId = do
    letters <- withTop dictionary getRandomWord
    random <- shuffle letters
    let newRound = Round Nothing Nothing random Nothing []
    withTop roundDAO $ insertRound gameId newRound
    
shuffle :: String -> Handler App GameApp String 
shuffle xs = do
    gen <- liftIO getStdGen
    let (permNum, newGen) = randomR (1, fac (length xs) -1) gen
    return $ permutations xs !! permNum

fac :: (Enum a, Num a) => a -> a
fac n = product [n, n-1 .. 1]


