{-# LANGUAGE OverloadedStrings #-}

-- | Snaplet, which offers API functions to deal with games.
module Api.GameSite 
( apiInit
, createGame
) where

import 			 Snap.PrettySnap (setStatusCode, setBody, getJSONBody, getIdParam)
import qualified Data.ByteString.Char8 as B
import 			 Types.Player
import 			 Snap.Core
import 			 Snap.Snaplet
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
         , (":id/round/newRound/:oldRoundNr/", method GET retrieveNewRound)
         ]
         
-- | Initializes the snaplet.         
apiInit :: SnapletInit App GameApp -- ^ Snaplet initializer
apiInit = makeSnaplet "game api" "handles games" Nothing $ do
    addRoutes routes
    return GameApp

-- | Returns the game for the id.
retrieveGame :: Handler App GameApp () -- ^ nothing
retrieveGame = do
    gameId <- getIdParam "id"
    game <- withTop gameDAO $ getGame gameId
    when (isJust game) $ setBody game
    setStatusCode 200
    
-- | Returns Game with new Round if new Round
retrieveNewRound :: Handler App GameApp () -- ^ nothing
retrieveNewRound = do
    gameId <- getIdParam "id"
    game <- withTop gameDAO $ getGame gameId
    when (isJust game) $ do
        oldRoundNr <- getIdParam "oldRoundNr"
        let newRound = filter (\round -> (fromJust $ roundNr round) > oldRoundNr) $ rounds $ fromJust game 
        when (newRound /= []) $ setBody game
    setStatusCode 200

-- | Inserts a user solution in the database.
-- | Depending whether both players posted a solution, the score is calculated and
-- | a new round is created.
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

-- | Checks whether the solution is made out of the letters of the challenge.
-- | E.g. "house" is a valid word, but cannot be build out of the the letters of "slat" (salt).
-- | But "ice" can be built out of the challenge letters "icecream".
doesSolutionFitLetters :: Solution -> Round -> Bool
doesSolutionFitLetters (Solution _ solutionText _) round =
    length cleanSolution == 0
    where 
        cleanSolution = deleteFromText challengeText solutionText
        challengeText = letters round

-- | Calculates the score and saves it in the db.
saveScore :: Integer  -- ^ databaseId of the round
          -> Solution -- ^ Solution of player1
          -> Solution -- ^ Solution of player2
          -> Handler App GameApp ()
saveScore roundId (Solution _  letters1 player1) (Solution _ letters2 player2) 
    | letters1L > letters2L = withTop scoreDAO $ insertScore roundId $ Score Nothing (letters1L - letters2L) player1
    | letters1L < letters2L= withTop scoreDAO $ insertScore roundId $ Score Nothing (letters2L - letters1L) player2
    | otherwise = return ()
    where
        letters1L = length letters1
        letters2L = length letters2

-- | Deletes all letters from the challenge word in the solution word. 
deleteFromText :: String -- ^ challenge word
               -> String -- ^ solution word
               -> String -- ^ hopefully empty word
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

-- | Creates a new round.
createRound :: Integer                -- ^ databaseId of the game
            -> Handler App GameApp () -- ^ nothing
createRound gameId = do
    letters <- withTop dictionary getRandomWord
    random <- shuffle letters
    let newRound = Round Nothing Nothing random Nothing []
    withTop roundDAO $ insertRound gameId newRound

-- | Shuffles a string randomly.
shuffle :: String                     -- ^ input string
        -> Handler App GameApp String -- ^ shuffled string
shuffle xs = do
    gen <- liftIO getStdGen
    let (permNum, _) = randomR (1, fac (length xs) -1) gen
    return $ permutations xs !! permNum

-- | Faculty method to calculate the possible permutations.
fac :: (Enum a, Num a) 
    => a -- ^ number
    -> a -- ^ faculty of the number.
fac n = product [n, n-1 .. 1]


