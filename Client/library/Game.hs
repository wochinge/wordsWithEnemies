module Game where 

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import           System.Exit (exitSuccess)
import           Network.PlayerClient
import           Network.GameClient
import           Data.Maybe
import           GHC.Conc (threadDelay)
import           Types.Player
import           Types.Game
import           Types.Round
import qualified Types.Solution as S
import qualified Types.Score as Score

welcomeMessage :: String
welcomeMessage = "Welcome to Words with Enemies\n\n \
                 \Please choose one of the following options:\n\
                 \[s]: Start game \t [h]: Help \t [q]: Quit"

waitingMessage :: String
waitingMessage = "Searching for a Teammate ..."

play :: IO ()
play = do 
    hSetBuffering stdout NoBuffering
    putStrLn welcomeMessage
    option <- getLine
    handleOption option

handleOption :: String -> IO ()
handleOption option
    | option == "q" = exitSuccess
    | option == "h" = help
    | option == "s" = enterName
    | otherwise = play
                
help :: IO ()
help = do 
    putStrLn "Help: \n\
    \5 Turns Each turn the two user are given random letters \n\
    \The two user must submit a dictionary checked word derived from these letters \n\
    \The words are compared. The winner of the duel is determined by whoever has the most left over letters.\n\
    \1 point is awarded for each left over letter.\
    \At the end of 5 turns who ever gets the most points wins the game."
    play

enterName :: IO ()
enterName = do
    hSetBuffering stdout NoBuffering
    putStrLn "Please enter your nickname:"
    nickname <- getLine
    handleNickname nickname

handleNickname :: String -> IO ()
handleNickname name 
    | null name = do
        putStrLn "Sorry, this is not a valid nickname!"
        enterName
    | otherwise = do
        player <- createPlayer name
        print player
        checkForGame $ fromJust player

checkForGame :: Player -> IO ()
checkForGame player = do
    putStrLn waitingMessage
    game <- loopForGame player
    startGame player game

loopForGame :: Player -> IO Game
loopForGame player = do 
    game <- getStatus player
    maybe
        (do threadDelay 1000000
            loopForGame player)
        return
        game
 
teammateMessage :: String
teammateMessage = "Your Teammate is "
    
startGame :: Player -> Game -> IO ()
startGame self game = do
    let teammate = name $ head $ filter (/= self) (player game)
    putStrLn $ teammateMessage ++ teammate
    playRound self game

lettersMessage :: String
lettersMessage = "Please form a word out of the following letters"

maxRoundNr :: Game -> Integer
maxRoundNr game = maximum $ map (\round -> fromJust $ roundNr round) $ rounds game

playRound :: Player -> Game -> IO ()
playRound self game = do
    let round = head $ filter (\round -> fromJust (roundNr round) == maxRoundNr game) $ rounds game
    putStrLn lettersMessage
    putStrLn $ letters round
    userSolution <- getLine
    postSolution (S.Solution Nothing userSolution self) game
    newGame <- loopForRound round game
    let lastRound = head $ filter (\round -> fromJust (roundNr round) == (maxRoundNr newGame -1)) $ rounds newGame
    case roundScore lastRound of
        Nothing -> putStrLn "Oh it's a tie!"
        Just (Score.Score _ points player) -> if player == self
                                                   then putStrLn $ "You won! You scored " ++ show points ++ " points."
                                                   else putStrLn $ "I'm sorry, you lost. Your teammate scored " ++ show points ++ " points."
    putStrLn $ "Your total score is now " ++ myTotalScore self newGame
    putStrLn $ "The totalscore of you teammate is now " ++ teammateTotalScore self newGame
    if not (status newGame)
        then 
            playRound self newGame
        else do
            putStrLn "This is the end of the game. Thanks for playing! Want to go again?"
            putStrLn "[y]: I want to player again! \t [n]: Nah, let me see the menu"
            getLine >>= handleEnd self

loopForRound :: Round -> Game -> IO Game
loopForRound lastRound game = do 
    newGame <- getGameWithNewRound lastRound game
    maybe
        (do threadDelay 1000000
            loopForRound lastRound game)
        return
        newGame
 
scoreRound :: Round -> String
scoreRound round = 
    show $ Score.score $ fromJust $ roundScore round
    
myTotalScore :: Player -> Game -> String
myTotalScore self game = 
    show $ sum wonScores
     where 
        roundWithScore = filter (\round -> isJust $ roundScore round) $ rounds game
        wonScores = map (\round -> if Score.player (fromJust $ roundScore round) == self then Score.score $ fromJust $ roundScore round else 0) roundWithScore

teammateTotalScore :: Player -> Game -> String
teammateTotalScore self game = 
    show $ sum wonScores
    where 
        roundWithScore = filter (\round -> isJust $ roundScore round) $ rounds game
        wonScores = map (\round -> if Score.player (fromJust $ roundScore round) /= self then Score.score $ fromJust $ roundScore round else 0) roundWithScore

handleEnd :: Player -> String -> IO ()
handleEnd player continue 
    | continue == "y" = do 
        insertPlayerInWaitingQueue player
        checkForGame player
    | otherwise = play