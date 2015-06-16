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

-- | Welcome Message for the User.
welcomeMessage :: String -- ^ Message
welcomeMessage = "\n\
                 \Welcome to Words with Enemies\n\n\
                 \Please choose one of the following options:\n\
                 \[s]: Start game \t [h]: Help \t [q]: Quit"

-- | Waiting Message for the User.
waitingMessage :: String -- ^ Message
waitingMessage = "\nSearching for a Teammate ..."

-- | Starts the game and give the user several options.
-- | Either to start playing, get help or quit the game.
play :: IO () -- ^ nothing
play = do 
    hSetBuffering stdout NoBuffering
    putStrLn welcomeMessage
    option <- getLine
    handleOption option

-- | Handles the users input on how to continue with the game.
handleOption :: String -- ^ option
             -> IO ()  -- ^ nothing
handleOption option
    | option == "q" = exitSuccess
    | option == "h" = help
    | option == "s" = enterName
    | otherwise = play

-- | Prints out the rules of the game and then restarts the game.
help :: IO () -- ^ nothing
help = do 
    putStrLn "\nHelp: \n\
    \5 Turns Each turn the two user are given random letters \n\
    \The two user must submit a dictionary checked word derived from these letters \n\
    \The words are compared. The winner of the duel is determined by whoever has the most left over letters.\n\
    \1 point is awarded for each left over letter.\
    \At the end of 5 turns who ever gets the most points wins the game."
    play

-- | Asks the user for the nickname he wants to use in the game.
enterName :: IO () -- ^ nothing
enterName = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nPlease enter your nickname:"
    nickname <- getLine
    handleNickname nickname

-- | Handles the nickname of the user.
-- | ensures that it's not null, then the player is send to the server.
-- | Then see if there is another person to play with.
handleNickname :: String -- ^ nickname
               -> IO ()  -- ^ nothing
handleNickname name 
    | null name = do
        putStrLn "\nSorry, this is not a valid nickname!"
        enterName
    | otherwise = do
        player <- createPlayer name
        checkForGame $ fromJust player

-- | Searches for a Teammate then starts the game.
checkForGame :: Player -- ^ player who's looking for a teammate
             -> IO ()  -- ^ nothing
checkForGame player = do
    putStrLn waitingMessage
    game <- loopForGame player
    startGame player game

-- | Search for a Teammate.
loopForGame :: Player   -- ^ player who's looking for a teammate
            -> IO Game  -- ^ game with to players
loopForGame player = do 
    game <- getStatus player
    maybe
        (do threadDelay 1000000
            loopForGame player)
        return
        game

-- |  Teammate Message.
teammateMessage :: String -- ^ Message
teammateMessage = "\nYour Teammate is "

-- | Starts the game and the first round.
startGame :: Player -- ^ current player
          -> Game   -- ^ game to play
          -> IO ()  -- ^ nothing
startGame self game = do
    let teammate = name $ head $ filter (/= self) (player game)
    putStrLn $ teammateMessage ++ teammate
    playRound self game

-- | Tells the player what to do with the letters.
lettersMessage :: String -- ^ Message
lettersMessage = "\nPlease form a word out of the following letters"

-- | Finds the newest round of the game.
maxRoundNr :: Game    -- ^ current game
           -> Integer -- ^ highest round number in the game
maxRoundNr game = maximum $ map (fromJust . roundNr) $ rounds game

-- | Starts a round.
-- | prints the letters and gets the solution which is pushed to the server.
-- | If both players typed in their solution, the server sends the outcome of the round.
-- | If the last round is played the game is ended.
playRound :: Player -- ^ current player
          -> Game   -- ^ current game
          -> IO ()  -- ^ nothing
playRound self game = do
    let round = head $ filter (\round -> fromJust (roundNr round) == maxRoundNr game) $ rounds game
    putStrLn lettersMessage
    putStrLn $ letters round
    userSolution <- getLine
    postSolution (S.Solution Nothing userSolution self) game
    newGame <- loopForRound round game
    let lastRound = head $ filter (\round -> fromJust (roundNr round) == (maxRoundNr newGame -1)) $ rounds newGame
    case roundScore lastRound of
        Nothing -> putStrLn "\nOh it's a tie! \n"
        Just (Score.Score _ points player) -> if player == self
                                                   then putStrLn $ "\nYou won! You scored " ++ show points ++ " points. \n"
                                                   else putStrLn $ "\nI'm sorry, you lost. Your teammate scored " ++ show points ++ " points. \n"
    putStrLn $ "Your total score is now " ++ myTotalScore self newGame
    putStrLn $ "The totalscore of you teammate is now " ++ teammateTotalScore self newGame
    if not (status newGame)
        then 
            playRound self newGame
        else do
            putStrLn "\nThis is the end of the game. Thanks for playing! Want to go again?"
            putStrLn "[y]: I want to player again! \t [n]: Nah, let me see the menu"
            getLine >>= handleEnd self

-- | Looks if there are both solutions, the outcome and a new round.
loopForRound :: Round   -- ^ last round
             -> Game    -- ^ current game
             -> IO Game -- ^ game with new round
loopForRound lastRound game = do 
    newGame <- getGameWithNewRound lastRound game
    maybe
        (do threadDelay 1000000
            loopForRound lastRound game)
        return
        newGame

-- | Score of current player.
myTotalScore :: Player -- ^ current player
             -> Game   -- ^ current game
             -> String -- ^ score
myTotalScore self game = 
    show $ sum wonScores
     where 
        roundWithScore = filter (isJust . roundScore) $ rounds game
        wonScores = map (\round -> if Score.player (fromJust $ roundScore round) == self then Score.score $ fromJust $ roundScore round else 0) roundWithScore

-- | Score of teammate.
teammateTotalScore :: Player -- ^ current player
                   -> Game   -- ^ current game
                   -> String -- ^ score
teammateTotalScore self game = 
    show $ sum wonScores
    where 
        roundWithScore = filter (isJust . roundScore) $ rounds game
        wonScores = map (\round -> if Score.player (fromJust $ roundScore round) /= self then Score.score $ fromJust $ roundScore round else 0) roundWithScore

-- | Handles options at the end of the game.
-- | Continue or end the game.
handleEnd :: Player -- ^ current player
          -> String -- ^ option of the player
          -> IO ()  -- ^ nothing
handleEnd player continue 
    | continue == "y" = do 
        insertPlayerInWaitingQueue player
        checkForGame player
    | otherwise = play