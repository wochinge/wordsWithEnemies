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
import qualified Data.List as L

-- | Welcome Message for the User.
welcomeMessage :: String -- ^ Message
welcomeMessage = "\n\n\
                 \Please choose one of the following options:\n\n\
                 \[s]: Start game \t [h]: Help \t [q]: Quit"

-- | Waiting Message for the User.
waitingMessage :: String -- ^ Message
waitingMessage = "\nSearching for a Teammate ..."

-- | Starts the game and give the user several options.
-- | Either to start playing, get help or quit the game.
play :: IO () -- ^ nothing
play = do 
    hSetBuffering stdout NoBuffering
    readFile "logo.txt" >>= putStrLn
    putStrLn welcomeMessage
    getLine >>= handleOption 

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
    putStrLn "\nHelp: \n\n\
    \A game consists of rounds. In each round you and your opponent get random letters.\
    \It is your task to build a word out of these letters which is longer then the word of your opponent.\
    \Be aware that your solution is checked with a dictionary so don't try to cheat! :-) \
    \For each letter which your word is longer than your oppenent's you get one point.\
    \At the end of 5 turns who ever gets the most points wins the game.\n\n\
    \Okay? So what do you want to do now?"
    putStrLn welcomeMessage
    getLine >>= handleOption

-- | Asks the user for the nickname he wants to use in the game.
enterName :: IO () -- ^ nothing
enterName = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nPlease enter your nickname:"
    getLine >>= handleNickname 

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
    putStrLn "Oh, that solution looks great! Now let's wait for your oppenents solution!"
    newGame <- loopForRound round game
    let lastRound = head $ filter (\round -> fromJust (roundNr round) == (maxRoundNr newGame -1)) $ rounds newGame
    case roundScore lastRound of
        Nothing -> putStrLn "\nOh it's a tie! \n"
        Just (Score.Score _ points player) -> if player == self
                                                   then putStrLn $ "\nYou won! You scored " ++ show points ++ " points. \n"
                                                   else putStrLn $ "\nI'm sorry, you lost. Your teammate scored " ++ show points ++ " points. \n"
    let (myScore, otherScore) = myTotalScore self newGame
    putStrLn $ "Your total score: " ++ show myScore ++ " points"
    putStrLn $ "Your teammate's score " ++ show otherScore ++ " points"
    if not (status newGame)
        then 
            playRound self newGame
        else do
            putStrLn "\nThis is the end of the game. Thanks for playing! Want to go again?\n"
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
             -> (Int, Int) -- ^ (score of player, other score)
myTotalScore self (Game _ _ _ rs) = 
    let roundsWithScore = filter (isJust . roundScore) rs
        scores = map (fromJust . roundScore) roundsWithScore
        (myScores, otherScores) = L.partition (\s -> self == Score.player s) scores
        foldScores = foldl (\a s -> Score.score s + a) 0
    in (foldScores myScores, foldScores otherScores)

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