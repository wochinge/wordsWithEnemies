module Game where 

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Exit (exitSuccess)
import Network.PlayerClient

welcomeMessage :: String
welcomeMessage = "Welcome to Words with Enemies\n\n \
                 \Please choose one of the following options:\n\
                 \[s]: Start game \t [h]: Help \t [q]: Quit"

startGame :: IO ()
startGame = do 
    hSetBuffering stdout NoBuffering
    putStrLn welcomeMessage
    option <- getLine
    handleOption (option)

handleOption :: String -> IO ()
handleOption option
    | option == "q" = exitSuccess
    | option == "h" = help
    | option == "s" = enterName
    | otherwise = startGame
                
help :: IO ()
help = do 
    putStrLn "Help: "
    startGame

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
        putStrLn $ show player