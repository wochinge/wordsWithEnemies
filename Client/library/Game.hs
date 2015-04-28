module Game where 

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Exit (exitSuccess)

welcomeMessage :: String
welcomeMessage = "Welcome to Words with Enemies\n\n \
                 \Bitte wählen Sie aus einer der folgenden Optionen:\n\
                 \[s]: Spiel starten \t [h]: Hilfe \t [q]: Beenden"

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
                | option == "s" = putStrLn "Spiel starten"
                | otherwise = startGame

                
help :: IO ()
help = do 
          putStrLn "Hilfe: "
          startGame