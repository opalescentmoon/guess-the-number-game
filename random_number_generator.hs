import Data.Char
import System.Random
import System.Exit
import Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

maxNum :: Int
maxNum = 100

isNum :: String -> Bool -- checks if input num semua or not
isNum [] = False
isNum (x:xs) = length (filter isDigit xs) == length xs && (x == '-' || isDigit x)

getFromStdin :: String -> IO a -> (a -> Bool) -> (a -> b) -> IO b -- transforms input jd the desired type
getFromStdin prompt inputAction isValid transformInput = do
    input <- inputAction
    if isValid input
        then return $ transformInput input
        else do
            putStr prompt
            getFromStdin prompt inputAction isValid transformInput

getNum :: String -> IO Int
getNum prompt =
    getFromStdin prompt getLine isNum read

resolveLimit :: Int -> Int
resolveLimit 0 = 0
resolveLimit 1 = 10
resolveLimit 2 = 5
resolveLimit 3 = 3
resolveLimit _ = 999999

gameDifficulty :: IO Int
gameDifficulty = do
    putStrLn "Select Difficulty:"
    putStrLn "0 = Quit"
    putStrLn "1 = Easy"
    putStrLn "2 = Medium"
    putStrLn "3 = Hard"
    putStrLn "Any other number = Unlimited"
    choice <- getNum "That is not a valid number. Please type a digit:"

    if choice == 0
        then quitGame >> return 0
        else do 
            let limit = resolveLimit choice
            if limit == 999999
                then putStrLn "You have unlimited attempts!"
                else putStrLn $ "You have " ++ show limit ++ " attempts!"
    
            return limit

guessAttempts :: Int -> Int -> Int -> IO ()  -- manages attempts
guessAttempts target limit attempts = do

    guessLimit target limit attempts

    Control.Monad.when (attempts >= limit) (return ())

    Control.Monad.unless (attempts >= limit) $ do
        putStr $ "Attempt " ++ show (attempts + 1) ++ ": "
        guess <- getNum "Invalid input. Please try again\n"

        if guess == 0
            then quitGame
        else if target == guess 
                then guessCorrect $ attempts + 1
                else do
                    guessWrong target attempts guess
                    guessAttempts target limit (attempts + 1)

guessLimit :: Int -> Int -> Int -> IO () --limits guesses
guessLimit target limit attempts = do
    Control.Monad.when (attempts >= limit) $
        putStrLn $ "Game over! The correct number is: " ++ show target

guessCorrect :: Int -> IO () --if guess correct
guessCorrect tries = do
    putStrLn $ "You guessed correct in " ++ show tries ++ " tries!"


guessWrong :: Int -> Int -> Int -> IO () --if guess wrong
guessWrong target attempts guess = do 
    if target < guess
        then putStrLn "Too high!"
        else putStrLn "Too low!"

showAnswer :: Int -> IO () --show correct answer
showAnswer answer = putStrLn $ "The answer was " ++ show answer

playAgain :: IO Bool
playAgain = do
    putStr "Play again? Y/N "
    input <- getLine 
    case input of
        "y" -> return True
        "Y" -> return True
        "n" -> return False
        "N" -> return False
        _   -> do
            putStrLn "Invalid option. Please type Y or N."
            playAgain

quitGame :: IO ()
quitGame = do
    putStrLn "Quitting game"
    exitSuccess

playGame :: Int -> StdGen -> IO ()
playGame limit randomGen = do
    putStrLn $ "Guess the number between 1 and " ++ show maxNum ++ " (or 0 to quit): "
    let (target, nextGen) = randomR (1, 100) randomGen
    
    guessAttempts target limit 0

    again <- playAgain
    if again
        then main
        else quitGame

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "================"
    putStrLn "GUESS THE NUMBER"
    putStrLn "================"
    randomGen <- newStdGen
    limit <- gameDifficulty
    playGame limit randomGen