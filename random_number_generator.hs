import Data.Char
import Data.Maybe
import System.Random
import System.Environment
import System.Exit
import Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

maxNum :: Int
maxNum = 100

getSeed :: [String] -> IO Int -- generates seed buat randomizer. kl egk dikasih lgsg randomize seed
getSeed [] = getRandomSeed
getSeed(x:_) = return $ read x

getRandomSeed :: IO Int
getRandomSeed = fst . random <$> getStdGen

isNum :: String -> Bool
isNum [] = False
isNum (x:xs) = all isDigit xs && (x == '-' || isDigit x)

getFromStdin :: String -> IO a -> (a -> Bool) -> (a -> b) -> IO b --transforms input jd the desired type
getFromStdin promptAgain inputAction isOk transformOk = do
    input <- inputAction
    if isOk input
        then return $ transformOk input
        else do
            putStr promptAgain
            getFromStdin promptAgain inputAction isOk transformOk

getNum :: String -> IO Int
getNum promptAgain =
    getFromStdin promptAgain getLine isNum read

getYesNo :: String -> IO Char
getYesNo promptAgain =
    getFromStdin promptAgain getChar (`elem` "yYnN") toUpper

verifyArgsOrQuit :: [String] -> IO ()
verifyArgsOrQuit args = 
    if verifyArgs args
        then putStrLn "Args OK"
        else do
            progName <- getProgName
            putStrLn $ progName ++ ": invalid arguments"
            exitWith (ExitFailure 1)
            
verifyArgs :: [String] -> Bool
verifyArgs [] = True
verifyArgs (x:xs) = null xs && isNum x

-- gameMode :: 

resolveLimit :: Int -> Int
resolveLimit 0 = 999999
resolveLimit 1 = 10
resolveLimit 2 = 5
resolveLimit 3 = 3
resolveLimit _ = 999999

gameDifficulty :: IO Int
gameDifficulty = do
    putStrLn "Select Difficulty (1 = Easy | 2 = Medium | 3 = Hard | _ = Unlimited): "
    choice <- getNum "That is not a valid number. Please type a digit:"

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
    putStrLn $ "Guess the number between 1 and " ++ show maxNum ++ " (or 0 to quit: )"
    let (target, nextGen) = randomR (1, 100) randomGen
    
    guessAttempts target limit 0

    again <- playAgain
    if again
        then main
        else quitGame

getRandomGen :: Int -> StdGen
getRandomGen = mkStdGen

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    verifyArgsOrQuit args
    seed <- getSeed args
    limit <- gameDifficulty
    playGame limit (getRandomGen seed)
    putStrLn "Game over"