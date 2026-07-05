import Data.Char
import Data.Maybe
import System.Random
import System.Environment
import System.Exit

maxNum :: Int
maxNum = 100

getSeed :: [String] -> IO Int
getSeed [] = getRandomSeed
getSeed(x:_) = return $ read x

getRandomSeed :: IO Int
getRandomSeed = fst . random <$> getStdGen

isNum :: String -> Bool
isNum [] = False
isNum (x:xs) = all isDigit xs && (x == '-' || isDigit x)

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

main :: IO ()
main = do
    randomGen <- newStdGen
    let randomNumbers = take 10 (randomRs (1, maxNum) randomGen)
    print randomNumbers