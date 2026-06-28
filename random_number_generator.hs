import Data.Char
import Data.Maybe
import System.Random

maxNum :: Int
maxNum = 100

main :: IO ()
main = do
    randomGen <- newStdGen
    let randomNumbers = take 10 (randomRs (1, maxNum) randomGen)
    print randomNumbers
