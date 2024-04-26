module Main where

import Control.Monad
import Data.Char
import System.IO
import System.Random

randGen = mkStdGen 99

select :: StdGen -> [String] -> String
select gen ss = ss !! x
    -- TODO: figure out how to keep the generator too
    where (x, _) = randomR (0, n) gen
          n = length ss

-- get all the words
getStandardWordList :: IO [String]
getStandardWordList = do
    usrDict <- readFile "/usr/share/dict/words"
    return $ lines usrDict

-- half the point of the `normalizeWords` and `filterWords` functions is so I can add more preprocessing in there later
filterWords :: [String] -> [String]
filterWords ss = filter (\w -> length w == 5) ss

normalizeWords :: [String] -> [String]
normalizeWords = map $ map toLower

preprocess = liftM $ (filterWords .normalizeWords)

main :: IO ()
main = do
    words <- preprocess $ getStandardWordList
    mapM putStrLn $ take 5 words
    let word = select randGen words
    putStrLn $ "The wordle of the day is " ++ word
