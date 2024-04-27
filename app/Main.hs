module Main where

import Control.Monad
import Data.Char
import qualified Data.Set as Set -- required install

import System.IO
import System.Random -- required install

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

-- preprocess takes any list of words, so we don't need to assume anything or rely on the /usr/share/dict/words file.
preprocess :: IO [String] -> IO [String]
preprocess = liftM $ (filterWords .normalizeWords)

askUser :: IO String
askUser = do
    putStrLn "Guess a word "
    getLine

data Color = Green | Yellow | Gray deriving (Show, Ord, Eq)

checkOneCharacter :: Char -> Int -> String -> Color
checkOneCharacter guess pos answer = case guess `elem` answer of
    True -> if guess == (answer !! pos) then Green else Yellow
    False -> Gray

checkWholeWord :: String -> String -> [Color]
checkWholeWord guess answer = [checkOneCharacter (guess !! i) i answer | i <- [0..(length answer)-1] ]

formatChar :: Char -> Color -> String
formatChar ch Green = "\ESC[42m" ++ [ch]
formatChar ch Yellow = "\ESC[43m" ++ [ch]
formatChar ch Gray = "\ESC[100m" ++ [ch]

formatGuess :: String -> String -> String
formatGuess guess answer = "\ESC[107m" ++ formattedGuess ++ "\ESC[0m"
    where
        -- gotta unnest the list of strings
        formattedGuess = foldl (++) "" $ zipWith formatChar guess guessColors
        guessColors = checkWholeWord guess answer

guessValid :: String -> Set.Set String -> Bool
guessValid guess dictionary = (length guess == 5) && (all isAscii guess) && guess `Set.member` dictionary

main :: IO ()
main = do
    words <- preprocess $ getStandardWordList
    let dictionary = Set.fromList words
    mapM putStrLn $ take 5 words
    let word = select randGen words
    putStrLn $ "The wordle of the day is " ++ word
    forever $ do
        guess <- askUser
        if not $ guessValid guess dictionary
        then putStrLn $ "That's not a recognized 5-letter word. Please try again."
        else putStrLn $ formatGuess guess word
