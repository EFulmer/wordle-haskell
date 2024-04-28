module Main where

import Control.Monad
import Data.Char
import qualified Data.Set as Set -- required install

import System.IO
import System.Random -- required install

data Color = Green | Yellow | Gray deriving (Show, Ord, Eq)

data LetterMatch = LetterMatch
    { letter :: Char
    , pos :: Int
    , color :: Color
    } deriving (Show)

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

checkOneCharacter :: Char -> Int -> String -> LetterMatch
checkOneCharacter guess pos answer
    | answer !! pos == guess = LetterMatch { letter = guess, pos = pos, color = Green  }
    | guess `elem` answer    = LetterMatch { letter = guess, pos = pos, color = Yellow }
    | otherwise              = LetterMatch { letter = guess, pos = pos, color = Gray   }

checkWholeWord :: String -> String -> [LetterMatch]
checkWholeWord guess answer = [ checkOneCharacter (guess !! i) i answer | i <- [0..4] ]

colorCode :: Color -> String
colorCode Green  = "\ESC[42m"
colorCode Yellow = "\ESC[43m"
colorCode Gray   = "\ESC[100m"

formatHint :: [LetterMatch] -> String
formatHint matches = join $ map formatOne matches
    where
        formatOne match = colorCode (color match) ++ [letter match]

strHint :: [LetterMatch] -> String
strHint matches = "\ESC[107m" ++ formatHint matches ++ "\ESC[0m"

guessValid :: String -> Set.Set String -> Bool
guessValid guess dictionary = (length guess == 5) && (all isAscii guess) && guess `Set.member` dictionary

isCorrect :: [LetterMatch] -> Bool
isCorrect matches = all (\match -> (color match) == Green) matches

playWordle :: String -> Set.Set String -> Int -> IO ()
playWordle answer dictionary roundsLeft
    | roundsLeft == 0 = putStrLn "Sorry, you're out of guesses. The word was: " >> putStrLn answer
    | otherwise = do
        guess <- askUser
        if not $ guessValid guess dictionary
        then putStrLn "That's not a recognized 5-letter word. Please try again." >> playWordle answer dictionary roundsLeft
        else do
            let hint = checkWholeWord guess answer
            putStrLn $ strHint hint
            if isCorrect hint
            then do
                putStrLn "Good work!"
            else do
                putStrLn $ "Try again. You have " ++ (show (roundsLeft - 1)) ++ " guesses left."
                playWordle answer dictionary (roundsLeft - 1)

main :: IO ()
main = do
    words <- preprocess $ getStandardWordList
    let dictionary = Set.fromList words
    rng <- getStdGen
    let word = select rng words
    playWordle word dictionary 6
