module Main where

import Control.Monad
import Data.Char
import qualified Data.Set as Set -- required install
import System.Console.ANSI hiding (Color, Green, Yellow, Gray) -- required install
import System.IO
import System.Random -- required install

data Color = Green | Yellow | Gray deriving (Show, Ord, Eq)

data LetterMatch = LetterMatch
    { letter :: Char
    , pos :: Int
    , color :: Color
    } deriving (Show)

-- Setup (get the word) functions
select :: StdGen -> [String] -> String
select gen ss = ss !! x
    -- TODO: figure out how to keep the generator too
    where (x, _) = randomR (0, n) gen
          n = length ss

getStandardWordList :: IO [String]
getStandardWordList = do
    usrDict <- readFile "/usr/share/dict/words"
    return $ lines usrDict

filterWords :: [String] -> [String]
filterWords ss = filter (\s -> length s == 5 && all isAscii s) ss

normalizeWords :: [String] -> [String]
normalizeWords = map $ map toLower

preprocess :: IO [String] -> IO [String]
preprocess = liftM $ (filterWords . normalizeWords)

-- Game logic functions
checkOneCharacter :: Char -> Int -> String -> LetterMatch
checkOneCharacter guess pos answer
    | answer !! pos == guess = LetterMatch { letter = guess, pos = pos, color = Green  }
    | guess `elem` answer    = LetterMatch { letter = guess, pos = pos, color = Yellow }
    | otherwise              = LetterMatch { letter = guess, pos = pos, color = Gray   }

checkWholeWord :: String -> String -> [LetterMatch]
checkWholeWord guess answer = [ checkOneCharacter (guess !! i) i answer | i <- [0..4] ]

guessValid :: String -> Set.Set String -> Bool
guessValid guess dictionary = (length guess == 5) && (all isAscii guess) && guess `Set.member` dictionary

isCorrect :: [LetterMatch] -> Bool
isCorrect matches = all (\match -> (color match) == Green) matches

-- Colorizing functions
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

-- I/O functions
askUser :: IO String
askUser = do
    getChars

getChars :: IO String
getChars = do
    input <- getChar
    if input == '\n'
    then return ""
    else do
        rest <- getChars
        return (input:rest)

-- Game loop
playWordle :: String -> Set.Set String -> Int -> IO ()
playWordle answer dictionary roundsLeft
    | roundsLeft == 0 = putStrLn "Sorry, you're out of guesses. The word was: " >> putStrLn answer >> putStrLn "Play another game soon!"
    | otherwise = do
        putStr "> "
        hFlush stdout
        guess <- askUser
        if not $ guessValid guess dictionary
        then putStrLn "That's not a recognized 5-letter word. Please try again." >> playWordle answer dictionary roundsLeft
        else do
            let hint = checkWholeWord guess answer
            -- putStrLn $ strHint hint
            printGuessHighlighted $ strHint hint
            if isCorrect hint
            then do
                putStrLn "Good work! Why not play another game soon?"
            else do
                playWordle answer dictionary (roundsLeft - 1)

printGuessHighlighted :: String -> IO ()
printGuessHighlighted word = do
    putStr $ cursorUpLineCode 1
    clearLine
    setCursorColumn 0
    putStrLn  word

main :: IO ()
main = do
    putStrLn "Welcome to Wordle!"
    words <- preprocess $ getStandardWordList
    let dictionary = Set.fromList words
    rng <- getStdGen
    let word = select rng words
    playWordle word dictionary 6
