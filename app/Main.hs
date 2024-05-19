{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Char
import qualified Data.Set as Set -- required install
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Numeric.Natural
import System.Console.ANSI hiding (Color, Green, Yellow) -- required install
import System.IO
import System.Random -- required install

data Color = Green | Yellow | Gray deriving (Show, Ord, Eq)

data LetterMatch = LetterMatch
  { letter :: Char,
    position :: Int,
    color :: Color
  }
  deriving (Show)

atMay ::  [a] -> Int -> Maybe a
atMay xs idx
    | idx < 0 = Nothing
    | idx > (length xs) = Nothing
    | otherwise = Just $ xs !! idx

-- Setup (get the word) functions
-- select returns a Maybe for the one in a billion chance that we mess up the indexing
select :: StdGen -> [T.Text] -> Maybe T.Text
select gen ss = atMay ss x
  where
    (x, _) = randomR (0, n) gen
    n = length ss

getStandardWordList :: IO [T.Text]
getStandardWordList = do
  usrDict <- T.IO.readFile "/usr/share/dict/words"
  return $ T.lines usrDict

filterWords :: [T.Text] -> [T.Text]
filterWords ss = filter (\s -> T.length s == 5 && T.all isAscii s) ss

normalizeWords :: [T.Text] -> [T.Text]
normalizeWords = fmap T.toLower

preprocess :: IO [T.Text] -> IO [T.Text]
preprocess = liftM $ (filterWords . normalizeWords)

-- Game logic functions
checkOneCharacter :: Char -> Int -> T.Text -> LetterMatch
checkOneCharacter guess pos answer
  | (T.index answer pos) == guess = LetterMatch {letter = guess, position = pos, color = Green}
  | guess `T.elem` answer = LetterMatch {letter = guess, position = pos, color = Yellow}
  | otherwise = LetterMatch {letter = guess, position = pos, color = Gray}

checkWholeWord :: T.Text -> T.Text -> [LetterMatch]
checkWholeWord guess answer = [checkOneCharacter (T.index guess i) i answer | i <- [0 .. 4]]

guessValid :: T.Text -> Set.Set T.Text -> Bool
guessValid guess dictionary = (T.length guess == 5) && (T.all isAscii guess) && guess `Set.member` dictionary

isCorrect :: [LetterMatch] -> Bool
isCorrect matches = all (\match -> (color match) == Green) matches

-- Colorizing functions
colorCode :: Color -> T.Text
colorCode Green = "\ESC[42m"
colorCode Yellow = "\ESC[43m"
colorCode Gray = "\ESC[100m"

formatOne :: LetterMatch -> T.Text
formatOne match = T.snoc (colorCode (color match)) (letter match)

formatHint :: [LetterMatch] -> T.Text
formatHint matches = T.concat $ fmap formatOne matches

strHint :: [LetterMatch] -> T.Text
strHint matches = T.append (T.append "\ESC[107m" (formatHint matches)) "\ESC[0m"

-- I/O functions
askUser :: IO T.Text
askUser = do
  getChars

getChars :: IO T.Text
getChars = do
  input <- getChar
  if input == '\n'
    then return $ T.pack ""
    else do
      rest <- getChars
      return $ T.cons input rest

-- Game loop
playWordle :: T.Text -> Set.Set T.Text -> Natural -> IO ()
playWordle answer dictionary roundsLeft
  | roundsLeft == 0 = T.IO.putStrLn "Sorry, you're out of guesses. The word was: " >> T.IO.putStrLn answer >> T.IO.putStrLn "Play another game soon!"
  | otherwise = do
      putStr "> "
      hFlush stdout
      guess <- askUser
      if not $ guessValid guess dictionary
        then putStrLn "That's not a recognized 5-letter word. Please try again." >> playWordle answer dictionary roundsLeft
        else do
          let hint = checkWholeWord guess answer
          printGuessHighlighted $ strHint hint
          if isCorrect hint
            then do
              putStrLn "Good work! Why not play another game soon?"
            else do
              playWordle answer dictionary (roundsLeft - 1)

printGuessHighlighted :: T.Text -> IO ()
printGuessHighlighted word = do
  putStr $ cursorUpLineCode 1
  clearLine
  setCursorColumn 0
  T.IO.putStrLn word

main :: IO ()
main = do
  putStrLn "Welcome to Wordle!"
  allPossibleWords <- preprocess $ getStandardWordList
  let dictionary = Set.fromList allPossibleWords
  rng <- getStdGen
  let word = select rng allPossibleWords
  case word of
    Just word' -> playWordle word' dictionary 6
    Nothing -> putStrLn "There was an error getting the words. File a GitHub issue at https://github.com/EFulmer/wordle-haskell and I'll fix it!"
