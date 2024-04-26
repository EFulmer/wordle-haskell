module Main where

import Data.Char
import System.IO

main :: IO ()
main = do
    usrDict <- readFile "/usr/share/dict/words"
    let words = lines usrDict
    let validWords = filter (\w -> length w == 5) words
    let normalizedValidWords = map (map toLower) validWords
    print $ take 5 words
    putStrLn $ head validWords
    mapM putStrLn $ take 5 normalizedValidWords
    return ()
