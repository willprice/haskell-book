module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

-- Puzzle Answer FilledInParts Guessed 
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
        ", Guessed so far: " ++ guessed


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ lines dict

minWordLength :: Int
minWordLength  = 5

maxWordLength :: Int
maxWordLength  = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return $ filter gameLength aw
    where gameLength w = let l = length w in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord  wl = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle answer = Puzzle answer emptyGuesses []
    where emptyGuesses = (take (length answer) $ repeat Nothing)


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle ans _ _) guess = guess `elem` ans

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed  (Puzzle _ _ guesses) guess = guess `elem` guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle answer filledInSoFar guesses) guess = 
    Puzzle answer newFilledInSoFar (guess : guesses)
        where 
            zipper wordChar guessChar = 
                if wordChar == guess
                then Just wordChar
                else guessChar
            newFilledInSoFar = zipWith zipper answer filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
      (_, True) -> do
          putStrLn "You already guess that\
                   \ character, pick\
                   \ something else!"
          return puzzle
      (True, _) -> do
          putStrLn "This character was in the\
                   \ word, filling in the word\
                   \ accordingly"
          return (fillInCharacter puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in\
                   \ the word, try again."
          return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle answer _ guesses) = 
    if (length guesses) > 7
       then do 
           putStrLn "You lose!"
           putStrLn $ "The word was: " ++ answer
           exitSuccess
    else
        return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar 
       then do
           putStrLn "You win!"
           exitSuccess
        else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must\
                      \ be a single character" 


main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle $ fmap toLower word
    runGame puzzle
