module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]
allWords::IO WordList
allWords = do
    dict <- readFile "data/words.txt"
    return (lines dict)
minWordLength::Int
minWordLength=5
maxWordLength::Int
maxWordLength=10

gameWords::IO WordList
gameWords = do
     allWords' <- allWords
     return (filter (\w -> length w `elem` [minWordLength..maxWordLength]) allWords')
randomWord::WordList->IO String
randomWord l =do
    idx <- randomRIO (0,length l -1)
    return (l !! idx)
randomWord'::IO String 
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Integer;

instance Show Puzzle where
     show (Puzzle _ discovered guessed cnt) =
          intersperse ' ' (fmap renderPuzzleChar discovered)
           ++ " Guessed so far: " ++ guessed ++ "\n Tries left:" ++show cnt
freshPuzzle::String->Puzzle
freshPuzzle wrd = Puzzle wrd (map (const Nothing) wrd) [] 7
charInWord::Puzzle->Char->Bool 
charInWord (Puzzle wrd _ _ _) c = c `elem` wrd
alreadyGuessed::Puzzle->Char->Bool 
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g
renderPuzzleChar::Maybe Char-> Char
renderPuzzleChar c =  
    case c of
        Nothing ->  '_'
        Just c' ->  c'
fillInCharacter::Puzzle->Char->Puzzle
fillInCharacter (Puzzle word filledInSoFar s cnt) c =
    Puzzle word newFilledInSoFar (c:s) newCnt where
        zipper guessed wordChar guessChar = 
            if wordChar == guessed then
                Just wordChar else 
                    guessChar
        newCnt = if c `elem` word then cnt else cnt-1
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
handleGuess::Puzzle->Char->IO Puzzle
handleGuess puzzle guess = do
    putStrLn $"Your guess was: "++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_,True)-> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True,_)-> do
            putStrLn "This character was in the word, filling in..."
            return (fillInCharacter puzzle guess)
        (False,_)->do
            putStrLn "This character wasn't in the word, try again"
            return (fillInCharacter puzzle guess)
gameOver::Puzzle->IO ()
gameOver (Puzzle wordToGuess _ _ cnt) =
    if cnt < 0 then
        do 
            putStrLn "You lose!"
            putStrLn $"The word was: "++ wordToGuess
            exitSuccess
    else
        return ()
gameWin::Puzzle->IO ()
gameWin (Puzzle _ gs _ _) = 
    if all isJust gs then do
        putStrLn "You win!"
        exitSuccess
    else
        return ()
runGame::Puzzle->IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++show puzzle
    putStr "Guess a letter: "
    guess <- getLine 
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"
main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
