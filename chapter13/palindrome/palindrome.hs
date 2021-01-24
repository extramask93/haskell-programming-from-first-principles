module Palindrome where
import Control.Monad
import System.Exit
import Data.Char
palindrome::IO ()
palindrome = forever $ do
    line1<- getLine
    let line2 = filter (\x->not$ x `elem` "' \n\t") $map toLower line1
    case (line2 == reverse line2) of
        True -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess