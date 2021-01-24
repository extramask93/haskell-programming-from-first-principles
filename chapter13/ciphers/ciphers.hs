module Cipher where
import Data.Char 
caesar::String->Int->String 
caesar wrd s = map shift'' wrd where
    shift::Int->Char->Char
    shift amount c = case c of
         _ | c `elem` ['a'..'z'] -> shift' amount c
           |otherwise -> c
    shift' amount c = if chr (ord c + amount) > 'z' then
         chr $(ord c + amount) `mod` (ord 'z'+ 1) + (ord 'a')
         else 
             chr $ord c + amount
    shift'' = shift s

unCaesar::String->Int->String
unCaesar wrd s = map shift'' wrd where
    shift amount c = case c of
        _ | c `elem` ['a'..'z'] -> shift' amount c
          |otherwise -> c
    shift' amount c = chr a where
        a = if (ord c-amount) < ord 'a' then ord 'z' + (ord c - ord 'a' - amount+1) else
            ord c - amount
    shift'' = shift s
------------------------------------------------------------------------------------------
--helper functions to replicate a pattern
repl :: String -> [Char]
repl str = repl' str 0 
repl'::String->Int->[Char]
repl' str n = (str !! n) : repl' str ((n+1) `mod` length str) 
---------
vigenere::String->String->String 
vigenere s c = fun code s where
      alphaCnt = length $filter (\x-> x `elem` ['a'..'z']) s
      code = take alphaCnt $ repl c
      fun [] ccs = ccs
      fun (cipher:ciphers) (cc:ccs) = if cc `elem` ['a'..'z'] then caesar [cc] (ord cipher - ord 'a') ++ fun ciphers ccs else 
          cc : fun (cipher:ciphers) ccs
unVigenere::String->String->String 
unVigenere s c = fun code s where
      alphaCnt = length $filter (\x-> x `elem` ['a'..'z']) s
      code = take alphaCnt $ repl c
      fun [] ccs = ccs
      fun (cipher:ciphers) (cc:ccs) = if cc `elem` ['a'..'z'] then unCaesar [cc] (ord cipher - ord 'a') ++ fun ciphers ccs else 
          cc : fun (cipher:ciphers) ccs
--------------CHAPTER 13 exercise----------------------------------
caesarIO::IO ()
caesarIO = do
    putStr "Please specify string to encode: "
    s <- getLine 
    putStr "Please specify shift: "
    shift <- getLine 
    let a = caesar s (read shift::Int)
    putStrLn $"Encoded string: " ++ a
    return ()
vigenereIO::IO ()
vigenereIO = do
    putStr "Please specify string to encode: "
    s <- getLine 
    putStr "Please specify code string: "
    code <- getLine 
    let a = vigenere s code
    putStrLn $"Encoded string: " ++ a
    return ()
------------------------------------------------------------------------------
