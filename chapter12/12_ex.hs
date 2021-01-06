
import Data.List
import Data.Maybe
import Data.Either
--12.1
notThe::String->Maybe String 
notThe word | word == "the" = Nothing 
       | word /= "the" = Just word;

repl :: Maybe String  -> String
repl Nothing = "a"
repl (Just a) = a 

replaceThe::String->String 
replaceThe [] = []
replaceThe str = repl (notThe currentWord) ++" " ++ replaceThe nextWords where
    currentWord = head (words str); nextWords = unwords (tail (words str))
--12.2
hasVowel (c:_) =  c `elem` "aeiouy"
countTheBeforeVowel::String->Integer
countTheBeforeVowel str = countTheBeforeVowel' str 0
countTheBeforeVowel'::String->Integer->Integer
countTheBeforeVowel' str cnt
    | length (words str) < 2 = cnt
    | Data.Maybe.isNothing (notThe (head (words str))) && hasVowel (words str !! 1) =
         countTheBeforeVowel' (unwords (tail (words str))) (cnt+1)
    | otherwise = countTheBeforeVowel' (unwords (tail (words str))) cnt
--12.3
isVowel c = c `elem` "aeiouy"
getVowels::String->String
getVowels = filter isVowel
countVowels::String->Integer
countVowels = toInteger . length . getVowels
--Validate the word
newtype Word' = Word' String deriving (Eq,Show)
vowels = "aeiou"
mkWord::String->Maybe Word'
mkWord w  = 
    let { v= length (filter (\x-> x `elem` vowels) w);
    c= length (filter (\x-> not (elem x vowels)) w)}
  in if v>c then Nothing else Just (Word' w)
--it's only natural 
data Nat = Zero | Succ Nat
    deriving (Eq, Show)
natToInteger::Nat->Integer 
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat::Integer->Maybe Nat
integerToNat i | i<0 =Nothing
    | i == 0 = Just Zero
    | otherwise = Just $ Succ $ unpack $ integerToNat $ i-1
        where unpack (Just x) = x
---------------------- small library for Maybe-----------------------------------------
isJust::Maybe a->Bool        
isJust Nothing = False
isJust (Just a) = True

isNothing::Maybe a->Bool 
isNothing = not.Main.isJust
--this is a catamorphism, reducing variables to a single value
mayybe::b->(a->b)->Maybe a -> b
mayybe alternative func may | Main.isNothing may = alternative
    |otherwise = func $ unpack may
        where unpack (Just x) = x
--
fromMaybe::a->Maybe a->a
fromMaybe a = mayybe a id
--
listToMaybe::[a]->Maybe a
listToMaybe [] = Nothing 
listToMaybe a = Just (head a)
-- 
maybeToList::Maybe a->[a]
maybeToList Nothing = []
maybeToList (Just a) = [a]
-- 
catMaybes::[Maybe a] -> [a]
catMaybes [] = []
catMaybes a = Main.maybeToList (head a) ++ Main.catMaybes (tail a)
--
flipMaybe::[Maybe a]-> Maybe [a]
flipMaybe [] = Just []
flipMaybe a = if any Main.isNothing a then Nothing else Just (Main.catMaybes a) 
---------------------------small library for Either----------------------------
fromLeft'::[a]->Either a b->[a]
fromLeft' _ (Left b) = [b]
fromLeft' a _ = a
lefts'::[Either a b]->[a]
lefts' = foldr (\x y-> fromLeft' [] x ++ y) []

fromRight'::[b]->Either a b->[b]
fromRight' _ (Right b) = [b]
fromRight' b _ = b
rights'::[Either a b]->[b]
rights' = foldr (\x y -> fromRight' [] x ++ y) []

partitionEithers'::[Either a b] -> ([a],[b])
partitionEithers' a = (lefts' a, rights' a)

eitherMaybe'::(b->c)->Either a b -> Maybe c
eitherMaybe' fun (Left a) = Nothing 
eitherMaybe' fun (Right b) = Just (fun b)
-- catamorphisms(folds) - let us break data structures down
either'::(a->c)->(b->c)->Either a b-> c
either' fun1 fun2 (Left a) = fun1 a
either' fun1 fun2 (Right b) = fun2 b

eitherMaybe''::(b->c)->Either a b->Maybe c
eitherMaybe'' fun = either' (const Nothing) (\x->Just (fun x))
---------------anamorphisms-------------------------------------
myIterate::(a->a)->a->[a]
myIterate fun seed = [seed] ++ myIterate fun (fun seed)
--this does not handle nothing, `case of` could be added and return [] on nothing
myUnfoldr::(b-> Maybe (a,b))->b->[a]
myUnfoldr fun seed = fst (fromJust result) : myUnfoldr fun (snd (fromJust result))
    where result = fun seed
--does not handle nothing
betterIterate::(a->a)->a->[a]
betterIterate fun seed = myUnfoldr (\b-> Just (b,fun b)) seed
--------------Binary Tree----------------------------------------
data BinaryTree a = 
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq,Ord,Show)

unfold'::(a->Maybe (a,b,a))->a->BinaryTree b
unfold' fun a = case fun a of
    Nothing->Leaf
    Just (x,y,z)-> Node (unfold' fun x) y (unfold' fun z)
treeBuild::Integer->BinaryTree Integer
treeBuild 0 = Leaf
treeBuild i = unfold' f 0
    where f x |x == i = Nothing
              |otherwise = Just (x+1,x,x+1)

