import Prelude hiding ((^))
double x = x+x;
quadruple x = double (double x);
factorial n = product [1..n];
average ns = sum ns `div` length ns;
-- comment
{-
 name cant be single letter>?
-}
nup = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5];
last2 ns = head (reverse ns);
last3 ns = take 1 (reverse ns)

init2 ns = take ((length ns)-1) ns;
init3 ns = reverse (drop 1 (reverse ns));

-- 4.8.1
--halve::[a] ->([a],[a])
--halve xs = (take n xs, drop n xs)
  --  where n  = length xs `div` 2;
--4.8.2
thirdHead xs = head (tail(tail xs));
thirdIndexing::[a]->a
thirdIndexing xs | (length xs) >2 = xs !! 2
thirdPattern (_:_:a:_) = a; 
--4.8.3
nulll::[a]->Bool
nulll xs | (length xs) > 0 = False 
        |otherwise = True;

safetailCond::[a]->[a]
safetailCond xs = if nulll xs then xs
else tail xs;

safetailGuard::[a]->[a]
safetailGuard xs | nulll xs = xs
    | otherwise = tail xs;

safetailPattern::[a]->[a]
safetailPattern [] = [];
safetailPattern (a:b) = tail (a:b);
--4.8.4
--sow how the disjunction operator || can be defined in four diffent ways
--TODO
--5.7.1
squares = [x^2 | x<-[1..100]];
--5.7.2
grid x y = [(a,b) | a<-[0..x], b<-[0..y]]
--5.7.3
square n = [(a,b) | (a,b)<-grid n n, a /= b]
--5.7.4
repl n val = [val | _<-[1..n]]
--5.7.5
pyth n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], a^2+b^2==c^2]
--5.7.6
factors num = [x | x<-[1..num], (num `mod` x) == 0] ;
perfects::Int->[Int]
perfects lim = [a | a<-[1..lim], sum (factors a) - a == a]
--5.7.7
build x ys = [(x,b)| b<-ys]
noDouble = concat [build a [3,4] | a<-[1,2]]
--5.7.8
positions x xs = [i | (x',i)<-zip xs [0..], x == x']
find k t = [v | (k',v)<-t, k==k']
positions2 x xs = find x (zip xs [0..])
--5.7.9
--TODO
--6.8.1
fac::Int->Int
fac 0 = 1
fac n | n>0 = n * fac (n-1)
 | otherwise = n
--6.8.2
sumdown::Int->Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)
--6.8.3
(^)::Int ->Int->Int
_ ^ 0 = 1
0 ^ _ = 0
a ^ b = a * (a^(b-1))
--2^3 => 2*(2*(2*(1)))
--6.8.4
euclid::Int->Int->Int
euclid 0 _ = 0
euclid _ 0 = 0
euclid a b | a==b = a
    | a < b = euclid a (b-a) 
    | a > b = euclid (a-b) b
    |otherwise = 666
--6.8.5
lengthh :: [a] -> Int
lengthh [] = 0
lengthh (_:xs) = 1 + lengthh xs
--lengthh [1,2,3] => 1 + (1 + (1 + (0)))

initt :: [a] -> [a]
initt [_] = []
initt (x:xs) = x : initt xs
--initt [1,2,3] => 1 : (2:([]))

dropp 0 xs = xs
dropp n [] = []
dropp n (x:xs) = drop (n-1) xs
--dropp 3 [1,2,3,4,5] => (((([]):3):2):1)
--6.8.6
-- a
akk::[Bool] -> Bool
akk [a] = a 
akk (x:xs) = x && and xs
--b 
conkat::[[a]]->[a]
conkat [a] = a
conkat (x:xs) = x ++ conkat xs
--c
replikate::Int->a->[a]
replikate 0 _ = []
replikate n c = [c] ++ replikate (n-1) c
--d
(!!!)::[a]->Int->a
(!!!) (x:_) 0 = x 
(!!!) (_:xs) idx = xs !!! (idx-1) 
--e
elemm::Eq a => a->[a]->Bool
elemm _ [] = False
elemm a (x:xs) | x == a = True
    |otherwise = elemm a xs
-- 6.8.7
halve::[a] ->([a],[a])
halve xs = (take n xs, drop n xs)
    where n  = length xs `div` 2;
merge::Ord a=>[a]->[a]->[a]
merge [] [] = []
merge (a:as) [] = a:as
merge [] (b:bs) = b:bs
merge (a:as) (b:bs) | a<=b = a:merge as (b:bs)
    |otherwise = b:merge (a:as) bs
--6.8.8
msort::Ord a=>[a]->[a]
msort [] = []
msort [a] = [a]
msort a = merge (msort b) (msort c) where
    (b,c) =  halve a
--6.8.9
--a
summ [] = 0
summ (x:xs) = x + sum xs
--b
takee :: (Eq t, Num t) => t -> [a] -> [a]
takee 0 _ = []
takee n (a:as) = a:takee (n-1) as
--c
lastt [a] = a
lastt (_:xs) = lastt xs
