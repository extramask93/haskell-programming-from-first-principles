--7.9.1
--[f x | x<-xs, p x]
--map f (filter p xs)
--7.9.2
--a
alll p xs= and (map p xs)
--b
anyy p xs = or (map p xs)
--c
takeWhilee::(a->Bool)->[a]->[a]
takeWhilee _ [] = []
takeWhilee p (x:xs) = if p x then x:takeWhilee p xs else [] 
--d
dropWhilee::(a->Bool)->[a]->[a]
dropWhilee _ [] = []
dropWhilee p (x:xs) = if p x then dropWhilee p xs else x:xs
--7.9.3
mapp f xs= foldr (\x xs-> f x:xs) []
filterr f xs = foldr (\x xs -> if f x then x:xs else xs) [] xs
--7.9.4
powers::Int->[Int]
powers 0 = [1]
powers a = (10^a):powers (a-1)
--dec2int::[Int]->Int
--w/o understanding of foldr/foldl
zz xs = foldl (+) 0 (map (\(a,b)->a*b) (zip (powers (length xs - 1)) xs))
--7.9.5
curryy::((a,b)->c)->a->b->c
curryy fun a b = fun (a,b)
uncurryy::(a->b->c)->((a,b)->c)
uncurryy fun (a,b) = fun a b
--7.9.6
unfold p h t x | p x = []
    |otherwise = h x : unfold p h t (t x)
type Bit = Int
int2bin = unfold (== 0) (`mod` 2) (`div` 2)
chop8::[Bit]->[[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)
mappp :: Eq b => (b -> a) -> [b] -> [a]
mappp fun = unfold (== []) (fun.head) tail 
iteratee f = unfold (const False) f f 
-- 7.9.7
data Mood = Wood | Blah deriving Show
changeMood::Mood->Mood
changeMood Wood = Blah
changeMood _ = Wood