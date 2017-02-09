import Prelude hiding (map)
import Data.Char

number x y = x*10 + y
fifty = number 5

is42 = (==42)
--is42` = (42==) --the same thing, the ordering does not matter(only logically, not sintax)

-- EXERCISE 1

takeThree :: [Int] -> [Int]
takeThree = (3 `take`)

dropThree :: [Int] -> [Int]
dropThree = (3 `drop`)

--hundretTimes :: a -> [a]
--hundretTimes = 100 `take` (repeat)
hundretTimes = replicate 100

index = zip [0..]
index' = (`zip` [0..]) 

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

addThree x y z  = x+ y + z
-- EX2
--applyOnLast f xs ys = last $ xs `f` last $ ys  

--lastTwoPlus100 xs ys =addThree( (applyOnLast add xs ys) 100 0)

--applyManyTimes n f x = 
--
--
--IMPORTANT Map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- EX3
lustifyList :: [a] -> [[a]]
lustifyList = map (:[])

cutoff n xs = map mx xs
   where mx a = min n a

digits = filter (`elem` ['0' .. '9'])

caesarCode s = map succ $ filter (/= ' ') s

-- TODO fix this
--freq x xs = sum $ map (head) $ filter (== x) xs

sumEvenSquares xs = sum $ map (^2) $ filter (even) xs

-- we define a anonymous function if we are going to call it once

--lambda expression
addThree' = \x y z -> x + y+ z

isWithin n m = \x -> not $ and(x > n, x < m+1)
withinInterval n m xs = filter (isWithin n m) xs




