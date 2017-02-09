import Prelude
import Data.Char

-- @author Filip Saina

-- EXERCISE 1
-- 1.1
takeThree :: [Int] -> [Int]
takeThree = (3 `take`)

dropThree :: [Int] -> [Int]
dropThree = (3 `drop`)

hundretTimes = replicate 100

-- 1.2
index = zip [0..]
index' = (`zip` [0..]) 


-- 1.3
divider :: Int -> [Char]
divider = (`take` repeat '=')

-- EX2
-- 2.1
applyOnLast f xs ys = (last xs) `f` (last ys)  

addThree :: Num a => a -> a -> a -> a   -- or: a -> (a -> (a -> a))
addThree x y z = x + y + z

lastTwoPlus100 xs ys = addThree (applyOnLast (+) xs ys) 100 0

-- 2.2
applyManyTimes n f x | n <= 0 = x
                     | otherwise = apply n f x n
                    where apply _ _ x 1 = f x
                          apply n f x i = ( f x ) + apply n f x (i-1) 

applyTwice f = applyManyTimes 2 f

-- EX3
-- 3.1
lustifyList :: [a] -> [[a]]
lustifyList = map (:[])

-- 3.2
cutoff n xs = map mx xs
   where mx a = min n a

-- EX4
-- 4.1
sumEvenSquares xs = sum $ map (^2) $ filter (even) xs

-- 4.2
freq x xs = length $ filter (== x) xs

-- 4.3
freqFilter n xs = map (\(a,b) -> a) $ filter (\(a,b) -> b >= n) $ [ (x, freq x xs) | x <- xs]


-- EX 5
--5.1
isWithin n m = \x -> not $ and(x > n, x < m+1)
withinInterval n m xs = filter (isWithin n m) xs

--5.2
sndColumn xs = map (\xs -> xs!!1) xs

--5.3
canonicalPairs :: Ord a => [(a,a)] -> [(a,a)]
canonicalPairs ls = map (\(a,b) -> if(a < b) then (a,b) else (b,a)) $ filter (\(a,b) -> not(a == b)) ls
