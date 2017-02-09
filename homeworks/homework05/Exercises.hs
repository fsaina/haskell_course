import Prelude
import Data.Char
import Data.List
import Data.Function

-- @author Filip Saina

-- ### LECTURE 7 ###

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



-- ### LECTURE 8 ###

-- 1.1
sumEven :: (Num a) => [a] -> a
sumEven xs = sum . map (snd) $ filter (even . fst) (zip [0..] xs)

-- 1.2
filterWords :: [String] -> String -> String
filterWords ws s = unwords $ filter (\w -> not $ elem w ws) $ words s

-- 1.3
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p s = concat $ map ( (\c -> [c]++d) . toUpper . head ) $ filter p $ words s

-- 2.1
maxDiff :: [Integer] -> Integer
maxDiff xs = maximum .  map (uncurry (-)) . zip xs $ tail xs

-- 2.2
type Student = (String, Double)

studentsPassed :: [Student] -> [String]
studentsPassed = map (fst) . filter ((>=0.5) . snd)


-- 3.1
isTitleCased :: String -> Bool
isTitleCased = and . map (\c -> let m = head c in (==) m $ toUpper  m) . words

-- 3.2
sortGt (a, b) (c, d)
    | b > d = LT
    | b < d = GT
    | b == d = compare b d

sortPairs xs = sortBy sortGt xs
sortPairs' xs= sortBy (compare `on` snd) xs

-- 3.3
filename :: String -> String
filename xs = drop (n+1) xs
    where n = maximum $ findIndices (=='/') xs

-- 3.4
maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = []
maxElemIndices xs = findIndices (== maximal) xs
            where maximal = maximum xs

-- 4.1
elem' :: (Eq a) => a -> [a] -> Bool
elem' y = (==1) . foldr (\x _ -> if x == y then 1 else 0) 0 

-- 4.2
reverse' :: Eq a => [a] -> [a]
reverse' = foldr (\x y-> y++[x]) []

-- 4.3
nubRuns :: Eq a => [a] -> [a]
nubfunction x y | length y == 0 = False
                | x == head y = True
                | otherwise = False
nubRuns = foldr (\x y -> if nubfunction x y then y else x:y) []

-- 5.1
reverse'' :: (Eq a) => [a] -> [a]
reverse'' = foldl (\x y -> y:x) []

-- 5.2
sumEven' :: (Integral a) => [a] -> a
sumEven' = foldl (\a x -> if even x then a+x else a) 0

-- 5.3
maxUnzip :: [(Integer, Integer)] -> (Integer, Integer)
maxUnzip [] = error "empty list"
maxUnzip xs = (maxL, maxR)
    where maxL = maximum $ foldl (\a x -> fst x: a) [] xs
          maxR = maximum $  foldl (\a x -> snd x : a) [] xs

