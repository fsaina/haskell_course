-- Haskell lecture 5

-- @author Filip Saina

import Debug.Trace

-- Exc 1
-- 1.1
prod [] = 1 
prod (x:xs) = x * prod xs

-- 1.2
headsOf ([]) = []
headsOf ((x:_):y) = x : headsOf y

incIncList' xs = inc 0 xs
    where inc _ [] = []
          inc n (x:xs) = x + n : inc (n+1) xs


-- 2.1
modMult _ _ [x] = [] 
modMult n m (x:xs) = x * n `mod` m : modMult n m xs

-- 2.2
addPredecessor [] = []
addPredecessor ys = addP 0 ys
                  where addP _ [] = []
                        addP y (x:xs) = x + y : addP x xs   

-- 3.1
equalTriplets [] = [] 
equalTriplets ((x, y, z): xs) | x == y  && y == z && x == z = (x, x, x) : equalTriplets xs
                              | otherwise = equalTriplets xs

-- 3.2
replicate' :: Int -> a -> [a]
replicate' c n | c == 0 = [] | otherwise =  n : replicate' (c-1) n

take' :: Int -> [a] -> [a]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- 4.1
drop' :: Int -> [a] -> [a]
drop' n (x:xs) | n == 0 = x:xs
                   | otherwise = drop' (n-1) xs

drop'' n xs | n > 0 = drop' n xs
            | n < 0 = reverse $ drop' (abs (n)) (reverse xs)
            | n == 0 = xs

-- 4.2 -- for some reason, the variable n is always set to 0
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo m n2 xs
   | m > n2 = error "Wrong intervals provided"
   | otherwise = fromToRec m n2 xs 0
                where fromToRec _ _ [] _ = []
                      fromToRec n b (x:xs) i  
                        | and(i >= n, i <= b) = x:fromToRec n b xs (i+1)
                        | otherwise = fromToRec n b xs (i+1)

-- 5.1
eachThird (a:b:c:xs) = c: eachThird xs
eachThird _ = []

-- 5.2
crossZip :: [a] -> [a] -> [(a,a)]
crossZip [] [] = []
crossZip (x:[]) (y:[]) = []
crossZip (x1:x2:xs) (y1:y2:ys) = [(x1,y2), (x2, y1)] ++ crossZip xs ys 

-- 6.1
length' xs = lenList xs 0
          where lenList [] x = x
                lenList (x:xs) i = lenList xs (i+1)

-- 6.2
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip xs 
   | null xs = error "Empty List"
   | otherwise =  maxVal xs (xs!!0)
            where maxVal [] p = p
                  maxVal ((x1,x2):xs) (a,b) 
                    | x1 > a && x2 > b  = maxVal xs (x1, x2)
                    | x1 > a && x2 <= b = maxVal xs (x1, b)
                    | x1 <= a && x2 > b = maxVal xs (a, x2)
                    | otherwise = maxVal xs (a,b)

-- 6.2 no accumulator
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' xs 
   | null xs = error "Empty List"
   | otherwise =  (maximum $ maxL xs,maximum $ maxR xs)
            where maxL [(x1,x2)] = [x1]
                  maxL ((x1, x2):xs) = x1:(maxL xs)
                  maxR [(x1,x2)] = [x2]
                  maxR ((x1, x2):xs) = x2:(maxR xs)
                   

