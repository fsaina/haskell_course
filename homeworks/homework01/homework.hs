import Data.Char(digitToInt)

-- @author: Filip Saina
-- @jmbag: 0036479300

type Vector  = (Double, Double)

-- Vector norm function
norm :: Vector -> Double
norm vector = sqrt (fst vector * fst vector + snd vector * snd vector)

-- Vector normalization function 
normalize :: Vector -> Vector
normalize v | not(norm v == 0) = (fst v / norm v, snd v / norm v) 
            | otherwise = error "Cannot normalize null vector"

-- Scalar multiplt 
scalarMult :: Double -> Vector -> Vector
scalarMult num v = (num * fst v, num * snd v)

-- Dot multiply
dot :: Vector -> Vector -> Double
dot v1 v2 = fst v1 * fst v2 + snd v1 * snd v2

-- cos prime function
cos' :: Vector -> Vector -> Double
cos' v1 v2 | not(norm v1 == 0 || norm v2 == 0) = v1 `dot` v2 / (norm v1 * norm v2) 
           | otherwise = error "Null vector given"

-- parallel vector
--  >> should I use magic numbers like this ? 
areParallel :: Vector -> Vector -> Bool
areParallel v1 v2 = cos' v1 v2 > 0.99

--  List splitting
splitAt' n xs | n > length xs || n < 0 = error "n is out of range" 
              | otherwise = ([ snd x | x <- zip [1..length xs] xs, fst x <= n], [snd x | x <- zip [1.. length xs] xs , fst x > n])

--  again, list splitting 
-- >> here we go! Less dragons..
firstXS n xs = reverse (drop n (reverse xs))
lastXS n xs = drop (length xs - n) xs
splitAt'' n xs | n > length xs || n < 0 = error "n is out of range" | otherwise = (firstXS n xs, lastXS n xs)


-- the algorithm
-- reverse elements (so the last element wont be doubled if its a even list size), provide them indexes, filter every
-- even element, and reverse all together
doubleSecond xs = reverse [ if even (fst x) then  2 * snd x else snd x | x <- zip [1..length xs] (reverse xs)]

-- sumation
-- >> Yeah .. sorry about that
numStringToDigits xs = [digitToInt x | x <- xs]
numbersSplit xs = [numStringToDigits (show  x) | x <- xs ]
sumDigitsInList xs = [ sum x | x <- xs ]

sumList :: [Int] -> Int
sumList xs = sum (sumDigitsInList (numbersSplit xs))

stringToIntegers xs = [ digitToInt x | x <- xs]
-- remainger
luhn :: String -> Bool
luhn xs =(sumList (doubleSecond (stringToIntegers xs)) `mod` 10) == 0
