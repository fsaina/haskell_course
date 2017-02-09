import Data.Char
import Data.List

-- @author Filip Saina
-- @jmbag 0036479300

-- === EXERCISE 1 ===============================================================
concat3 s1 s2 s3 | length s2 < 2 = s1 ++ s3 | otherwise = s1 ++ s2 ++ s3

showSalary amount bonus = if amount > 0 && bonus > 0 then show "Salary is " ++ show amount ++ ", and bonus is " ++ show bonus else if amount > 0 && bonus < 0 then show "Salary is " ++ show amount else show "Sorry, the amount cannot be negative"

-- ==============================================================================



-- === EXERCISE 1 ===============================================================
remove3 xs = reverse $ drop 3 $ reverse $ drop 3 xs
initials s1 s2 = s1 !! 0 : ". " ++ s2 !! 0 : "."
concatLong s1 s2 | length s1 >= length s2 = s1 ++ s2 | length s1 < length s2 = s2 ++ s1

safeHead xs = if null xs then [] else [xs !! 0]

hasDuplicates xs = not $ xs == nub xs
-- ==============================================================================



-- === EXERCISE 2 ===============================================================

-- doublesFromTo is not defined 

ceasarCode :: Int -> String -> String
ceasarCode n xs = [chr ((ord $ toLower x) +n) | x <-xs, (ord $ toLower x) + n >= ord 'a' && (ord $ toLower x) + n <= ord 'z']

-- ==============================================================================



-- === EXERCISE 3 ===============================================================

lengths xss = [length xs | xs <- xss]
totalLength xss = sum $ lengths xss
letterCount xs = totalLength [show x | x <- words xs , length x >= 3]

lowerString xs = [ toLower x | x<- xs]
isPalidrome xs = lowerString xs == (lowerString $ reverse xs)

flipp xss = concat $ reverse [reverse x  | x <- xss]

-- ==============================================================================



-- === EXERCISE 4 ===============================================================
inCircle :: Int -> Int -> Int -> [(Int, Int)] 
inCircle r x y = [ (a+x, b+y) | a <- [-10..10], b <- [-10..10], (a+x)^2 + (b+y)^2 <= r^2]

inCircleN r x y n = [ ((a+x)*n, (b+y)*n) | a <- [-10..10], b <- [-10..10], ((a+x)*n)^2 + ((b+y)*n)^2 <= r^2]

steps xs = [ (xs !! fst x, xs !! snd x) | x <- zip [0..] [1..length xs -1]]

-- ==============================================================================



-- === EXERCISE 5 ===============================================================

indices :: Char -> String -> [Int]
indices x xs = [ fst ix | ix <- zip [0..] xs, snd ix == x]

showLineNumbers :: String -> String
showLineNumbers s = concat [show (fst x) ++ (snd x) | x <- zip [1..] (lines s)]

common xs ys = [ snd x  | x<- zip [0..] xs, y <- zip [0..] ys, fst x == fst y && snd x == snd y]
haveAlignment xs ys = not $ null $ common xs ys

-- ==============================================================================
