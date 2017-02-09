import Data.Char
import Data.List 

-- @author Filip Saina

-- task 1
isWellFormed :: [[Int]] -> Bool
isWellFormed [[]] = False
isWellFormed m@(a:_) = and [ length a == length e | e <- m ]

listMean xs l = sum xs `div` l

-- another way to check if the matrix is well formed using the avg size
size :: [[Int]] -> (Int, Int)
size [[]]  = error "Matrix is malformed"
size matrix@(row:_) | length row == avgSizeRow = (avgSizeRow, length matrix)
                    | otherwise = error "Matrix is malformed"
                    where avgSizeRow = listMean [ length r  | r <- matrix] (length matrix)


getElement :: [[Int]] -> Int -> Int -> Int
getElement matrix@(row:_) r c | not (isWellFormed matrix) = error "Matrix is malformed"
                              | r >= mLen || c >= mRow  = error "Not valid index!"
                              | otherwise = matrix!!r!!c 
                              where mLen = length matrix
                                    mRow = length row

getRow :: [[Int]] -> Int -> [Int]
getRow xs@(row:_) n | not ( isWellFormed xs) = error "Matrix is malformed"
                    | n >= rows = error "Invalid row index!"
                    | otherwise = xs!!n
                    where rows = length xs

getCol :: [[Int]] -> Int -> [Int]
getCol xs@(row:_) n | not( isWellFormed xs ) = error "Matrix is malformed"
                    | n >= columns || n < 0 = error "Invalid row index!"
                    | otherwise = [ rw!!n | rw <- xs]
                    where columns = length row

addListElements :: [Int] -> [Int] -> [Int]
addListElements xs ys = [ xs!!x + ys!!x  | x <- [0..length xs -1]]

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices m1 m2 | not (isWellFormed m1) || not (isWellFormed m2) = error "Matrix is malformed"
                  | not (msize == size m2) = error "Matrices are not equal sizes!"
                  | otherwise = [ addListElements (getRow m1 x1) (getRow m2 x1)  | x1 <- [0..length m1 -1] ]
                  where msize = size m1

transpose' ::  [[Int]] -> [[Int]]
transpose' m  | not (isWellFormed m) = error "Matrix is malformed"
               | otherwise = [ getCol m r  | r <- [0..snd $ (size m) ] ]

multiplyList :: [Int] -> [Int] -> [Int]
multiplyList xs ys = [  xs!!x * ys!!x  |  x <- [0..length xs - 1]  ]

multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices m1@(roww: _) m2 | not (isWellFormed m1) || not (isWellFormed m2) = error "Matrix is malformed"
                             | otherwise = [  [ sum $ multiplyList (row) (getCol m2 col)   |  col <-[0..colNum-1]  ]  | row <- m1 ]
                            where colNum = length roww
                                  rowNum = length m1

-- 2nd task
type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value,Int)]

exists :: Key -> Dictionary -> Bool
exists i xs = or [ True | e <- xs, i == fst e]

get :: Dictionary -> Key -> Value 
get xs k | not $ exists k xs = error "Key does not exist"
         | otherwise = head [snd e  | e <- xs, fst e == k ]


insert :: Entry -> Dictionary -> Dictionary
insert entry@(key, val) d | exists key d = entry : delete' key d 
                          | otherwise = entry : d

-- I had a clash with the Data.List method delete
delete' :: Key -> Dictionary -> Dictionary
delete' k d = [e  | e <- d, not(fst e == k)]

freq :: Dictionary -> Frequency
freq [] = error "Empty dictionary"
freq d  = [(get d key, sum [ 1 |e <-d, fst e == key]) | key <- keys ] where keys = [ fst e |e <- d  ]


--- task 3
largestMultiple  :: String -> Int
largestMultiple xs | null searchList = error "No such number"
                   | otherwise = maximum searchList 
                     where permuts = permutations xs
                           searchList = [ read str :: Int  | str <- permuts, (read str :: Int) `mod` 30 == 0]  
