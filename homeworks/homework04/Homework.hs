import Data.Char
import Data.List

-- @author Filip Saina
:
-- 1.1
intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs []  = []
intercalate' xs yss = intercal xs yss
    where intercal _ [[a]] = [a]
          intercal xs (ys:yss) = ys ++ xs ++ (intercal xs yss)

-- 2a
chunk :: Int -> [a] -> [[a]]
chunk 0 _  = []
chunk n xs = chunxs n xs
    where chunxs n xs | n < length xs = [take n xs] ++ chunxs n (drop n xs)
                      | otherwise = [xs]
-- 2b
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy [] _ = []
chunkBy lx@(x:xs) ys | x < 1   = chunkBy xs ys
                     | null lx = [ys]
                     | otherwise = [take x ys] ++ chunkBy xs (drop x ys)
-- 2c
chunkInto :: Int -> [a] -> [[a]]
chunkInto n xs = chunkf n xs ( (length xs) `div` n)
      where chunkf _ [] _ = []
            chunkf 0 _ _ = []
            chunkf n xs 0 = (( take 1 xs): (chunkf 1 (drop 1 xs) 0) )
            chunkf n xs m | n == 0 = []
                          | l == 0 = []
                          | r  < 2  = ( (take l xs) :(chunkf l (drop l xs) m))
                          | otherwise = ((take m xs) :(chunkf n (drop m xs) m))
                          where l  = length xs
                                r  = length xs `div` m

-- 3
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap fx ys = cycleMapRunner fx ys 0
cycleMapRunner [] _ _ = []
cycleMapRunner _ [] _ = []
cycleMapRunner fx (y:ys) n = [((fx!!functionMod) y)] ++ cycleMapRunner fx ys (n+1)
            where functionMod = n `mod` (length fx) 

-- 4a
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ s [] = s
reduce f s (x:xs) = reduce f res xs 
            where res = (s `f` x)
-- 4b
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f [] = error "reduce1 got an empty list" 
reduce1 f (x:xs) = reduce f x xs

-- 4c
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan f s xs = reverse $ scanIm f s xs [s]
scanIm _ s [] xs = xs
scanIm f s (x:xs) l = scanIm f res xs (res:l)
            where res = (s `f` x)

-- 4d
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f x [] = x
rreduce f x ys = rreduce f (f (last ys) x) (init ys)

-- 4e
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 f [] = error "rreduce1 got an empty list"
rreduce1 f ys = rreduce f (last ys) (init ys)

-- 4f
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan f x [] = reverse [x]
rscan f x ys =reverse $ scan function x (reverse ys)
      where function a b = f b a

-- 5a
type Tolerance = Double

newton :: Tolerance -> Double -> Double
newton t n | n < 0 = error "Cant get sqrt from a negative number"
           | otherwise = nwt t n (n/2)  -- start with x = 0
           where nwt t l h | abs (h - calc) < t = calc
                           | otherwise = nwt t l calc
                           where calc = (h + l/h) /2

-- 5b
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx
         where dx = 1e-5

-- 6
type Operators = [(Char, Int -> Int -> Int)]

basic :: Operators
basic = [('+', (+)), ('-', (-))]

standard :: Operators
standard = [('+', (+)),
             ('-', (-)),
             ('*', (*)),
             ('/', (div)),
             ('^', (^))]

errorMessage = "Invalid rpn expression"
executeOp f xs
          | l < 2     = error errorMessage
          | otherwise = ( f oprtr2 oprtr1):stack
          where l = length xs
                oprtr1 = xs !! 0
                oprtr2 = xs !! 1
                stack = drop 2 xs

rpnCalc :: String -> Operators -> Int
rpnCalc s opr = calculate s opr []
            where calculate (x:xs) op rez | x `elem` "0123456789" = calculate xs op ((digitToInt x):rez)
                                          | not (x `elem` valid) = error ("Invalid symbol " ++ [x])
                                          | otherwise = calculate xs op (executeOp f rez)
                                          where valid = map fst op
                                                f = snd $ head $ (filter (\(z, y) -> (z == x)) op)

                  calculate [] _ rez  | (length rez ) > 1 = error errorMessage
                                      | (length rez)  == 0 = 0
