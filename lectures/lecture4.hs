-- 1.1
headHunter :: [[a]] -> a
headHunter (x:_) = head x
headHunter (_:x:_) = head x
headHunter (_:_:x:_) = head x
headHunter _         = error "Sorry, error!"

-- 1.2
firstColumn (x: _) = x
firstColumn xss = [ x | (x:_) <- xss]

-- 1.3
shoutOutLoud xs = concat [ take 100 $ repeat h  | (h:t) <-words xs ]

-- 2.1
pad (x:xs) (y:ys) | n < 0 = (x:xs, y:ys ++  take  (abs n) (repeat ' '))
                  | otherwise = (x:xs ++ take (abs n) (repeat ' '), y:ys)
                  where n = length ys - length xs
-- 2.2
-- It should work but it makes an error about '='
quartile xs | isNull xs = error "SorryEmptyList"
            | otherwise = (mean lis, mean lib, mean lic)
            where siz = length xs
                    m = floor (siz/4)
                    n = ceiling (2*siz/4)
                    t = floor 2*size/4
                    h = ceiling 3*siz/4
                  lis = fromIndex xs 0 m
                  lib = fromIndex xs m n
                  lic = fromIndex xs t h

-- 3.1
pad' (x:xs) (y:ys) = let n = length ys - length xs in if n < 0 then (x:xs, y:ys ++ take (abs n) (repeat ' ')) else (x:xs ++ take (abs n) (repeat ' '), y:ys)

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
 | odd l     = realToFrac $ ys !! h
 | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = xs

fromIndex xs n y = fst  (splitAt y (drop n xs)) 


--4.1
stringGen (a,b) (_:n:_) = "The pair " ++ numbOfOnes ++ " and the second element of the list is " ++ show n  where numbOfOnes | a == 1 && b == 1 = "contains two ones" | a == 1 || b == 1 = "contains one one" | otherwise = "does not contain a single one"
