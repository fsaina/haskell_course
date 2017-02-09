/import Debug.Trace

-- @author Filip Saina

type RomanNumeral = String

resolveChar :: Char -> Int
resolveChar c | c == 'I' = 1
              | c == 'V' = 5
              | c == 'X' = 10
              | c == 'L' = 50
              | c == 'C' = 100
              | c == 'D' = 500
              | c == 'M' = 1000
              | c == '.' = 0
              | c == ',' = 3999
              | c == 'N' = error "Zero here!"
              | otherwise = error "Invalid roman character"

resolveNum n | n == 1 = 'I'
             | n == 5 = 'V'
             | n == 10 = 'X'
             | n == 50 = 'L'
             | n == 100 = 'C'
             | n == 500 = 'D'
             | n == 1000 = 'M'
             | otherwise = '.'

isValidLetter :: Char -> Bool
isValidLetter c | c == 'I' || c == 'V' || c == 'X' || c == 'L' || c == 'C' || c == 'D' || c == 'M' = True
                | otherwise = False

numberOfOccurances :: String -> Char -> Int
numberOfOccurances xs c = length [x | x <- xs , x == c ]

repetition :: String -> Bool
repetition xs = not $ null [x |  x <- xs , numberOfOccurances xs x > 3]

repetitionVLD :: String -> Bool 
repetitionVLD xs = numberOfOccurances xs 'V' > 1 || numberOfOccurances xs 'L' > 1  || numberOfOccurances xs 'D' > 1

isAdditive ys = and $ additiveCon ys 'M'
            where additiveCon [] _ = []
                  additiveCon (x:xs) p | resolveChar p >= resolveChar x = True:additiveCon xs x
                                       | otherwise = False:additiveCon xs x


-- 1 a,  p - previous char, pp - pre-previous char, m - maximum current valid roman number
isValidRoman :: RomanNumeral -> Bool 
isValidRoman ys | repetition ys = False
                | repetitionVLD ys = False
                | otherwise = and $ validCon ys ',' ',' (resolveChar ',')
            where validCon [] _ _ _ = []
                  validCon (x:xs) p pp m      | resolveChar pp <= resolveChar x = [False]
                                              | and [resolveChar p < resolveChar x, m >= (diff p x), isAtLeastOneTenth p x, isIXC p] =  (True:validCon xs x p (diff p x) )
                                              | resolveChar p>= resolveChar x= True:validCon xs x pp (resolveChar p) 
                                              | otherwise = [False]
                                             where isAtLeastOneTenth m n = resolveChar m >= ( resolveChar n `div` 10) 
                                                   isIXC c = c == 'I' || c == 'X' || c == 'C'
                                                   diff a b = resolveChar b - resolveChar a
 -- 1 b
biggestMatchRomanNum n | not (resolveNum n == '.') = resolveNum n
                       | otherwise = biggestMatchRomanNum (n-1)

toRoman :: Int -> RomanNumeral
toRoman n | n <= 0 = error "Number cannot be represented"
          | isValidRoman rez = rez
          | otherwise = trace (show rez) error "Number cannot be represented"
            where calcRoman 0 = [] 
                  calcRoman 1 = ['I']
                  calcRoman n =  biggestMatch:(calcRoman (n - resolveChar biggestMatch)) 
                               where biggestMatch = biggestMatchRomanNum n
                  rez = calcRoman n

-- 1 c
fromRoman:: RomanNumeral -> Int 
fromRoman ys | not (isValidRoman ys) = error "Not a valid roman number" 
             | otherwise = sum $ validCon ys ',' ',' (resolveChar ',')
            where validCon [] _ _ _ = []
                  validCon (x:xs) p pp m | and [resolveChar p < resolveChar x, m >= diffVal] =  (diffVal:validCon xs x p diffVal )
                                         | otherwise = (resolveChar x):validCon xs x pp (resolveChar p) 
                                         where diff a b = resolveChar b - resolveChar a
                                               diffVal = diff p x

-- 2 - this task solved two ways, second one is correct
-- greedy algorithm, always look for the best next destination form the current position
shortestDistance' :: (Int, Int) -> [(Int, Int)] -> Int
shortestDistance' p l = traversee( instructions p l ) 

traversee [] = 0
traversee (_:[]) = 0
traversee (x:y:[]) = manhattanDist x y
traversee (x:y:xs) = manhattanDist x y + traversee (y:xs) 
instructions p l = p : (reverse $ drop 1 $ reverse $ shortest (p:l) ++ [p])

-- generate a path based on always looking for the shortest solution
shortest [] = []
shortest [x] = [x]
shortest l@(p:xs) = xs!!shortestIndex : shortest (p:removeListIndex)
                  where shortestIndex = shortestManhattan p xs 
                        removeListIndex = removeAt xs shortestIndex

-- removes element at a index from a list
removeAt [] _ = []
removeAt l@(x:xs) n | n == 0 = removeAt xs (n-1)
                    | otherwise = x: removeAt xs (n-1)

-- calculates the manhattan distance
manhattanDist l1@(s, ss) l2@(d, dd) = abs (d - s) + abs (dd - ss)

-- from a point and a list of points - find the nearest solution
-- I know it is suboptimal 
shortestManhattan :: (Int, Int) -> [(Int, Int)] -> Int
shortestManhattan p l@(x : xs) = shortest p l x 0 0
                    where shortest _ [] _ i _ = i
                          shortest p (x : xs) m i c | manhattanDist p x <= manhattanDist p m = shortest p xs x c (c+1)
                                                    | otherwise = shortest p xs m i (c+1) 

-- BUT WAIT !   ...a better way there is master Luke *yoda voice*
-- TODO this is the real shortestDistance 
shortestDistance :: (Int, Int) -> [(Int, Int)] -> Int
shortestDistance p xs = calcShortest xs [] p
                      where calcShortest [] ts p  = traversee (p:ts ++ [p])
                            calcShortest xs ts p = minimum [ calcShortest (removeAt xs i) (ts ++ [s]) p | (i, s) <-[0..length xs - 1] `zip` xs ]

-- 3
type Probability = Double
type DiscreteRandVal = [(Int, Probability)]
x :: DiscreteRandVal
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- 3 a
mean :: DiscreteRandVal -> Double
mean [] = 0.0
mean ((a,b):xs) = (fromIntegral a * b) + mean xs 

mean' :: DiscreteRandVal -> Double
mean' ys = calculateMean ys 0 
      where calculateMean [] m = m
            calculateMean ((a,b):xs) m = calculateMean xs (m + fromIntegral a * b) 

-- 3 b
variance :: DiscreteRandVal -> Double
variance ys = calcVariance ys mi
            where mi = mean' ys
                  calcVariance [] _ = 0.0 
                  calcVariance ((i, p):xs) mi = ((fromIntegral i - mi) ** 2) * p + calcVariance xs mi

variance' :: DiscreteRandVal -> Double
variance' ys = cVar ys mi 0
             where mi = mean' ys
                   cVar [] _ m = m
                   cVar ((i,p):xs) mi m = cVar xs mi (m + (fromIntegral i - mi) ** 2 * p)

-- 3 c
probabilityFilter :: Probability -> DiscreteRandVal -> [Int]
probabilityFilter _ [] = []
probabilityFilter t ((i, p):xs) | p >= t = i : probabilityFilter t xs
                                | otherwise = probabilityFilter t xs

probabilityFilter' :: Probability -> DiscreteRandVal -> [Int]
probabilityFilter' t ys = probFilter t ys []
                    where probFilter _ [] ys = ys
                          probFilter t ((i, p):xs) ys | p >= t = probFilter t xs (ys ++ [i])
                                                      | otherwise = probFilter t xs ys
