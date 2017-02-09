import Data.List

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date x y z) = show(x)++"."++show(y)++"."++show(z)

data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x2 y2) m) = Circle2 (Point (x+x2) (y+y2)) m
-- TODO the second

data Level = Bachelors | PhD | Masters  deriving (Show, Eq)

data Student = Student
    {firstName :: String
    ,lastName :: String
    ,studentId :: String
    ,level :: Level
    ,avgGrade :: Double } deriving Show

-- ex2
improveStudent :: Student -> Student
improveStudent s | avgGrade s +1.0<= 5.0 =  s {avgGrade = avgGrade s+1.0}
                 | otherwise = s {avgGrade = 5.0}

--calcAvgFroLevel l xs= (sum $ map (avgGrade) $ fil xs) / num
--                    where num = length $ len fil
--                          fil = filter (\s -> level s == l)

--avgGradePerLevels :: [Student] -> (Double, Double, Double)
--avgGradePerLevels xs = (calcAvgFroLevel Bachelors xs, calcAvgFroLevel PhD xs, calcAvgFroLevel Masters xs)


--
--ex3
--data MyTriplet a b c= (h) deriving (Show, Eq)

