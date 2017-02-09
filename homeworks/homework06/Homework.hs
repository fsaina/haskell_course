import CSVUtils
import Data.Set

-- 2
type Decision = String
caseA = ["Raining", "Yes", "Workday"]
caseB = ["Sunny", "No", "Workday"]

nbDecide :: CSV -> [String] -> Decision
nbDecide c xs = let p0 = product $ Prelude.map (pConditionC0) xs
                    p1 = product $ Prelude.map (pConditionC1) xs in
                    if p0*pc0 > p1*pc1
                    then "No"
                    else "Yes"
        where pC0 = filterColIfEq 3 "No"
              pC1 = filterColIfEq 3 "Yes"
              pc0 = size pC0 `div` (length $ colFields 0 c)
              pc1 = size pC1 `div` (length $ colFields 0 c)
              pConditionC0 xs= twoConditionProb (resolve xs) xs pC0
              pConditionC1 xs= twoConditionProb (resolve xs) xs pC1
              twoConditionProb i v p = size (intersection (filterColIfEq i v) (p)) `div` (size p)
              filterColIfEq i p = fromList $ Prelude.filter ((==)  p) $ colFields i c
              resolve xs
                | elem xs ["Sunny","Raining", "Cloudy"] = 0
                | elem xs ["Yes", "No"] = 1
                | elem xs ["Weekend", "Workday"] = 2
                | otherwise = error "Value not recognized!"


nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll c d = Prelude.map (nbDecide c) d

-- 3
class Truthy a where
    truey :: a -> Bool
    falsey :: a -> Bool
    truey m = not $ falsey m
    falsey n = not $ truey n

instance Truthy Int where
    truey 0 = False
    truey _ = True

instance Truthy Bool where
    truey True = True
    truey False = False

instance Truthy [a] where
    truey [] = False
    truey _  = True

-- a
if' :: Truthy p => p -> a -> a -> a
if' p t e | truey p = t
          | otherwise = e

-- b
assert :: Truthy p => p -> a -> a
assert p m | truey p = m
           | otherwise = error "Assertion failed"

-- c
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) a b = and (truey a, truey b)

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) a b = or (truey a, truey b)

-- 4
-- Did not solve

-- 5
data MyList a = Cons a (MyList a) | Nil deriving Show

-- a
instance Functor MyList where
    fmap f a = mapIt f a
        where mapIt f (Cons a Nil) = Cons (f a) Nil
              mapIt f (Cons a l) = Cons (f a) (mapIt f l)


-- b
(+|+) :: (MyList a) -> (MyList a) -> (MyList a)
(+|+) Nil l2 = l2
(+|+) l1 Nil = l1
(+|+) (Cons val other) l = Cons val ((+|+) other l)

instance Applicative MyList where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f l) <*> something = (fmap f something) +|+ (l <*> something)

-- c
instance Monad MyList where
    (>>=) Nil _ = Nil
    (>>=) (Cons a l) f = (f a) +|+ (l >>= f)
