import Data.List

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
personId2 :: String,
forename2 :: String,
surname2  :: String,
sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
mother2   :: Maybe Person2,
father2   :: Maybe Person2,
partner2  :: Maybe Person2,
children2 :: [Person2] } deriving (Show,Read, Eq)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]

-- 1.1
resolve (Just n) = n

-- 1.2
parentCheck :: Person2 -> Bool
parentCheck p | father2 p == Nothing = False
              | otherwise = (elem) p $ children2 $ resolve $ father2 p

-- 1.3
sister :: Person2 -> Maybe Person2
sister p = case mother2 p of
    Just m -> let children = children2 m in Just $ head $ filter ((== Female) . sex2) children
    Nothing -> Nothing

-- 1.4
descendant :: Person2 -> [Person2]
descendant p = foldr (\prsn a -> a ++ descendant prsn) [] (children2 p)


data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

infixr 5 -+-
(-+-) = Cons

-- 2.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons a l) = Just a

-- 2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty = Empty
listMap f (Cons a l) = Cons (f a) (listMap f l)

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

-- 3.1
treeMax :: Ord a => Tree a -> a
treeMax Null               = error "The tree is empty"
treeMax (Node a Null Null) = a
treeMax (Node a Null rs)   = maximum [a, treeMax rs]
treeMax (Node a ls Null)   = maximum [a, treeMax ls]
treeMax (Node a ls rs)     = maximum [a, treeMax ls, treeMax rs]

-- 3.2
treeToList :: Ord a => Tree a -> [a]
treeToList t = ttl t []
  where ttl Null l = l
        ttl (Node a left right) l = ttl left [] ++ [a] ++ ttl right []

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node a left right)
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)
  | otherwise = t

-- 4.1
listToTree :: Ord a => [a] -> Tree a
listToTree l= foldr (\x t -> treeInsert x t) Null l

-- 4.2
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree . nub

-- EX 5.1
data Weekday =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show,Enum)

-- 5.1
instance Eq Weekday where
    w1 == w2 | w1 == Friday && w2 == Friday = False
            | otherwise                    = w1 == w2

data Person = Person {
    idNumber :: String,
    forename :: String,
    surname  :: String,
    sex      :: Sex,
    age      :: Int,
    partner  :: Maybe Person,
    children :: [Person] } deriving (Read,Eq,Ord)

p3 = read $ "Person {idNumber=\"111\",forename=\"Ivo\",surname=\"Ivic\"," ++
             "sex=Male,age=11,partner=Nothing,children=[]}" :: Person
-- 5.2
instance Show Person where
  show (Person _ f s _ _ _ _ ) = f ++ " " ++ s

-- 6.1
instance (Eq a) => Eq (MyList a) where
  Empty == _ = False
  _ == Empty = False
  (Cons a l) == (Cons b lw) = a == b

-- 6.2
instance (Eq a, Ord a) => Eq (Tree a) where
  Null == Null = True
  Null == _ = False
  _ == Null = False
  t == t2 = sortedT t == sortedT t2
        where sortedT = sortAndNub . treeToList
