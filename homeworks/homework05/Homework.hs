import Data.List

-- @author Filip Saina

type Position = (Integer, Integer)
data Orientation = West | East | North | South deriving (Eq, Show)

data TurnDir = CW | CCW deriving (Eq, Show)

-- 1
-- a, b, c
data Turtle = Turtle { position :: Position, orientation :: Orientation } deriving Show

-- d
newTurtle :: Turtle
newTurtle = Turtle (0,0) (North)

-- e
move :: Integer -> Turtle -> Turtle
move n t  | n < 0 = error "Turtles cannot move backwords"
          | otherwise = let o = orientation t in case o of 
                            North -> Turtle (p1, y2+n) o
                            East -> Turtle (p1+n, y2) o
                            South -> Turtle (p1, y2-n) o
                            West -> Turtle (p1-n, y2) o
               where p1 = fst $ position t
                     y2 = snd $ position t

-- f
turn :: TurnDir -> Turtle -> Turtle 
turn d t = case d of
           CW -> setOrientation t (succL (orientation t) list)
           CCW -> setOrientation t (succL (orientation t) (reverse list))
        where succL p xs = let i = justVal (findIndex (==p) list) in list!!((succ i) `mod` (length list)) 
              setOrientation t oo = let p = position t in t { position = p, orientation = oo}
              list = [North, East, South, West]
              justVal (Just n) = n

-- g
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle xs t = foldr (\f t -> f t) t (reverse xs)


-- 2
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)

testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- a
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter p (Node v left right)
    | (p v) /= True = Leaf
    | otherwise = Node v (treeFilter p left) (treeFilter p right)

-- b
levelMap :: (Integer -> a -> b) -> Tree a -> Tree b
levelMap f t = mapLevel 0 f t
        where mapLevel _ _ Leaf = Leaf
              mapLevel d f (Node v left right) = let s = succ d in Node (f d v) (mapLevel s f left) (mapLevel s f right)

-- c
-- Could you clarify why this causes trouble ?
--isSubtree :: Eq a => Tree a -> Tree a -> Bool 
isSubtree Leaf Leaf = True
isSubtree (Node _ _ _) Leaf = False
isSubtree t t1@(Node v left right)
    | subtreeLevel t1 == testTree = True 
    | subtreeLevel left == testTree = True
    | subtreeLevel right == testTree = True
    | otherwise = isSubtree t left || isSubtree t right
    where subtreeLevel t = levelMap (\d v -> d + v) t
          testTree = subtreeLevel t

-- 3
data Pred = And {
    ex1      :: Pred,
    ex2      :: Pred 
    }
    | Or {
    ex1      :: Pred,
    ex2      :: Pred 
    } 
    | Not {
    ex1      :: Pred
    } 
    | Val {
    boolVal :: Bool
    } deriving (Show)

eval :: Pred -> Bool
eval (Val v) = v
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Not a) = not $ eval a


-- 4
data Track = Track {
    trackTitle :: String,
    trackNo :: String,
    albumName :: String } deriving (Eq)

instance Ord Track where
    t1 <= t2 = trackNo t1 <= trackNo t2

instance Show Track where
    show (Track t n an) = (t ++ " " ++ n ++ " " ++ an)

trackFromStr :: String -> Maybe Track
trackFromStr xs 
    | length split <= 2 = Nothing
    | otherwise = Just Track {trackTitle = split!!0, trackNo = split!!1, albumName = unwords $ drop 2 split}
    where split = words xs

-- a
sortTracks :: [String] -> [String]
sortTracks = map (show). sort . map (\(Just t) -> t) . map (trackFromStr)

-- b
numberOfPlays :: [String] -> Integer
numberOfPlays = foldr (\s tsum -> (+) tsum $ firstSplit s) 0
              where firstSplit s = read $ (!!0) $ words s :: Integer
