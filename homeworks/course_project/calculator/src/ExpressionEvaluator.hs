-- | Expression evaluator implementation
-- Contains code for parsing, modeling and evaluating mathematical
-- expressions provided to the functions.
--
-- Implementation translates expressions into the RPN which in
-- turn is used for constructing a evaluation tree data structure.
--
-- Current implementation works on certain restrictions:
-- - only single charater variable names
-- - no decimal point values
-- - no multi digit numbers

module ExpressionEvaluator(
    Tree
  , createExpression
  , substitute
  , deriveBy
  , evaluate
)where

import qualified ShuntingYarn as SY
import Data.Char

-- | Function enum abstraction containing alltypes of functions
-- An Function in this context is defined as a operation that takes
-- one element and produces one
data Function = Cosine | Sine | Exp | Log  deriving (Eq)

-- | Typeclass operation abstraction of all supported Operators
-- An Operator in this context is defined as a operation that takes
-- two elements and produces one
data Operator = Plus | Minus | Multiply | Divide | Power deriving (Eq)

-- | Data representation of a expression data structure
data Tree = BinaryNode Operator Tree Tree  -- Operatiors
          | UnaryNode Function Tree        -- Functions
          | NumNode Double                 -- Value nodes
          | VarNode Char deriving(Eq)      -- Variable nodes

-- | Stack data strucuture definiton consisting of trees
-- see buildStep function implementation for more details
type Stack = [Tree]

instance Show Operator where
    show Plus     = "+"
    show Minus    = "-"
    show Multiply = "*"
    show Divide   = "/"
    show Power    = "^"

instance Show Function where
    show Cosine = "cos"
    show Sine   = "sin"
    show Exp    = "exp"
    show Log     = "log"

instance Show Tree where
     show = inorderTraversal

-- | Stack emulation function with a list
-- Virtual stack used for constructing a evaluation tree
buildStep :: Stack -> Char -> Stack
buildStep (l:r:stack) '+' = (BinaryNode Plus r l):stack
buildStep (l:r:stack) '-' = (BinaryNode Minus r l):stack
buildStep (l:r:stack) '*' = (BinaryNode Multiply r l):stack
buildStep (l:r:stack) '/' = (BinaryNode Divide r l):stack
buildStep (l:r:stack) '^' = (BinaryNode Power r l):stack
buildStep (m:stack)   'S' = (UnaryNode Sine m):stack
buildStep (m:stack)   'C' = (UnaryNode Cosine m):stack
buildStep (m:stack)   'E' = (UnaryNode Exp m):stack
buildStep (m:stack)   'L' = (UnaryNode Log m):stack
buildStep stack c | isDigit c = (NumNode (fromIntegral $ digitToInt c)):stack
                  | isLower c = (VarNode c):stack
                  | otherwise = error $ "ERROR: Invalid character in postfix notation: " ++ [c]

-- || Generates a tree from a input string expression
-- Provide a valid string. Space delimeted equation not
-- required. See restictions mentioned above.
-- Example => "7* (x^2) +sin(y+x)"
createExpression :: String -> Tree
createExpression = getTree . foldl buildStep [] . SY.convert

-- || Proviedes an interface for substituting values on already
-- constructed trees.
substitute :: Char -> Double -> Tree -> Tree
substitute c i tn@(VarNode cc) =
    if c == cc
    then (NumNode i)
    else tn
substitute c i (UnaryNode cc t) = UnaryNode cc (substitute c i t)
substitute c i (BinaryNode cc l r) = BinaryNode cc (substitute c i l) (substitute c i r)
substitute c i tn@(NumNode _) = tn

-- || Evaluates to a reduces tree after performing a derivation
-- over a given variable Char
deriveBy :: Tree -> Char -> Tree
deriveBy t c = resolveTreeLoop $ derivationStep t c

-- | Unpacks a tree from a Stack
getTree :: Stack -> Tree
getTree = head

-- | Continously apply building steps for the evaluation tree
buildTree :: String -> Stack
buildTree = foldl buildStep []

-- | Traversing abstracion over Variable nodes.
-- Returns True if the condition is met, false otherwise
hasVarNode :: (Char -> Bool) -> Tree -> Bool
hasVarNode p (VarNode cc) = p cc
hasVarNode p (NumNode _) = False
hasVarNode p (UnaryNode _ t) = hasVarNode p t
hasVarNode p (BinaryNode _ l r) = or[hasVarNode p l, hasVarNode p r]

-- | Checks if the given Tree structure contains Variable nodes.
-- Used prior to evaluation of the tree
containsVarNode :: Tree -> Bool
containsVarNode = hasVarNode (\_ -> True)

-- | Checks if the given Tree strucutre contains a Variable defined
-- with the key character
containsVariable :: Char -> Tree -> Bool
containsVariable c = hasVarNode (== c)

-- | Checks if tree contains any 0 or 1 Numerical values
containsZeroOneNum :: Tree -> Bool
containsZeroOneNum (NumNode i) = or[i == 0, i == 1]
containsZeroOneNum (UnaryNode _ t) = containsZeroOneNum t
containsZeroOneNum (BinaryNode _ l r) = or[containsZeroOneNum l, containsZeroOneNum r]
containsZeroOneNum varNode = False

-- | Used for printing the evaluation tree
inorderTraversal :: Tree -> String
inorderTraversal (VarNode cc) = cc:[]
inorderTraversal (NumNode i)  = show i
inorderTraversal (UnaryNode f t)  = show f ++ inorderTraversal t
inorderTraversal (BinaryNode o t1 t2)  = "(" ++ (inorderTraversal t1) ++ show o ++ (inorderTraversal t2) ++ ")"

-- | Evaluates the whole tree given as parameter
-- NOTE: in order to be succesfully evaluated a tree
-- needs to be complete - meaning with no variable nodes
evaluate :: (Eq a,Floating a, Show a) => Tree -> a
evaluate (VarNode _) = error "ERROR: Tree still contains variables, susbstitute them before evaluation"
evaluate (NumNode i) = realToFrac i
evaluate (UnaryNode f t)
  | f == Cosine = cos (evaluate t)
  | f == Sine  = sin (evaluate t)
  | f == Exp  = exp (evaluate t)
  | f == Log  = log (evaluate t)
evaluate (BinaryNode o t1 t2)
  | o == Plus =(+) left right
  | o == Minus =(-) left right
  | o == Multiply = if or[t1 == (NumNode 0), t2 == (NumNode 0)]
                   then 0
                   else (*) left right
  | o == Divide =  if abs(right) == 0
                  then error "ERROR: Division by zero in the expression"
                  else (/) left right
  | o == Power =  (^) left (round (read (show right) :: Double))
    where left = evaluate t1
          right =  evaluate t2

-- | Resolves an incomplete expresion tree
-- Provides a mean of expression reduction and optimization
-- over a full blown expression tree. Meaning the method
-- reduces common operations like
--   (x*0) -> 0
resolveTree :: Tree -> Tree
resolveTree n@(UnaryNode f t) =
    if containsVarNode t
    then (UnaryNode f (resolveTree t))
    else UnaryNode f (NumNode (evaluate t))
resolveTree (BinaryNode Plus (NumNode 0) t) = t
resolveTree (BinaryNode Plus t (NumNode 0)) = t
resolveTree (BinaryNode Minus t (NumNode 0)) = t
resolveTree (BinaryNode Multiply (NumNode 0) _) = NumNode 0
resolveTree (BinaryNode Multiply _ (NumNode 0)) = NumNode 0
resolveTree (BinaryNode Multiply (NumNode 1) v) = v
resolveTree (BinaryNode Multiply v (NumNode 1)) = v
resolveTree (BinaryNode Power _ (NumNode 0)) = NumNode 1
resolveTree (BinaryNode Power (NumNode 0) _) = NumNode 0
resolveTree (BinaryNode Power v (NumNode 1)) = v
resolveTree n@(BinaryNode o l r)
  | and[containsVarNode l, containsVarNode r] = (BinaryNode o (resolveTree l) (resolveTree r) )
  | and[containsVarNode l == False, containsVarNode r == True] = (BinaryNode o (NumNode (evaluate l)) (resolveTree r))
  | and[containsVarNode l == True, containsVarNode r == False] = (BinaryNode o (resolveTree l) (NumNode (evaluate r)))
  | otherwise = BinaryNode o (NumNode (evaluate l)) (NumNode (evaluate r))
resolveTree t = t

-- | Repeatedly apply expression reduction over a given tree
-- The condition beeing while the tree has numerical vales that
-- equal to 0 or 1.
resolveTreeLoop :: Tree -> Tree
resolveTreeLoop t | containsZeroOneNum t = resolveTreeLoop $ resolveTree t
                  | otherwise         = t

-- | Single step derivation over a tree/subtree
derivationStep :: Tree -> Char -> Tree
derivationStep (BinaryNode o t1 t2) c
    | o == Plus = BinaryNode Plus (derivationStep t1 c) (derivationStep t2 c)
    | o == Minus = BinaryNode Minus (derivationStep t1 c) (derivationStep t2 c)
    | o == Multiply = (BinaryNode Plus (BinaryNode Multiply (t1) (derivationStep t2 c))) (BinaryNode Multiply (t2) (derivationStep t1 c))
    | o == Divide = (BinaryNode Divide (BinaryNode Minus (BinaryNode Multiply t2 (derivationStep t1 c)) (BinaryNode Multiply t1 (derivationStep t2 c))) (BinaryNode Power t2 (NumNode 2)))
    | o == Power = (BinaryNode Multiply (BinaryNode Multiply t2 (BinaryNode Power t1 (BinaryNode Minus t2 (NumNode 1)))) (derivationStep t1 c))
derivationStep un@(UnaryNode f t) c
    | f == Sine   = BinaryNode Multiply (UnaryNode Cosine (t)) (derivationStep t c)
    | f == Cosine = BinaryNode Multiply (BinaryNode Multiply (NumNode (-1)) (UnaryNode Sine t)) (derivationStep t c)
    | f == Exp    = BinaryNode Multiply un (derivationStep t c)
    | f == Log    = BinaryNode Multiply (BinaryNode Divide (NumNode 1) (t)) (derivationStep t c)
derivationStep (VarNode cc) c
    | c == cc = NumNode 1
    | otherwise = NumNode 0
derivationStep t@(NumNode i) _ = NumNode 0
