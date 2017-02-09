
-- | Implementation of the ShauntingYarn algorihtm
-- used for transforming from infix to postfix notation

module ShuntingYarn(
    convert -- converst a given string to postfix notation
)where

import Data.Char
import Data.Map
import Data.List.Utils

data Token = Number Int | ParenOpen | ParenClose | Variable Char
           | AddOp | MulOp  | DivOp | SubOp | PowOp | CosOp | SinOp | ExpOp | LogOp
            deriving (Show, Eq)

-- || converts a string to postfix notation with error handling
convert:: String -> String
convert = toString . transform . tokenize . replaceAll

transform:: [Maybe Token] -> [Token]
transform ts = transform' ts [] [] where
    transform' [] [] q = q
    transform' [] s q =
        if head s == ParenOpen
            then error "Mismatched Parentheses"
            else transform' [] (tail s) (q ++ [head s])
    transform' (x:xs) s q = case x of
        Nothing -> error "Illegal tokens"
        (Just (Number n)) -> transform' xs s (q ++ [Number n])
        (Just (Variable n)) -> transform' xs s (q ++ [Variable n])
        (Just ParenOpen) -> transform' xs (ParenOpen:s) q
        (Just ParenClose) -> transform' xs s0 q0 where
            s0 = tail $ dropWhile (/= ParenOpen) s
            q0 = q ++ takeWhile (/= ParenOpen) s
        (Just o1) -> transform' xs s1 q1 where
            cond o2 = operation o2 && (prec o1 < prec o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl

scanNumber:: String -> (Int, String)
scanNumber xs = (num, str) where
        (n, str) = span isNumber xs
        num = read n :: Int

scanCharacter :: String -> (String, String)
scanCharacter xs = span isLower xs

opmap:: Map Char Token
opmap = fromList [ ('+', AddOp), ('^', PowOp), ('C', CosOp), ('S', SinOp), ('*', MulOp), ('/', DivOp), ('(', ParenOpen),
                    ('-', SubOp), (')', ParenClose), ('E', ExpOp), ('L', LogOp)]

tokenize:: String -> [Maybe Token]
tokenize s = loop s [] where
    loop str tokens
        | Prelude.null str = tokens
        | isNumber $ head str = let
                    (num, str') = scanNumber str
                    tokens' = tokens ++ [Just (Number num)]
                in loop str' tokens'
        | isLower $ head str      = let (char, str') = scanCharacter str in
                                        if (length char) /= 1
                                        then error $ "Invalid variable name: " ++ char
                                        else loop str' (tokens ++ [Just (Variable (head char))])
        | isWhiteSpace $ head str = loop (tail str) tokens
        | otherwise = loop (tail str) (tokens ++ [Data.Map.lookup (head str) opmap])

prec:: Token -> Int
prec AddOp = 0
prec SubOp = 0
prec MulOp = 1
prec DivOp = 1
prec CosOp = 3
prec SinOp = 3
prec ExpOp = 3
prec LogOp = 3
prec PowOp = 2
prec ParenOpen = 4
prec ParenClose = 4
prec (Variable _) = 5
prec (Number _) = 5

operation:: Token -> Bool
operation AddOp = True
operation CosOp = True
operation SinOp = True
operation ExpOp = True
operation LogOp = True
operation PowOp = True
operation MulOp = True
operation DivOp = True
operation SubOp = True
operation  _ = False

isWhiteSpace:: Char -> Bool
isWhiteSpace '\n' = True
isWhiteSpace '\r' = True
isWhiteSpace '\0' = True
isWhiteSpace _ = False

toString:: [Token] -> String
toString = concatMap toStringOne where
    toStringOne (Number n) = show n
    toStringOne (Variable n) = n:[]
    toStringOne AddOp = "+"
    toStringOne SinOp = "S"
    toStringOne ExpOp = "E"
    toStringOne LogOp = "L"
    toStringOne CosOp = "C"
    toStringOne PowOp = "^"
    toStringOne MulOp = "*"
    toStringOne DivOp = "/"
    toStringOne SubOp = "-"
    toStringOne ParenOpen = "("
    toStringOne ParenClose = ")"

replaceAll :: String -> String
replaceAll = replace "cos" "C" . replace "sin" "S" . replace "log" "L" . replace "exp" "E" . replace " " ""
