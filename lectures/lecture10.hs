import System.IO
import Control.Monad

-- "<-" gets

--main = putStrLn "Hello, world!"

main2 = do
    putStrLn "bok"
    putStrLn "bubble"
    putStrLn "hehe"

blob = do
    putStrLn "Bok bok hej"
    number <- getLine
    putStr $ "hejj " ++ number

-- EX1

main3 = do
    s1 <- getLine
    s2 <- getLine
    putStrLn $ reverse $ s1 ++ s2
    threeNumbers
    

threeNumbers = do
    n1 <- getLine
    n2 <- getLine
    n3 <- getLine
    print $ read n1 + read n2 + read n3


-- print je zapravo printStrLn . show ()


threeStrings :: IO Int
threeStrings = do
    s1 <- getLine
    s2 <- getLine
    s3 <- getLine
    let s = s1 ++ s2 ++ s3
    putStr s
    return $ length s

--askNumber9 :: IO Int
--askNumber9 

--EX3
--readNumTimesN = 

--filterOdd :: IO ()
main = do
    s <- getContents
    putStr . unlines . map (snd) . filter (odd . fst) $ zip [1..] $ lines s

