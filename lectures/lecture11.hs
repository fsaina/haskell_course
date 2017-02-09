import Data.List
import Data.Char

import Control.Exception

import System.Directory
import System.IO
import System.Environment
import System.Random

-- 1.1
main1 :: IO ()
main1 = do
  putStrLn "Enter first string"
  xs <- getLine
  putStrLn "Enter second string"
  ys <- getLine
  putStrLn $ reverse $ xs ++ ys

-- 1.2
threeNumbers :: IO ()
threeNumbers = do
  n1 <- getLine
  n2 <- getLine
  n3 <- getLine
  print $ read n1 + read n2 + read n3

-- 2.1
threeStrings :: IO Int
threeStrings = do
    s1 <- getLine
    s2 <- getLine
    s3 <- getLine
    let s = s1 ++ s2 ++ s3
    putStr s
    return $ length s

-- 2.2
askNumber9 :: IO Int
askNumber9 = do
  n1 <- getLine
  if containsAllDigits n1 then
    let n2 = read n1 :: Int in
    return n2
  else askNumber9
  where containsAllDigits xs = and $ map (isDigit) xs

main2 :: IO ()
main2 = do
  num <- askNumber9
  print num

-- 2.3
askUser :: String -> (String -> Bool) -> IO String
askUser [] _ = error "Enter a non empty string"
askUser m p= do
  putStrLn m
  val <- getLine
  if p val then
    return val
  else
    askUser m p

askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  val <- getLine :: IO String
  let num = read val in
    if p val then
      return num
    else
      askUser' m p

main3 :: IO ()
main3 = do
  s <- askUser "Enter a number" (\s -> and $ map isDigit s)
  putStrLn s

inputStrings :: IO [String]
inputStrings = inputStr []
  where inputStr l = do
          xs <- getLine :: IO String
          if null xs then
             return l
          else inputStr (xs:l)

-- 3.1
numberRead :: IO ()
numberRead = do
  m <- getLine
  let n = read m :: Int
  l <- sequence $ take n $ repeat getLine
  mapM_ (print) $ reverse l

-- 3.2
sequence' :: (Monad m) => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do
    x <- m
    xs <- sequence' ms
    return (x:xs)

sequence'_ xs = do
  sequence' xs
  return ()

-- 3.3
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as    =  sequence (map f as)

mapM'_ f as = do
  mapM' f as
  return ()

-- 3.4
-- TODO

-- 4.1
filterOdd :: IO ()
filterOdd = do
    s <- getContents
    putStr . unlines . map (snd) . filter (odd . fst) $ zip [1..] $ lines s

-- 4.2
numberLines :: IO ()
numberLines = do
  s <- getContents
  putStr . unlines . map (\(n, l) -> show n ++ " " ++ l ) $ zip [1..] $ lines s

-- 4.3
filterWords :: [String] -> IO ()
filterWords xs = do
  s <- getContents
  putStr . unwords . filter (\s -> not $ elem s xs) $ words s

-- wc :: FilePath -> IO (Int)
-- wc f = (withFile f ReadMode $ \h -> do
--         s <- getContents h
--         return $ length s)
--        dd


-- 5.2
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xi f f2 = do
  h <- openFile f ReadMode
  s <- hGetContents h
  let linesList = map (snd) . filter (\(n,l) -> elem n xi) . zip [1..] $ lines s
  h2 <- openFile f2 WriteMode
  mapM_ (writeFile f2) linesList
  return ()

-- 6.1
wordTypes :: FilePath -> IO Int
wordTypes f = withFile f ReadMode $ (\h -> do
  s <- hGetContents h
  let dWords = length $ nub $ words s in return dWords)

-- 6.2
diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  h1 <- openFile f1 ReadMode
  h2 <- openFile f2 ReadMode
  s1 <- hGetContents h1
  s2 <- hGetContents h2
  let l1 = lines s1
      l2 = lines s2
      lineDiffPairs = filter (\(l1,l2) -> l1 /= l2) $ zip l1 l2
      outputString = unlines $ map (\(l1,l2) -> "< " ++ l1 ++ "\n" ++ "> "++l2) lineDiffPairs
  putStrLn outputString

-- 6.3
removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  s <- readFile f
  let cleanLines = trimEnd $ lines s
  writeFile f $ unlines cleanLines
  where trimEnd xs = map (reverse . dropWhile isSpace . reverse ) xs

fileHead :: IO ()
fileHead = do
  a <- getArgs
  h <- case a of
         (_:f:[]) -> do e <- doesFileExist f
                        if e then openFile f ReadMode else return stdin
         []     -> return stdin
  n <- case a of
         (n:f:_) -> return $ (read n :: Int)
         _ -> return 10
  s <- hGetContents h
  if n < 0 then
      putStr . unlines . take (abs n) . reverse $ lines s
  else  putStr . unlines . take n $ lines s

sortFiles :: IO ()
sortFiles = do
    a <- getArgs
    if length a /= 3
    then do
        putStrLn "Invalid number of arguments"
    else do
        (h1,h2,h3) <- do
           let f1 = a!!0
               f2 = a!!1
               f3 = a!!2 in do
            checkFileExist f1
            checkFileExist f2
            checkFileExist f3
            let h1 = openFile f1 ReadMode
                h2 = openFile f2 ReadMode
                h3 = openFile f3 ReadMode
            return (h1,h2,h3)
        hh1 <- h1
        hh2 <- h2
        hh3 <- h3
        s1 <- hGetContents hh1
        s2 <- hGetContents hh2
        s3 <- hGetContents hh3
        let s = lines s1 ++ lines s2 ++ lines s3 in
            putStr . unlines $ sort s

    where checkFileExist f = do
            e <- doesFileExist f
            if e
            then return ()
            else putStrLn $ "File " ++ f ++ " does not exist"

-- 8.1
-- No idea kako bi ovo generalizirao da ne vraca samo Int --help ?
randoms' g = let (n1, g2) = next g in
             [n1] ++ randoms' g2

-- 8.2
-- Why doesn't this work? .. It seems the x function get evaluated only once
randomPosition :: Int -> Int -> Int -> Int -> IO [(Int, Int)]
randomPosition x1 x2 y1 y2 = do
    m <- x (x1, x2)
    n <- x (y1, y2)
    return $ zip (repeat m) (repeat n)
        where x r = do
                    g <- newStdGen
                    getStdRandom (randomR r) :: IO Int
