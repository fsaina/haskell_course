module CSVUtils
    ( Separator
    , Document
    , CSV
    , Entry
    , Field
    , parseCSV
    , showCSV
    , colFields
    , readCSV
    , writeCSV
    ) where

import Data.List.Split
import Data.List
import System.Directory
import Control.Monad
import Data.Maybe
import System.IO

type Separator = String
type Document  = String
type CSV       = [Entry]
type Entry     = [Field]
type Field     = String

csvLen :: CSV -> [Int]
csvLen = map (length)

csvMaxLen :: CSV -> Int
csvMaxLen = maximum . csvLen

csvAllSameLength :: CSV -> Bool
csvAllSameLength c = and . map ((==) (length $ head c)) $ csvLen c

parseCSV :: Separator -> Document -> CSV
parseCSV s d
  | not(allSameLength) = error "CSV file is not well formed"
  | not(validDelimeter)= error $ "The separator " ++ s ++ " does not occur"
  |  otherwise       = split
  where split =  map (splitOn s) $ lines d
        allSameLength = csvAllSameLength split
        validDelimeter = elem (s!!0) d

showCSV :: Separator -> CSV -> Document
showCSV s c
  | csvAllSameLength c = unlines $ map (intercalate s) c
  | otherwise          = error "CSV file not well formed"

colFields :: Int -> CSV -> [Field]
colFields col c
  | col >= csvMaxLen c = error $ "There is no col: " ++ show col ++ "in the CSV document"
  | otherwise         = map (\f -> f!!col) c

-- This is just beautiful
readCSV :: Separator -> FilePath -> IO CSV
readCSV s f =
    let exists b = if b
                   then openFile f ReadMode
                   else do
                       error "The file you provided does not exist"
    in doesFileExist f >>= exists >>= hGetContents >>= return . parseCSV s

writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV s f c = (return $ showCSV s c) >>= writeFile f
