import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import System.IO
import System.Environment
import System.Directory

data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person = Person {
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  mother   :: Maybe Person,
  father   :: Maybe Person,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)

-- 1.1
grandfathersPartnerForename :: Person -> Maybe String
grandfathersPartnerForename p = father p >>= father >>= partner >>= return . forename

-- 1.2
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = let rev = reverse xs in Just ys >>= return . reverse >>= stripPrefix rev>>= return . reverse

removeAffixes :: String -> String -> String -> Maybe String
removeAffixes xs ys zs = Just zs >>= stripPrefix xs >>= stripSuffix ys

-- 1.3
gradfathersPartnerForename2 :: Person -> Maybe String
gradfathersPartnerForename2 p = do
    f <- father p
    g <- father f
    pp <- partner g
    return $ forename pp

-- desurarizded
-- This is actually incredible
main :: IO ()
main =
    let readArgs (f:_) = doesFileExist f >>= (\e -> if e then openFile f ReadMode else return stdin)
        []         = return stdin
     in getArgs >>= readArgs >>= hGetContents >>= putStr.unlines.sort.lines

