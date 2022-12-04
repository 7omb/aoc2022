import Control.Arrow ((>>>))
import Data.Bifunctor (bimap, second)

main :: IO ()
main = interact (lines >>> contains)

contains :: [String] -> String
contains = map (split ',' >>> bimap toInterval toInterval >>> uncurry partiallyContains) >>> filter (== True) >>> length >>> show

split :: Char -> String -> (String, String)
split c = span (/= c) >>> second (drop 1)

data Interval = Interval Int Int
  deriving (Show)

toInterval :: String -> Interval
toInterval = split '-' >>> bimap read read >>> uncurry Interval

fullyContains :: Interval -> Interval -> Bool
fullyContains (Interval l1 u1) (Interval l2 u2) = (l2 >= l1 && u2 <= u1) || (l1 >= l2 && u1 <= u2)

partiallyContains :: Interval -> Interval -> Bool
partiallyContains (Interval l1 u1) (Interval l2 u2) = (l1 <= l2 && l2 <= u1) || (l2 <= l1 && l1 <= u2)
