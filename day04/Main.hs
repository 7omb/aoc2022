import Control.Arrow ((>>>))
import Data.List.Split (splitOn)

main :: IO ()
main = interact (lines >>> contains)

contains :: [String] -> String
contains = map (splitOn "," >>> map toInterval >>> \[i1, i2] -> partiallyContains i1 i2) >>> filter (== True) >>> length >>> show

data Interval = Interval Int Int
  deriving (Show)

toInterval :: String -> Interval
toInterval = splitOn "-" >>> map read >>> \[l, u] -> Interval l u

fullyContains :: Interval -> Interval -> Bool
fullyContains (Interval l1 u1) (Interval l2 u2) = (l2 >= l1 && u2 <= u1) || (l1 >= l2 && u1 <= u2)

partiallyContains :: Interval -> Interval -> Bool
partiallyContains (Interval l1 u1) (Interval l2 u2) = (l1 <= l2 && l2 <= u1) || (l2 <= l1 && l1 <= u2)
