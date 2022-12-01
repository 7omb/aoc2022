import Data.List (sort)

main :: IO ()
main = interact calories

calories :: String -> String
calories = show . sum . take 3 . reverse . sort . sumEntries . lines

sumEntries :: [String] -> [Integer]
sumEntries = foldl go []
  where
    go (x : xs) "" = 0 : x : xs
    go (x : xs) calorie = read calorie + x : xs
    go [] "" = []
    go [] calorie = [read calorie]
