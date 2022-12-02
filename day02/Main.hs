main :: IO ()
main = interact points

points :: String -> String
points = show . sum . map (pointsForRound . words) . lines

pointsForRound :: [String] -> Integer
pointsForRound ["A", "X"] = 1 + 3
pointsForRound ["A", "Y"] = 2 + 6
pointsForRound ["A", "Z"] = 3 + 0
pointsForRound ["B", "X"] = 1 + 0
pointsForRound ["B", "Y"] = 2 + 3
pointsForRound ["B", "Z"] = 3 + 6
pointsForRound ["C", "X"] = 1 + 6
pointsForRound ["C", "Y"] = 2 + 0
pointsForRound ["C", "Z"] = 3 + 3
