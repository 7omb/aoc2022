main :: IO ()
main = interact points

points :: String -> String
points = show . sum . map (pointsForRound . words) . lines

pointsForRound :: [String] -> Integer
pointsForRound ["A", "X"] = 0 + 3
pointsForRound ["A", "Y"] = 3 + 1
pointsForRound ["A", "Z"] = 6 + 2
pointsForRound ["B", "X"] = 0 + 1
pointsForRound ["B", "Y"] = 3 + 2
pointsForRound ["B", "Z"] = 6 + 3
pointsForRound ["C", "X"] = 0 + 2
pointsForRound ["C", "Y"] = 3 + 3
pointsForRound ["C", "Z"] = 6 + 1
