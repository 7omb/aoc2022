import Control.Arrow ((>>>))
import Data.Char (isLower, isUpper, ord)
import Data.Set qualified as S

main :: IO ()
main = interact (lines >>> prioriies)

prioriies :: [String] -> String
prioriies = (map . map) toPriority >>> collectThree >>> map commonItem >>> sum >>> show

collectThree :: [a] -> [(a, a, a)]
collectThree = go []
  where
    go acc (a : b : c : xs) = go ((a, b, c) : acc) xs
    go acc [] = acc

toPriority :: Char -> Int
toPriority c
  | isUpper c = ord c - 38
  | isLower c = ord c - 96
  | otherwise = error "unexpected char"

commonItem :: Ord a => ([a], [a], [a]) -> a
commonItem (x, y, z) = S.findMax intersection
  where
    intersection = S.fromList x `S.intersection` S.fromList y `S.intersection` S.fromList z
