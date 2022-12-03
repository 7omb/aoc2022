{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.Char (isLower, isUpper, ord)
import Data.Set qualified as S
import System.Posix (BaudRate (B9600))

main :: IO ()
main = interact (lines >>> prioriies)

prioriies :: [String] -> String
prioriies = (map . map) toPriority >>> map (split >>> commonItem) >>> sum >>> show

split :: [a] -> ([a], [a])
split s = splitAt mid s
  where
    mid = length s `div` 2

toPriority :: Char -> Int
toPriority c
  | isUpper c = ord c - 38
  | isLower c = ord c - 96
  | otherwise = error "unexpected char"

commonItem :: Ord a => ([a], [a]) -> a
commonItem (x, y) = S.findMax intersection
  where
    intersection = S.fromList x `S.intersection` S.fromList y
