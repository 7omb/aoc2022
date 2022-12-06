import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Set qualified as S

main :: IO ()
main = interact (lines >>> head >>> marker)

headerLength :: Int
headerLength = 14

marker :: String -> String
marker input = show $ foldl' search window remaining
  where
    window = (headerLength, take headerLength input)
    remaining = drop headerLength input

type Window = (Int, String)

search :: Window -> Char -> Window
search (index, sequence) next =
  if length (S.fromList sequence) == headerLength
    then (index, sequence)
    else (index + 1, drop 1 sequence ++ [next])
