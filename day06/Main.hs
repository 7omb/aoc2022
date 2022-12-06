import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Set qualified as S

main :: IO ()
main = interact (lines >>> head >>> marker)

marker :: String -> String
marker input = show $ foldl' search window remaining
  where
    window = (4, take 4 input)
    remaining = drop 4 input

type Window = (Int, String)

search :: Window -> Char -> Window
search (index, sequence) next =
  if length (S.fromList sequence) == 4
    then (index, sequence)
    else (index + 1, drop 1 sequence ++ [next])
