import Control.Arrow ((>>>))
import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Vector qualified as V
import Debug.Trace (trace)

main :: IO ()
main = interact (lines >>> visible >>> show)

visible :: [String] -> Int
visible input =
  let grid = parseGrid input
      height = length grid
      width = length grid
      relevant = relevantIndices height width
   in maximum $ fmap (scenicScore height width grid) relevant

type Grid = V.Vector (V.Vector Int)

type Index = (Int, Int)

parseGrid :: [String] -> Grid
parseGrid = (map . map) digitToInt >>> map V.fromList >>> V.fromList

relevantIndices :: Int -> Int -> [(Int, Int)]
relevantIndices height width =
  [(row, col) | row <- [1 .. height - 2], col <- [1 .. width - 2]]

data Connected a = Connected {left :: a, right :: a, top :: a, bottom :: a}
  deriving (Show, Functor)

connectedIndices :: Int -> Int -> (Int, Int) -> Connected [Index]
connectedIndices height width (row, col) =
  Connected
    { left = reverse [(row, c) | c <- [0 .. col - 1]],
      right = [(row, c) | c <- [col + 1 .. width - 1]],
      top = reverse [(r, col) | r <- [0 .. row - 1]],
      bottom = [(r, col) | r <- [row + 1 .. height - 1]]
    }

toList :: Connected a -> [a]
toList Connected {left, right, top, bottom} = [left, right, top, bottom]

getHeight :: Grid -> Index -> Int
getHeight grid (row, col) = grid V.! row V.! col

scenicScore :: Int -> Int -> Grid -> Index -> Int
scenicScore height width grid index =
  let treeHeight = getHeight grid index
      connected = connectedIndices height width index
      heights = (fmap . map) (getHeight grid) connected
      distances = fmap (distance treeHeight) heights
   in product (toList distances)

distance :: Int -> [Int] -> Int
distance treeHeight heights =
  case findIndex (>= treeHeight) heights of
    Just index -> index + 1
    Nothing -> length heights
