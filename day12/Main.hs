import Control.Arrow ((>>>))
import Data.Array qualified as A
import Data.Char (ord)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

main :: IO ()
main = interact (lines >>> parse >>> transform >>> run >>> shortestDistance >>> show)

type Grid = A.Array Position Char

type Position = (Int, Int)

parse :: [[Char]] -> Grid
parse input =
  A.array
    ((0, 0), (numRows - 1, numCols - 1))
    [((row, col), x) | (row, cols) <- withIndices, (col, x) <- cols]
  where
    numRows = length input
    numCols = length . head $ input
    withIndices = zip [0 ..] $ map (zip [0 ..]) input

transform :: Grid -> (Position, Position, Grid)
transform input =
  (start, end, array)
  where
    findChar c = fst . fromJust . find (\e -> snd e == c) . A.assocs
    start = findChar 'S' input
    end = findChar 'E' input
    array = input A.// [(start, 'a'), (end, 'z')]

data Graph = Graph
  { parents :: M.Map Position Position,
    distances :: M.Map Position Int
  }
  deriving (Show)

run :: (Position, Position, Grid) -> (Position, Graph)
run (start, end, grid) =
  (end, go grid inProgress visited parents distances)
  where
    inProgress = Seq.singleton start
    visited = Set.singleton start
    parents = M.empty
    distances = M.singleton start 0

    go grid Seq.Empty vs ps ds = Graph ps ds
    go grid (i Seq.:<| is) vs ps ds =
      let next = filter (`Set.notMember` vs) $ moves grid i
          newVisited = Set.fromList next `Set.union` vs
          newParents = M.fromList (zip next $ repeat i) `M.union` ps
          distance = 1 + ds M.! i
          newDistances = M.fromList (zip next $ repeat distance) `M.union` ds
       in go grid (is Seq.>< Seq.fromList next) newVisited newParents newDistances

moves :: Grid -> Position -> [Position]
moves grid current@(row, col) =
  filter
    (\next -> inGrid grid next && isAllowed grid current next)
    [up, down, left, right]
  where
    up = (row + 1, col)
    down = (row - 1, col)
    left = (row, col - 1)
    right = (row, col + 1)

inGrid :: Grid -> Position -> Bool
inGrid grid (row, col) =
  let (maxRow, maxCol) = snd . A.bounds $ grid
   in 0 <= row && row <= maxRow && 0 <= col && col <= maxCol

isAllowed :: Grid -> Position -> Position -> Bool
isAllowed grid current next =
  let currentHeight = grid A.! current
      nextHeight = grid A.! next
   in ord nextHeight <= ord currentHeight + 1

shortestDistance :: (Position, Graph) -> Int
shortestDistance (end, graph) =
  distances graph M.! end
