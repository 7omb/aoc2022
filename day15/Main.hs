import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = interact (lines >>> parser >>> map toInterval >>> rows >>> show)

parser :: [String] -> [(Position, Position)]
parser = map (parseMaybe line >>> fromJust)

type Position = (Int, Int)

line :: Parsec Void String (Position, Position)
line = do
  string "Sensor at x="
  sx <- number
  string ", y="
  sy <- number
  string ": closest beacon is at x="
  bx <- number
  string ", y="
  by <- number
  return ((sx, sy), (bx, by))

number :: Parsec Void String Int
number =
  read <$> do
    sign <- optional (char '-')
    digits <- some digitChar
    return $ case sign of
      Nothing -> digits
      Just s -> s : digits

type Row = Int

type Col = Int

type Interval = (Col, Col)

toInterval :: (Position, Position) -> Row -> Interval
toInterval (sensor@(sX, sY), beacon) row =
  if gap <= distance
    then (sX - (distance - gap), sX + (distance - gap))
    else (0, 0)
  where
    distance = manhattan sensor beacon
    gap = abs (sY - row)

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

rowIntervals :: Row -> [Row -> Interval] -> [Interval]
rowIntervals row = map ($ row) >>> sort >>> merge

merge :: [Interval] -> [Interval]
merge = go []
  where
    go acc ((ax1, ax2) : (bx1, bx2) : is) =
      if ax2 >= bx1
        then go acc ((ax1, max ax2 bx2) : is)
        else go ((ax1, ax2) : acc) ((bx1, bx2) : is)
    go acc (i : is) = go (i : acc) is
    go acc [] = reverse acc

numPositions :: [Interval] -> Int
numPositions = map (\(x1, x2) -> x2 - x1) >>> sum

rows :: [Row -> Interval] -> [(Row, [Interval])]
rows intervals = filter (\e -> length (snd e) > 1) [(row, rowIntervals row intervals) | row <- [1 .. 4000000]]
