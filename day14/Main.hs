{-# LANGUAGE MultiWayIf #-}

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Array qualified as A
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Position = (Int, Int)

type Wall = (Position, Position)

data Cave = Cave
  { walls :: S.Set Position,
    sand :: S.Set Position,
    maxY :: Int
  }
  deriving (Show)

main :: IO ()
main = interact (parser >>> toCave >>> run >>> sand >>> S.size >>> show)

parser :: String -> [[Position]]
parser = parseMaybe file >>> fromJust

type Parser = Parsec Void String

file :: Parser [[Position]]
file = line `endBy` char '\n'

line :: Parser [Position]
line = coordinate `sepBy` string " -> "

coordinate :: Parser Position
coordinate = (,) <$> (digit <* char ',') <*> digit
  where
    digit = read <$> some digitChar

toCave :: [[Position]] -> Cave
toCave input =
  Cave
    { walls = S.fromList positions,
      sand = S.empty,
      maxY = maxY + 2
    }
  where
    lines = concatMap toLines input
    positions = concatMap toPositions lines
    toLines ps = zip ps (drop 1 ps)

    maxY = maximum $ map (\((_, y1), (_, y2)) -> max y1 y2) lines

toPositions :: Wall -> [Position]
toPositions ((x1, y1), (x2, y2)) = [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
  where
    xMin = min x1 x2
    xMax = max x1 x2
    yMin = min y1 y2
    yMax = max y1 y2

run :: Cave -> Cave
run cave =
  if S.size (sand cave) == S.size (sand nextCave)
    then cave
    else run nextCave
  where
    nextCave = grain cave

grain :: Cave -> Cave
grain cave =
  case nextPosition cave of
    Just p -> cave {sand = S.insert p (sand cave)}
    Nothing -> cave

data Update = Falling Position | AtRest Position | Outside
  deriving (Show)

nextPosition :: Cave -> Maybe Position
nextPosition = go (500, 0)
  where
    go pos cave =
      case step pos cave of
        Falling p -> go p cave
        AtRest p -> Just p
        Outside -> Nothing

step :: Position -> Cave -> Update
step p@(x, y) cave =
  if
      | isFree cave mid -> Falling mid
      | isFree cave left -> Falling left
      | isFree cave right -> Falling right
      | otherwise -> AtRest p
  where
    mid = (x, y + 1)
    left = (x - 1, y + 1)
    right = (x + 1, y + 1)

isFree :: Cave -> Position -> Bool
isFree cave pos@(_, y) = pos `S.notMember` walls cave && pos `S.notMember` sand cave && y < maxY cave
