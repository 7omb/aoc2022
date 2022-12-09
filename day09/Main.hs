{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Set qualified as S

numKnots :: Int
numKnots = 10

main :: IO ()
main = interact (lines >>> parseMoves >>> run numKnots >>> snd >>> S.size >>> show)

data Direction = L | R | U | D
  deriving (Show, Read)

parseMoves :: [String] -> [Direction]
parseMoves = map (words >>> parseLine) >>> concat
  where
    parseLine [direction, number] =
      let amount = read number
          move = read direction :: Direction
       in replicate amount move

type Position = (Int, Int)

type Visited = S.Set Position

type State = ([Position], Visited)

run :: Int -> [Direction] -> State
run numKnots = foldl' move (initialPositions, S.empty)
  where
    initialPositions = replicate numKnots (0, 0)

move :: State -> Direction -> State
move (currentHead : tails, visited) direction =
  let newHead = moveHead currentHead direction
      newPositions = foldl' (\(last : rest) tail -> moveTail tail last : last : rest) [newHead] tails
      newVisited = S.insert (head newPositions) visited
   in (reverse newPositions, newVisited)

moveHead :: Position -> Direction -> Position
moveHead (x, y) = \case
  L -> (x - 1, y)
  R -> (x + 1, y)
  U -> (x, y + 1)
  D -> (x, y - 1)

moveTail :: Position -> Position -> Position
moveTail (tx, ty) (hx, hy)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty)
  | dx == 0 = if dy > 0 then (tx, ty + 1) else (tx, ty - 1)
  | dy == 0 = if dx > 0 then (tx + 1, ty) else (tx - 1, ty)
  | dx > 0 && dy > 0 = (tx + 1, ty + 1)
  | dx < 0 && dy > 0 = (tx - 1, ty + 1)
  | dx > 0 && dy < 0 = (tx + 1, ty - 1)
  | dx < 0 && dy < 0 = (tx - 1, ty - 1)
  where
    dx = hx - tx
    dy = hy - ty
