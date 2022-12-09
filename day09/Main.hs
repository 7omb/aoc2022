{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Set qualified as S

main :: IO ()
main = interact (lines >>> parseMoves >>> run >>> visited >>> S.size >>> show)

data Move = L | R | U | D
  deriving (Show, Read)

parseMoves :: [String] -> [Move]
parseMoves = map (words >>> parseLine) >>> concat
  where
    parseLine [direction, number] =
      let amount = read number
          move = read direction :: Move
       in replicate amount move

type Tail = (Int, Int)

type Head = (Int, Int)

type Visited = S.Set Tail

type State = (Head, Tail, Visited)

run :: [Move] -> State
run = foldl' move ((0, 0), (0, 0), S.empty)

move :: State -> Move -> State
move (head, tail, visited) move =
  let newHead = moveHead head move
      newTail = moveTail tail newHead
      newVisited = S.insert newTail visited
   in (newHead, newTail, newVisited)

moveHead :: Head -> Move -> Head
moveHead (x, y) = \case
  L -> (x - 1, y)
  R -> (x + 1, y)
  U -> (x, y + 1)
  D -> (x, y - 1)

moveTail :: Tail -> Head -> Tail
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

visited :: State -> S.Set Tail
visited (_, _, v) = v
