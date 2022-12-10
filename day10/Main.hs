import Control.Arrow ((>>>))
import Data.List (foldl')
import Data.Vector qualified as V
import Prelude hiding (cycle)

height :: Int
height = 6

width :: Int
width = 40

main :: IO ()
main = interact (lines >>> parse >>> foldl' run initialState >>> screen >>> render)
  where
    initialState = State {x = 1, cycle = 1, screen = initialScreen}

data Instruction = Noop | Addx Int | Busy
  deriving (Show)

parse :: [String] -> [Instruction]
parse = map (words >>> instruction) >>> concat
  where
    instruction ["noop"] = [Noop]
    instruction ["addx", value] = [Busy, Addx (read value)]

data State = State {x :: Int, cycle :: Int, screen :: Screen}
  deriving (Show)

run :: State -> Instruction -> State
run (State {x, cycle, screen}) Busy =
  State {x, cycle = cycle + 1, screen = draw screen x cycle}
run (State {x, cycle, screen}) Noop =
  State {x, cycle = cycle + 1, screen = draw screen x cycle}
run (State {x, cycle, screen}) (Addx value) =
  State {x = x + value, cycle = cycle + 1, screen = draw screen x cycle}

type Screen = V.Vector (V.Vector Char)

initialScreen :: Screen
initialScreen = V.replicate height (V.replicate width '.')

draw :: Screen -> Int -> Int -> Screen
draw s x cycle =
  if atSprite
    then
      let row = s V.! cycleRow
          updatedRow = row V.// [(cycleCol, '#')]
       in s V.// [(cycleRow, updatedRow)]
    else s
  where
    cycleRow = row cycle
    cycleCol = col cycle

    atSprite = (cycleCol + 1) `elem` [x, x + 1, x + 2]

render :: Screen -> String
render s =
  let rows = V.map V.toList s
   in V.foldl' (\acc row -> acc ++ row ++ "\n") "" rows

col :: Int -> Int
col pos = (pos - 1) `mod` width

row :: Int -> Int
row pos = (pos - 1) `div` width
