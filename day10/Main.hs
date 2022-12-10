{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (foldl')
import Prelude hiding (cycle)

main :: IO ()
main = interact (lines >>> parse >>> foldl' run initialState >>> show)
  where
    initialState = State {x = 1, cycle = 1, signal = 0}

data Instruction = Noop | Addx Int | Busy
  deriving (Show)

parse :: [String] -> [Instruction]
parse = map (words >>> instruction) >>> concat
  where
    instruction ["noop"] = [Noop]
    instruction ["addx", value] = [Busy, Addx (read value)]

data State = State {x :: Int, cycle :: Int, signal :: Int}
  deriving (Show)

run :: State -> Instruction -> State
run (State {x, cycle, signal}) Busy = State {x, cycle = cycle + 1, signal = nextSignal x cycle signal}
run (State {x, cycle, signal}) Noop = State {x, cycle = cycle + 1, signal = nextSignal x cycle signal}
run (State {x, cycle, signal}) (Addx value) = State {x = x + value, cycle = cycle + 1, signal = nextSignal x cycle signal}

nextSignal :: Int -> Int -> Int -> Int
nextSignal x cycle signal
  | (cycle + 20) `mod` 40 == 0 = signal + (x * cycle)
  | otherwise = signal
