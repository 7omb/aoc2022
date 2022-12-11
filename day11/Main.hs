{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (foldl', partition, sort)
import Data.List.Split (splitOn)
import Prelude hiding (round)

main :: IO ()
main = interact (parse >>> run 10000 >>> map business >>> sort >>> reverse >>> take 2 >>> product >>> show)

parse :: String -> [Monkey]
parse = splitOn "\n\n" >>> map parseMonkey

parseMonkey :: String -> Monkey
parseMonkey = lines >>> map (splitOn ":") >>> parseLines
  where
    parseLines [[monkey, _], [_, items], [_, operation], [_, test], [_, ifTrue], [_, ifFalse]] =
      Monkey
        { monkey = parseMonkeyNumber monkey,
          items = parseItems items,
          operation = parseOperation operation,
          test = parseTest test,
          ifTrue = parseThrow ifTrue,
          ifFalse = parseThrow ifFalse,
          business = 0
        }
    parseMonkeyNumber = words >>> last >>> read
    parseItems = splitOn ", " >>> map read
    parseOperation = splitOn " = " >>> last >>> words >>> toOperation
    parseTest = words >>> last >>> read
    parseThrow = words >>> last >>> read

data Monkey = Monkey
  { monkey :: Int,
    items :: [Int],
    operation :: Operation,
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    business :: Int
  }
  deriving (Show)

data Operation = Add Value Value | Mul Value Value
  deriving (Show)

data Value = Const Int | Old
  deriving (Show)

toOperation :: [String] -> Operation
toOperation ["old", "*", "old"] = Mul Old Old
toOperation ["old", "*", v] = Mul Old (Const $ read v)
toOperation ["old", "+", v] = Add Old (Const $ read v)

eval :: Int -> Operation -> Int
eval old = \case
  Mul Old Old -> old * old
  Mul Old (Const v) -> old * v
  Add Old (Const v) -> old + v
  _ -> error "case not covered"

data Throw = Throw {toMonkey :: Int, item :: Int}

throw :: Int -> Monkey -> (Monkey, [Throw])
throw divisor Monkey {monkey, items, operation, test, ifTrue, ifFalse, business} =
  let inspected = map (`eval` operation) items
      bored = map (`mod` divisor) inspected
   in ( Monkey {monkey, items = [], operation, test, ifTrue, ifFalse, business = business + length inspected},
        map (\item -> Throw {toMonkey = if item `mod` test == 0 then ifTrue else ifFalse, item = item}) bored
      )

catch :: [Throw] -> [Monkey] -> [Monkey]
catch (Throw {toMonkey, item} : ts) monkeys =
  let ([m], ms) = partition (\m -> monkey m == toMonkey) monkeys
   in catch ts (m {items = items m ++ [item]} : ms)
catch [] monkeys = monkeys

round :: [Monkey] -> [Monkey]
round monkeys = go 0 monkeys
  where
    divisor = product $ map test monkeys
    go current monkeys
      | current < length monkeys =
          let ([m], ms) = partition (\m -> monkey m == current) monkeys
              (updatedMonkey, throws) = throw divisor m
              catched = catch throws ms
           in go (current + 1) (updatedMonkey : catched)
      | otherwise = monkeys

run :: Int -> [Monkey] -> [Monkey]
run numRounds monkeys = go 0 monkeys
  where
    go currentRound monkeys
      | currentRound < numRounds = go (currentRound + 1) (round monkeys)
      | otherwise = monkeys
