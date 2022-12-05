import Control.Arrow ((>>>))
import Data.Bifunctor (bimap)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf, splitWhen)

main :: IO ()
main = interact (lines >>> parseAndRun)

parseAndRun :: [String] -> String
parseAndRun input = show $ run initialStacks moves
  where
    [rawStacks, rawMoves] = splitWhen (== "") input
    initialStacks = parseStacks rawStacks
    moves = parseMoves rawMoves

type Crate = Char

type Stacks = [Stack]

type Stack = [Crate]

parseStacks :: [String] -> Stacks
parseStacks = map (chunksOf 4) >>> reverse >>> drop 1 >>> transpose >>> (map . map) parseCrate >>> map concat

parseCrate :: String -> String
parseCrate r = [r !! 1 | r !! 1 /= ' ']

run :: Stacks -> [Move] -> Stacks
run = foldl' move

move :: Stacks -> Move -> Stacks
move stacks (Move {start, end, numCrates}) = addedEnd
  where
    startStack = stacks !! (start - 1)
    endStack = stacks !! (end - 1)

    index = length startStack - numCrates
    newStart = take index startStack
    toMove = reverse $ drop index startStack
    newEnd = endStack ++ toMove

    addedStart = take (start - 1) stacks ++ [newStart] ++ drop start stacks
    addedEnd = take (end - 1) addedStart ++ [newEnd] ++ drop end addedStart

data Move = Move
  { start :: Int,
    end :: Int,
    numCrates :: Int
  }
  deriving (Show)

parseMoves :: [String] -> [Move]
parseMoves = map words >>> map parseLine
  where
    parseLine l = Move {numCrates = read (l !! 1), start = read (l !! 3), end = read (l !! 5)}
