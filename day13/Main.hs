{-# LANGUAGE LambdaCase #-}

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = interact (parser >>> map (uncurry compare) >>> sumIndices >>> show)

parser :: String -> [(Packet, Packet)]
parser = parse fileParser "" >>> fromRight []

type Parser = Parsec Void String

fileParser :: Parser [(Packet, Packet)]
fileParser = pair `sepBy` string "\n\n"
  where
    pair = (,) <$> (packetParser <* char '\n') <*> packetParser

packetParser :: Parser Packet
packetParser = char '[' *> cellParser `sepBy` char ',' <* char ']'

cellParser :: Parser Cell
cellParser = (Nested <$> packetParser) <|> (Num . read <$> some digitChar)

type Packet = [Cell]

data Cell = Num Int | Nested Packet
  deriving (Show, Eq)

instance Ord Cell where
  compare (Num n1) (Num n2) = compare n1 n2
  compare (Num n) (Nested p) = compare [Num n] p
  compare (Nested p) (Num n) = compare p [Num n]
  compare (Nested p1) (Nested p2) = compare p1 p2

sumIndices :: [Ordering] -> Int
sumIndices = zip [1 ..] >>> filter (snd >>> (== LT)) >>> map fst >>> sum
