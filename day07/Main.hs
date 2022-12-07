{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (find, foldl', partition)

main :: IO ()
main = interact (lines >>> map (words >>> parseLine) >>> sizes)

sizes :: [Input] -> String
sizes = foldl' apply Initial >>> toFileSystem >>> toSizes >>> dirSizes >>> filter (<= sizeLimit) >>> sum >>> show

data Input = Cd Target | Ls | Listing Content
  deriving (Eq, Show)

data Target = Up | ChangeDir String
  deriving (Eq, Show)

data Content = Dir String | File String Integer
  deriving (Eq, Show)

parseLine :: [String] -> Input
parseLine = \case
  ["$", "cd", ".."] -> Cd Up
  ["$", "cd", dir] -> Cd (ChangeDir dir)
  ["$", "ls"] -> Ls
  ["dir", dir] -> Listing (Dir dir)
  [size, name] -> Listing (File name (read size))

data FileSystem = DirNode String [FileSystem] | FileNode String Integer
  deriving (Eq, Show)

data Position = In String [FileSystem] Breadcrumb | Initial
  deriving (Eq, Show)

type Breadcrumb = [(String, [FileSystem])]

apply :: Position -> Input -> Position
apply (In name content breadcrumbs) (Listing listing) =
  In name (add content listing) breadcrumbs
apply (In name content breadcrumbs) (Cd (ChangeDir subName)) =
  case getNode subName content of
    Just (DirNode _ fs) -> In subName fs newBreadcrumb
    Just (FileNode _ _) -> error ("Cannot cd into file: " ++ subName)
    Nothing -> In subName [] newBreadcrumb
  where
    newBreadcrumb = (name, removeNode subName content) : breadcrumbs
apply (In name content ((parentName, parentFs) : breadcrumbs)) (Cd Up) =
  let currentDir = DirNode name content
   in In parentName (currentDir : parentFs) breadcrumbs
apply (In name content []) (Cd Up) = error "Cannot cd .. without breadcrumbs"
apply pos@(In {}) Ls = pos
apply Initial (Cd (ChangeDir "/")) = In "/" [] []
apply Initial _ = error "Initial position must start at root"

add :: [FileSystem] -> Content -> [FileSystem]
add fs content
  | contentName content `elem` map nodeName fs = fs
  | otherwise = toNode content : fs

nodeName :: FileSystem -> String
nodeName (DirNode name _) = name
nodeName (FileNode name _) = name

contentName :: Content -> String
contentName (Dir name) = name
contentName (File name _) = name

toNode :: Content -> FileSystem
toNode (Dir name) = DirNode name []
toNode (File name size) = FileNode name size

getNode :: String -> [FileSystem] -> Maybe FileSystem
getNode name = find (\n -> nodeName n == name)

removeNode :: String -> [FileSystem] -> [FileSystem]
removeNode name = filter (\n -> nodeName n /= name)

toFileSystem :: Position -> FileSystem
toFileSystem pos@(In _ _ (b : bs)) = toFileSystem (apply pos (Cd Up))
toFileSystem (In name fs []) = DirNode name fs
toFileSystem Initial = error "Cannot convert initial state to filesystem"

sizeLimit :: Integer
sizeLimit = 100000

data Sizes = DirSize Integer [Sizes] | FileSize Integer
  deriving (Eq, Show)

toSizes :: FileSystem -> Sizes
toSizes (FileNode _ size) = FileSize size
toSizes (DirNode name fs) =
  let sizes = map toSizes fs
      dirSize = sum $ map getSize sizes
   in DirSize dirSize sizes

getSize :: Sizes -> Integer
getSize (DirSize size _) = size
getSize (FileSize size) = size

dirSizes :: Sizes -> [Integer]
dirSizes (DirSize size sizes) = size : concatMap dirSizes sizes
dirSizes (FileSize _) = []
