module Day11 where

import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal
import Math.Geometry.GridMap (lookup)
import qualified Math.Geometry.GridMap as G
import Math.Geometry.GridMap.Lazy
import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Show

type Parser = M.Parsec Void String

data Tile = Floor | Empty | Occupied deriving (Eq)

instance Show Tile where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

type GameGrid = LGridMap RectOctGrid Tile

inputParser :: Parser [[Tile]]
inputParser = M.many tile `sepBy1` M.newline
  where
    tile = Floor <$ M.char '.' <|> Empty <$ M.char 'L' <|> Occupied <$ M.char '#'

parseInput :: String -> Maybe GameGrid
parseInput s = M.parseMaybe inputParser s >>= createGrid

createGrid :: [[Tile]] -> Maybe GameGrid
createGrid ts = (\columns -> lazyGridMap (rectOctGrid rows columns) (join ts)) <$> gridSize
  where
    rows = length ts
    gridSize = length <$> listToMaybe ts

converge :: GameGrid -> GetNeighbours -> Int -> GameGrid
converge g ns limit = if g == ng then ng else converge ng ns limit
  where
    ng = nextGrid g ns limit

nextGrid :: GameGrid -> GetNeighbours -> Int -> GameGrid
nextGrid g ns limit = G.mapWithKey (nextTile g ns limit) g

nextTile :: GameGrid -> GetNeighbours -> Int -> GridPos -> Tile -> Tile
nextTile g ns limit i t = nextTile' limit t (ns g i)

type GridPos = (Int, Int)

type GetNeighbours = GameGrid -> GridPos -> [Tile]

-- | If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
--   If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
--   Otherwise, the seat's state does not change.
nextTile' :: Int -> Tile -> [Tile] -> Tile
nextTile' _ Floor _ = Floor
nextTile' _ Empty ts | count (Occupied ==) ts == 0 = Occupied
nextTile' limit Occupied ts | count (Occupied ==) ts >= limit = Empty
nextTile' _ t _ = t

count :: (a -> Bool) -> [a] -> Int
count p as = length $ filter p as

-- | This solution is not fast, but it works
evaluate :: GameGrid -> GetNeighbours -> Int -> Int
evaluate grid ns limit = length $ G.filter (Occupied ==) (converge grid ns limit)

evaluate1 :: GameGrid -> Int
evaluate1 grid = evaluate grid getNeighbours 4

getNeighbours :: GetNeighbours
getNeighbours g i = catMaybes $ (`lookup` g) <$> neighbours g i

evaluate2 :: GameGrid -> Int
evaluate2 grid = evaluate grid getNeighbours2 5

getNeighbours2 :: GetNeighbours
getNeighbours2 grid pos = catMaybes [top, right, down, left, diag1, diag2, diag3, diag4]
  where
    top = foo grid (-1, 0) pos
    right = foo grid (0, 1) pos
    down = foo grid (1, 0) pos
    left = foo grid (0, -1) pos
    diag1 = foo grid (-1, -1) pos
    diag2 = foo grid (1, -1) pos
    diag3 = foo grid (-1, 1) pos
    diag4 = foo grid (1, 1) pos
    foo :: GameGrid -> GridPos -> GridPos -> Maybe Tile
    foo g inc i
      | nextT == Just Floor = foo g inc nextI
      | otherwise = nextT
      where
        nextI = add i inc
        nextT = G.lookup nextI g

-- | basically (<>) via Sum
add :: GridPos -> GridPos -> GridPos
add (a, b) (a', b') = (a + a', b + b')

run :: IO ()
run = readFile "inputs/day11-2.txt" >>= \s -> print (evaluate2 <$> parseInput s)
