module Day08 (run) where

import Data.Char (digitToInt)
import Data.List (maximum, zipWith4)
import qualified Data.Map as M
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))
import Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as G
import Math.Geometry.GridMap.Lazy
import Relude hiding (many)
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char

inputParser :: Parsec Void Text Input1
inputParser = many (digitToInt <$> digitChar) `sepBy` newline

type Input1 = [[Int]]

evaluate1 :: Input1 -> Sum Int
evaluate1 leftToRight = foldMap (\b -> if b then Sum 1 else mempty) resGrid
  where
    rightToLeft = reverse <$> leftToRight
    topToBottom = transpose leftToRight
    bottomToTop = reverse <$> topToBottom
    resGrid =
      Compose $
        zipWith4
          (zipWith4 (\b b' b3 b4 -> b || b' || b3 || b4))
          (f leftToRight)
          (reverse <$> f rightToLeft)
          (transpose $ f topToBottom)
          (transpose $ reverse <$> f bottomToTop)
    f :: [[Int]] -> [[Bool]]
    f grid = snd . mapAccumL g Nothing <$> grid
    g :: Maybe Int -> Int -> (Maybe Int, Bool)
    g Nothing n = (Just n, True)
    g (Just prev) n = (Just (max prev n), prev < n)

type Input2 = LGridMap RectSquareGrid Int

createGrid :: [[Int]] -> Input2
createGrid ts = lazyGridMap (rectSquareGrid rows columns) (join ts)
  where
    columns = length ts
    rows = length $ Unsafe.head ts

evaluate2 :: Input1 -> Int
evaluate2 = evaluate2' . createGrid

evaluate2' :: Input2 -> Int
evaluate2' grid = maximum $ snd <$> M.toList combinedDistances
  where
    combinedDistances = foldl' (\acc g -> M.unionWith (*) acc (G.toMap g)) M.empty distances
    distances = (\direction -> G.mapWithKey (f direction) grid) <$> [North, South, East, West]
    f :: SquareDirection -> (Int, Int) -> Int -> Int
    f direction index value = go index
      where
        go :: (Int, Int) -> Int
        go ix = case neighbour grid ix direction of
          Nothing -> 0
          Just in' -> 1 + if value > grid ! in' then go in' else 0

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/08-example.txt"
  let exampleParsed1 = parseMaybe inputParser exampleInputRaw
  print $ traverse evaluate1 exampleParsed1

  inputRaw <- readFileText "inputs/08.txt"
  let parsed1 = parseMaybe inputParser inputRaw
  print $ traverse evaluate1 parsed1

  print $ evaluate2 <$> exampleParsed1
  print $ evaluate2 <$> parsed1
