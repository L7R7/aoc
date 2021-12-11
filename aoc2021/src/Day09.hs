{-# LANGUAGE TupleSections #-}

module Day09 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import qualified Data.Map as M
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Math.Geometry.GridMap (lookup, (!))
import Math.Geometry.GridMap.Lazy
import Relude hiding (init)
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char

example :: Text
example = unlines ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]

exampleParsed :: [[Int]]
exampleParsed =
  [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
    [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
    [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
    [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
    [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
  ]

type GameGrid = LGridMap RectSquareGrid Int

inputParser :: Parsec Void Text GameGrid
inputParser = createGrid . filter (not . Relude.null) <$> Text.Megaparsec.many (digitToInt <$> digitChar) `sepEndBy1` newline

createGrid :: [[Int]] -> GameGrid
createGrid ts = lazyGridMap (rectSquareGrid rows columns) (join ts)
  where
    columns = length ts
    rows = length $ Unsafe.head ts

evaluate1 :: GameGrid -> Int
evaluate1 grid = sum $ (+ 1) . fst <$> filter (\(i, is) -> all (i <) is) (getNeighbours <$> indices grid)
  where
    getNeighbours :: (Int, Int) -> (Int, [Int])
    getNeighbours i = (grid ! i, catMaybes $ (`lookup` grid) <$> neighbours grid i)

evaluate2 :: GameGrid -> Int
evaluate2 grid = findAndMultiplyTopThree $ groupSamePoints $ findBasinLowPoint <$> indices grid
  where
    groupSamePoints :: Ord k => [k] -> Map k (Sum Int)
    groupSamePoints = M.fromListWith (<>) . fmap (,Sum (1 :: Int))

    findAndMultiplyTopThree :: (Num a, Ord a) => Map (Int, Int) (Sum a) -> a
    findAndMultiplyTopThree = getProduct . foldMap (Product . getSum) . take 3 . reverse . sort . M.elems

    findBasinLowPoint :: (Int, Int) -> (Int, Int)
    findBasinLowPoint index
      | val == 9 = index
      | val < fst adjacentMin = index
      | otherwise = findBasinLowPoint (snd adjacentMin)
      where
        val = grid ! index
        adjacentMin = minimumBy (\(i, _) (i', _) -> compare i i') $ (\ix -> (grid ! ix, ix)) <$> neighbours grid index

run :: IO ()
run = do
  inputRaw <- readFileText "inputs/day09.txt"

  case parseMaybe inputParser inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 $ createGrid exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 15)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 518)

      putStrLn "=== Part 2"
      let solution4 = evaluate2 $ createGrid exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4
      putStrLn $ "Solution is correct for example input: " <> show (solution4 == 1134)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 949905)
