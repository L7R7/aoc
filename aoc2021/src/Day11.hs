{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.Char (digitToInt)
import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal
import Math.Geometry.GridMap (adjust, (!))
import qualified Math.Geometry.GridMap as G
import Math.Geometry.GridMap.Lazy
import Relude hiding (init)
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char

example :: Text
example = unlines ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]

exampleParsed :: [[Int]]
exampleParsed =
  [ [5, 4, 8, 3, 1, 4, 3, 2, 2, 3],
    [2, 7, 4, 5, 8, 5, 4, 7, 1, 1],
    [5, 2, 6, 4, 5, 5, 6, 1, 7, 3],
    [6, 1, 4, 1, 3, 3, 6, 1, 4, 6],
    [6, 3, 5, 7, 3, 8, 5, 4, 7, 8],
    [4, 1, 6, 7, 5, 2, 4, 6, 4, 5],
    [2, 1, 7, 6, 8, 4, 1, 7, 2, 1],
    [6, 8, 8, 2, 8, 8, 1, 1, 3, 4],
    [4, 8, 4, 6, 8, 4, 8, 5, 5, 4],
    [5, 2, 8, 3, 7, 5, 1, 5, 2, 6]
  ]

type GameGrid = LGridMap RectOctGrid (Sum Int, Int)

inputParser :: Parsec Void Text GameGrid
inputParser = createGrid . filter (not . Relude.null) <$> Text.Megaparsec.many (digitToInt <$> digitChar) `sepEndBy1` newline

createGrid :: [[Int]] -> GameGrid
createGrid ts = lazyGridMap (rectOctGrid rows columns) ((mempty,) <$> join ts)
  where
    columns = length ts
    rows = length $ Unsafe.head ts

evaluate1 :: GameGrid -> _
evaluate1 grid = getSum $ foldMap fst $ G.elems $ foldl' (\lgm _ -> step lgm) grid [1 .. 100 :: Int]

step :: GameGrid -> GameGrid
step = flash . increase

increase :: GameGrid -> GameGrid
increase = G.map (fmap (+ 1))

flash :: GameGrid -> GameGrid
flash g = G.map (fmap (\i -> if (i < 0) || (i > 9) then 0 else i)) $ flash' g

flash' :: GameGrid -> GameGrid
flash' g = reflash $ foldl' (\lgm x0 -> if snd (lgm ! x0) > 9 then adjust (\(si, _) -> (Sum 1 <> si, -1)) x0 (foldl' (\g ix -> adjust (fmap (\i -> if i >= 0 then i + 1 else i)) ix g) lgm (neighbours lgm x0)) else lgm) g (indices g)
  where
    reflash grd = if any (\(_, i) -> i > 9) grd then flash' grd else grd

evaluate2 :: GameGrid -> _
evaluate2 grid = Unsafe.head $ fst <$>( take 1 $ dropWhile (\ (i, g) -> not (all (\(_, i) -> i == 0) g)) $ scanl' (\(_, lgm) i -> (i, step lgm)) (0, grid) [1 :: Int ..])

run :: IO ()
run = do
  inputRaw <- readFileText "inputs/day11.txt"

  case parseMaybe inputParser inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 $ createGrid exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 1656)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 1719)

      putStrLn "=== Part 2"
      let solution4 = evaluate2 $ createGrid exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4
      putStrLn $ "Solution is correct for example input: " <> show (solution4 == 195)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 949905)
