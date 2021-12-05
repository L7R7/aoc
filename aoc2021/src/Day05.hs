{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.Map (fromListWith)
import qualified Data.Map
import Relude hiding (init)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

example :: [Text]
example =
  [ "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  ]

exampleParsed :: [Input]
exampleParsed =
  [ Input (Point 0 9) (Point 5 9),
    Input (Point 8 0) (Point 0 8),
    Input (Point 9 4) (Point 3 4),
    Input (Point 2 2) (Point 2 1),
    Input (Point 7 0) (Point 7 4),
    Input (Point 6 4) (Point 2 0),
    Input (Point 0 9) (Point 2 9),
    Input (Point 3 4) (Point 1 4),
    Input (Point 0 0) (Point 8 8),
    Input (Point 5 5) (Point 8 2)
  ]

parseInput :: Parsec Void Text Input
parseInput = Input <$> pointParser <* string " -> " <*> pointParser
  where
    pointParser = Point <$> decimal <* string "," <*> decimal

data Point = Point
  { xVal :: Int,
    yVal :: Int
  }
  deriving (Eq, Ord, Show)

data Input = Input
  { from :: Point,
    to :: Point
  }
  deriving (Eq, Show)

vertical :: Input -> Bool
vertical (Input (Point x _) (Point x' _)) = x == x'

horizontal :: Input -> Bool
horizontal (Input (Point _ y) (Point _ y')) = y == y'

evaluate1 :: [Input] -> _
evaluate1 inputs = length $ Data.Map.filter (\(Sum i) -> i > 1) $ fromListWith (<>) $ (,Sum (1 :: Int)) <$> (filter ((||) <$> horizontal <*> vertical) inputs >>= line)
  where
    line :: Input -> [Point]
    line (Input (Point x y) (Point x' y'))
      | x == x' = Point x <$> [(min y y') .. (max y y')]
      | y == y' = (`Point` y) <$> [(min x x') .. (max x x')]
      | otherwise = []

evaluate2 :: [Input] -> _
evaluate2 = undefined

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (traverse (parseMaybe parseInput) example == Just exampleParsed)
  inputRaw <- lines <$> readFileText "inputs/day05.txt"

  case traverse (parseMaybe parseInput) inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 5)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 5280)

-- putStrLn "=== Part 2"
-- let solution4 = evaluate2 exampleParsed
-- putStrLn $ "Solution for example is: " <> show solution4

-- putStrLn $ "Solution is correct for example input: " <> show (solution4 == Just 1924)
-- let solution5 = evaluate2 input
-- putStrLn $ "Solution for input is: " <> show solution5
-- putStrLn $ "Solution is correct for input: " <> show (solution5 == Just 1827)
