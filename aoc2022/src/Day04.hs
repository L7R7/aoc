module Day04 (run) where

import Relude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parsec Void Text [Input1]
parseInput = parseInputLine `sepBy` newline

parseInputLine :: Parsec Void Text Input1
parseInputLine = Input1 <$> pair <* char ',' <*> pair
  where
    pair = (,) <$> decimal <* char '-' <*> decimal

data Input1 = Input1 (Int, Int) (Int, Int) deriving stock (Show)

evaluate1 :: [Input1] -> Int
evaluate1 xs = length $ filter overlaps xs
  where
    overlaps :: Input1 -> Bool
    overlaps (Input1 (l1, r1) (l2, r2)) = (l1 <= l2 && r1 >= r2) || (l2 <= l1 && r2 >= r1)

evaluate2 :: [Input1] -> Int
evaluate2 xs = length $ filter overlaps xs
  where
    overlaps :: Input1 -> Bool
    overlaps (Input1 (l1, r1) (l2, r2)) = (r1 >= l2 && l1 <= r2) || (l1 >= r2 && r1 <= l2)

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/04-example.txt"
  let exampleParsed = parseMaybe parseInput exampleInputRaw
  print $ evaluate1 <$> exampleParsed

  inputRaw <- readFileText "inputs/04.txt"
  let parsed = parseMaybe parseInput inputRaw
  print $ evaluate1 <$> parsed

  print $ evaluate2 <$> exampleParsed

  print $ evaluate2 <$> parsed
