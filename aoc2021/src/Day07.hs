module Day07 where

import Data.Foldable (maximum, minimum)
import Relude hiding (init)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

example :: Text
example = "16,1,2,0,4,2,7,1,2,14"

exampleParsed :: [Int]
exampleParsed = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

parseInput :: Parsec Void Text [Int]
parseInput = decimal `sepBy` char ','

evaluate1 :: [Int] -> Int
evaluate1 inputs = minimum $ sum . (\c -> (\i -> abs (c - i)) <$> inputs) <$> candidates
  where
    mn = minimum inputs
    mx = maximum inputs
    candidates = [mn .. mx]

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (parseMaybe parseInput example == Just exampleParsed)
  inputRaw <- readFileText "inputs/day07.txt"

  case parseMaybe parseInput inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 37)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 335271)
