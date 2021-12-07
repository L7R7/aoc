{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day06 (run) where

import qualified Data.List
import qualified Data.Map as M
import Relude hiding (init)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

example :: Text
example = "3,4,3,1,2"

exampleParsed :: [Int]
exampleParsed = [3, 4, 3, 1, 2]

parseInput :: Parsec Void Text [Int]
parseInput = decimal `sepBy` char ','

evaluate1 :: [Int] -> Int
evaluate1 inputs = length $ Data.List.last $ take 80 $ evaluateX inputs
  where
    evaluateX ins = let res = ins >>= f in res : evaluateX res
    f 0 = [6, 8]
    f i = [i - 1]

evaluate1' :: [Int] -> Int
evaluate1' = evaluate 80

evaluate2 :: [Int] -> Int
evaluate2 = evaluate 256

evaluate :: Int -> [Int] -> Int
evaluate n inputs = getSum $ fold $ M.elems $ Data.List.last $ take n $ evaluateX start
  where
    start = M.fromListWith (<>) $ ((,Sum 0) <$> [0 .. 8]) <> ((,Sum 1) <$> inputs)
    evaluateX ins = let res = f ins in res : evaluateX res
    f m | null m = M.empty
    f s = M.fromListWith (<>) $ zip [0 .. 8] xs <> [(6, x), (8, x)]
      where
        (x : xs) = M.elems s

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (parseMaybe parseInput example == Just exampleParsed)
  inputRaw <- readFileText "inputs/day06.txt"

  case parseMaybe parseInput inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 5934)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 380612)
      let solution3 = evaluate1' input
      putStrLn $ "Solution with second attempt for input is: " <> show solution3
      putStrLn $ "Solution with second attempt is correct for input: " <> show (solution3 == 380612)

      putStrLn "=== Part 2"
      let solution4 = evaluate2 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4
      putStrLn $ "Solution is correct for example input: " <> show (solution4 == 26984457539)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 1710166656900)
