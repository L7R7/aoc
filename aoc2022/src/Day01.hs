module Day01 (run) where

import Data.Foldable (Foldable (maximum))
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Relude
import Relude.Unsafe (read)
import qualified Prelude

type Elve = [Int]

type Input = NonEmpty Elve

parseFile :: FilePath -> IO (Maybe Input)
parseFile fp = fmap parse . nonEmpty . Prelude.lines <$> readFile fp

parse :: NonEmpty String -> Input
parse inputs = foldl' f ([read frstLine] :| []) tailLines
  where
    frstLine = head inputs
    tailLines = tail inputs

    f :: Input -> String -> Input
    f xs "" = [] <| xs
    f xs s = (read @Int s : head xs) :| tail xs

solution1 :: Input -> Int
solution1 = maximum . fmap sum

solution2 :: Input -> Int
solution2 = sum . NE.take 3 . NE.reverse . NE.sort . fmap sum

run :: IO ()
run = do
  inputExample <- parseFile "inputs/01-example.txt"
  case inputExample of
    Nothing -> putStrLn "empty or unparsable example input"
    Just input -> do
      putStrLn $ "example can be parsed: " <> show (input == [10000] :| [[9000, 8000, 7000], [6000, 5000], [4000], [3000, 2000, 1000]])
      putStrLn $ "solution 1 for example: " <> show (solution1 input)
      putStrLn $ "solution 2 for example: " <> show (solution2 input)

  input1 <- parseFile "inputs/01-1.txt"
  case input1 of
    Nothing -> putStrLn "empty or unparsable input"
    Just input -> do
      putStrLn $ "solution 1: " <> show (solution1 input)
      putStrLn $ "solution 2: " <> show (solution2 input)
