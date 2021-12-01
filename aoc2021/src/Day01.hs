{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day01 where

import Data.List (zipWith3)
import Data.List.Split
import Relude

example :: [Int]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

evaluate1 :: [Int] -> Int
evaluate1 xs = getSum $ foldMap f (zip xs (drop 1 xs))

f :: (Ord a, Num b) => (a, a) -> Sum b
f (a, b)
  | b > a = Sum 1
  | otherwise = Sum 0

evaluate2 :: [Int] -> Int
evaluate2 xs = evaluate1 $ zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

run :: IO ()
run = do
  inputRaw <- splitOn "\n" <$> readFile "inputs/day01.txt"
  let input = catMaybes $ readMaybe <$> inputRaw
  putStrLn "=== Part 1"
  let solution1 = evaluate1 example
  putStrLn $ "Solution is correct for example input: " <> show (solution1 == 7)
  let solution2 = evaluate1 input
  print solution2
  putStrLn $ "Solution is correct for input: " <> show (solution2 == 1791)
  putStrLn "=== Part 2"
  let solution3 = evaluate2 example
  putStrLn $ "Solution is correct for example input: " <> show (solution3 == 5)
  let solution4 = evaluate2 input
  print solution4
  putStrLn $ "Solution is correct for input: " <> show (solution4 == 1822)
