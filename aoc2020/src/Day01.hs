module Day01 where

import Data.List.Split
import Relude

example :: [Int]
example = [1721, 979, 366, 299, 675, 1456]

-- this solution will be incorrect if the input contains 1010
evaluate1 :: [Integer] -> Maybe Integer
evaluate1 xs = listToMaybe [x * y | x <- xs, y <- xs, x + y == 2020]

evaluate2 :: [Integer] -> Maybe Integer
evaluate2 xs = listToMaybe [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

run :: IO ()
run = do
  inputRaw <- splitOn "\n" <$> readFile "inputs/day1.txt"
  let input = catMaybes $ readMaybe <$> inputRaw
  case evaluate2 input of
    Nothing -> print ("No result found" :: String)
    Just i -> print i
