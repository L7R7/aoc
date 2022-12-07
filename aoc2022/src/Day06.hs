module Day06 (run) where

import Data.List (findIndex, zipWith5)
import Relude hiding (many)

evaluate1 :: String -> Maybe Integer
evaluate1 txt = join $ find isJust $ zipWith5 f [4 ..] txt (drop 1 txt) (drop 2 txt) (drop 3 txt)
  where
    f :: Integer -> Char -> Char -> Char -> Char -> Maybe Integer
    f i a b c d
      | ordNub [a, b, c, d] == [a, b, c, d] = Just i
      | otherwise = Nothing

evaluate2 :: String -> Maybe Int
evaluate2 s = (+ 14) <$> findIndex id ((\cs -> ordNub cs == cs) <$> windows 14 s)
  where
    windows n xs = transpose (take n (tails xs))

run :: IO ()
run = do
  let examples =
        [ "inputs/06-example-1.txt",
          "inputs/06-example-2.txt",
          "inputs/06-example-3.txt",
          "inputs/06-example-4.txt",
          "inputs/06-example-5.txt"
        ]
  exampleInputs <- traverse readFile examples
  print $ evaluate1 <$> exampleInputs

  input1 <- readFile "inputs/06.txt"
  print $ evaluate1 input1

  print $ evaluate2 <$> exampleInputs

  print $ evaluate2 input1
