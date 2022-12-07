module Day06 (run) where

import Data.List (findIndex)
import Relude hiding (many)

evaluate1 :: String -> Maybe Int
evaluate1 = evaluate 4

evaluate2 :: String -> Maybe Int
evaluate2 = evaluate 14

evaluate :: Int -> String -> Maybe Int
evaluate i s = (+ i) <$> findIndex id ((\cs -> ordNub cs == cs) <$> windows i s)
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
