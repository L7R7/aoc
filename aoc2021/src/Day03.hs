{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day03 where

import Relude hiding (Down)
import Relude.Extra (toSnd)
import Text.Megaparsec
import qualified Text.Megaparsec as T
import Text.Megaparsec.Char

example :: [Text]
example =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

exampleParsed :: [InputLine]
exampleParsed =
  [ [O, O, I, O, O],
    [I, I, I, I, O],
    [I, O, I, I, O],
    [I, O, I, I, I],
    [I, O, I, O, I],
    [O, I, I, I, I],
    [O, O, I, I, I],
    [I, I, I, O, O],
    [I, O, O, O, O],
    [I, I, O, O, I],
    [O, O, O, I, O],
    [O, I, O, I, O]
  ]

data Input = I | O deriving (Eq, Show)

type InputLine = [Input]

parseInputLine :: Parsec Void Text InputLine
parseInputLine = T.some $ I <$ char '1' <|> O <$ char '0'

evaluate1 :: [InputLine] -> Int
evaluate1 = uncurry (*) . bimap toDecimal toDecimal . toSnd flipped . selectMaximum . fmap fold . transpose . fmap countBits

flipped :: InputLine -> InputLine
flipped = fmap $ \case
  I -> O
  O -> I

countBits :: InputLine -> [(Sum Int, Sum Int)]
countBits = fmap $ \case
  I -> (Sum 1, Sum 0)
  O -> (Sum 0, Sum 1)

selectMaximum :: [(Sum Int, Sum Int)] -> InputLine
selectMaximum = fmap $ \case
  (Sum i, Sum o)
    | i > o -> I
    | otherwise -> O

toDecimal :: InputLine -> Int
toDecimal = foldl' f 0
  where
    f acc O = acc * 2
    f acc I = acc * 2 + 1

evaluate2 :: [InputLine] -> Int
evaluate2 is = 0

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (traverse (parseMaybe parseInputLine) example == Just exampleParsed)
  inputRaw <- lines <$> readFileText "inputs/day03.txt"

  case traverse (parseMaybe parseInputLine) inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      print solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 198)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 693486)

      putStrLn "=== Part 2"
      let solution3 = evaluate2 exampleParsed
      putStrLn $ "Solution is correct for example input: " <> show (solution3 == 900)
      let solution4 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution4
      putStrLn $ "Solution is correct for input: " <> show (solution4 == 2044620088)
