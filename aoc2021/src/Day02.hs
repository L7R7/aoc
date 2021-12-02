{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import Relude hiding (Down)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

example :: [Text]
example = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

exampleParsed :: [Command]
exampleParsed = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

data Command = Forward Int | Down Int | Up Int deriving (Eq, Show)

parseInputLine :: Parsec Void Text Command
parseInputLine =
  Forward <$> (string "forward " *> decimal)
    <|> Down <$> (string "down " *> decimal)
    <|> Up <$> (string "up " *> decimal)

data Position a b = Position {horizontal :: a, depth :: b} deriving (Eq, Show)

instance Bifunctor Position where
  bimap fab fcd (Position a b) = Position (fab a) (fcd b)

evaluate1 :: [Command] -> Int
evaluate1 cs = endHorizontal * endDepth
  where
    (Position endHorizontal endDepth) = flipfoldl' f (Position 0 0) cs

f :: Command -> Position Int Int -> Position Int Int
f (Forward i) = first (i +)
f (Down i) = second (i +)
f (Up i) = second (\d -> d - i)

data Position2 a = Position2 {position :: Position Int Int, aim :: a} deriving (Functor)

evaluate2 :: [Command] -> Int
evaluate2 cs = endHorizontal * endDepth
  where
    (Position2 (Position endHorizontal endDepth) _) = flipfoldl' g (Position2 (Position 0 0) 0) cs

g :: Command -> Position2 Int -> Position2 Int
g (Forward i) (Position2 p a) = Position2 (bimap (i +) (\d -> d + a * i) p) a
g (Down i) p = (+ i) <$> p
g (Up i) p = (\a -> a - i) <$> p

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (traverse (parseMaybe parseInputLine) example == Just exampleParsed)
  inputRaw <- lines <$> readFileText "inputs/day02.txt"

  case traverse (parseMaybe parseInputLine) inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 150)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 2147104)

      putStrLn "=== Part 2"
      let solution3 = evaluate2 exampleParsed
      putStrLn $ "Solution is correct for example input: " <> show (solution3 == 900)
      let solution4 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution4
      putStrLn $ "Solution is correct for input: " <> show (solution4 == 2044620088)
