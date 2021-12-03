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

evaluate1 :: [Command] -> Int
evaluate1 cs = endHorizontal * endDepth
  where
    (endHorizontal, endDepth) = flipfoldl' f (0, 0) cs
    f (Forward i) = first (i +)
    f (Down i) = second (i +)
    f (Up i) = second (\d -> d - i)

evaluate2 :: [Command] -> Int
evaluate2 cs = endHorizontal * endDepth
  where
    ((endHorizontal, endDepth), _) = flipfoldl' g ((0, 0), 0) cs
    g (Forward i) p@(_, a) = first (bimap (i +) (\d -> d + a * i)) p
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
