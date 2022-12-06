module Day03 (run) where

import Data.Char (isUpper)
import Relude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char

parseInput1 :: Parsec Void Text [Input1]
parseInput1 = parseInput1Line `sepBy` newline

parseInput1Line :: Parsec Void Text Input1
parseInput1Line = uncurry Input1 . (\s -> let len = length s `div` 2 in splitAt len s) <$> many letterChar

data Input1 = Input1 String String deriving stock (Show)

evaluate1 :: [Input1] -> Sum Int
evaluate1 = foldMap (maybe mempty (Sum . prio) . findItem)
  where
    findItem :: Input1 -> Maybe Char
    findItem (Input1 left right) = find (`elem` left) right

prio :: Char -> Int
prio c
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

data Input2 = Input2 String String String deriving stock (Show)

parseInput2 :: Parsec Void Text [Input2]
parseInput2 = parseInput2Line `sepBy` newline

parseInput2Line :: Parsec Void Text Input2
parseInput2Line = Input2 <$> line <* newline <*> line <* newline <*> line
  where
    line = many letterChar

evaluate2 :: [Input2] -> Sum Int
evaluate2 = foldMap (maybe mempty (Sum . prio) . findItem)
  where
    findItem :: Input2 -> Maybe Char
    findItem (Input2 left middle right) = find (\c -> c `elem` left && c `elem` middle) right

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/03-example.txt"
  let exampleParsed1 = parseMaybe parseInput1 exampleInputRaw
  print $ traverse evaluate1 exampleParsed1

  inputRaw <- readFileText "inputs/03.txt"
  let parsed1 = parseMaybe parseInput1 inputRaw
  print $ traverse evaluate1 parsed1

  let exampleParsed2 = parseMaybe parseInput2 exampleInputRaw
  print $ traverse evaluate2 exampleParsed2

  let parsed2 = parseMaybe parseInput2 inputRaw
  print $ traverse evaluate2 parsed2
