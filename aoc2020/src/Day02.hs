{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Day02 where

import Relude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

example :: [Text]
example = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]

evaluate1 :: [Text] -> Maybe Int
evaluate1 = evaluate isValid1

evaluate2 :: [Text] -> Maybe Int
evaluate2 = evaluate isValid2

evaluate :: (InputLine -> Bool) -> [Text] -> Maybe Int
evaluate f inputs = do
  parseResult <- sequence $ parseMaybe parseInputLine <$> inputs
  pure $ length $ filter f parseResult

data InputLine = InputLine
  { min :: Int,
    max :: Int,
    char :: Char,
    password :: String
  }
  deriving (Show)

parseInputLine :: Parsec Void Text InputLine
parseInputLine = do
  min <- decimal
  _ <- Text.Megaparsec.Char.char '-'
  max <- decimal
  _ <- spaceChar
  char <- alphaNumChar
  _ <- string ": "
  password <- Text.Megaparsec.many alphaNumChar
  pure InputLine {..}

isValid1 :: InputLine -> Bool
isValid1 InputLine {..} = numChar <= max && numChar >= min
  where
    numChar = length $ filter (char ==) password

isValid2 :: InputLine -> Bool
isValid2 InputLine {..} = ((char ==) <$> password !!? (min - 1)) /= ((char ==) <$> password !!? (max - 1))

run :: IO ()
run = do
  inputRaw <- lines <$> readFileText "inputs/day2.txt"
  print $ evaluate2 inputRaw
