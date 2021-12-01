{-# LANGUAGE TupleSections #-}

module Day13_2 where

import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

inputParser :: Parser Input
inputParser = do
  x <- L.decimal
  _ <- M.newline
  busLines <- zip [0 ..] <$> busLineParser `sepBy1` M.char ','
  pure $ Input x (catMaybes $ f <$> busLines)
  where
    busLineParser = M.string "x" <|> M.many M.digitChar
    f :: (Offset, String) -> Maybe (Line, Offset)
    f (i, cs) = (,i) <$> readMaybe cs

parseInput :: String -> Maybe Input
parseInput = M.parseMaybe inputParser

type Line = Int

type Offset = Int

data Input = Input Int [(Line, Offset)] deriving (Show)

evaluate2 :: Input -> Maybe Line
evaluate2 (Input startTime busLines) = find timeWithBusLinesStarting [startTime ..]
  where
    timeWithBusLinesStarting time = all (\(l, o) -> (time + o) `mod` l == 0) busLines

run :: IO ()
run = readFile "inputs/day13-2.txt" >>= \s -> print (parseInput s >>= evaluate2)
