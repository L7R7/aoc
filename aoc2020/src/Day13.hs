module Day13 where

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
  busLines <- busLineParser `sepBy1` M.char ','
  pure $ Input x (catMaybes busLines)
  where
    busLineParser = (\l -> if l == "x" then Nothing else readMaybe l) <$> (M.string "x" <|> M.many M.digitChar)

parseInput :: String -> Maybe Input
parseInput = M.parseMaybe inputParser

data Input = Input Int [Int]

evaluate1 :: Input -> Maybe Int
evaluate1 (Input startTime busLines) = find hasMatchingBusLine (timeWithBusLinesStarting <$> [startTime ..]) >>= multiplyResult
  where
    hasMatchingBusLine = (== 1) . length . snd
    timeWithBusLinesStarting time = (time, filter (\l -> time `mod` l == 0) busLines)
    multiplyResult (time, ls) = (* (time - startTime)) <$> viaNonEmpty head ls

run :: IO ()
run = readFile "inputs/day13-2.txt" >>= \s -> print (parseInput s >>= evaluate1)
