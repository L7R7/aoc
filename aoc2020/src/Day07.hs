{-# LANGUAGE ScopedTypeVariables #-}

module Day07 where

import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

example :: [String]
example =
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  ]

type Parser = M.Parsec Void String

parseExample :: [(String, [String])]
parseExample = catMaybes $ M.parseMaybe lineParser <$> example

inputParser :: Parser [(String, [String])]
inputParser = lineParser `sepBy1` M.newline

lineParser :: Parser (String, [String])
lineParser = (,) <$> colorParser <* M.string " bags contain " <*> bagContentParser

colorParser :: Parser String
colorParser = (\p1 p2 -> p1 <> " " <> p2) <$> M.some M.letterChar <* M.spaceChar <*> M.some M.letterChar

bagContentParser :: Parser [String]
bagContentParser = (emptyBagParser <|> notEmptyBagParser) <* M.string "."
  where
    emptyBagParser = M.string "no other bags" >> pure []
    single = do
      (i :: Int) <- L.decimal
      _ <- M.spaceChar
      c <- colorParser
      _ <- M.spaceChar
      b <- M.string "bags" <|> M.string "bag"
      guard $ (i == 1 && b == "bag") || (i > 1 && b == "bags")
      pure c
    notEmptyBagParser = single `sepBy1` M.string ", "

evaluate1 :: String -> Maybe Int
evaluate1 bla = filterResult <$> M.parseMaybe inputParser bla

filterResult :: (Ord a, IsString a) => [(a, [a])] -> Int
filterResult input = length . ordNub $ filter ("shiny gold" /=) $ filterResult' ["shiny gold"] input

filterResult' :: Ord a => [a] -> [(a, [a])] -> [a]
filterResult' [] _ = []
filterResult' as aas = res <> if as == res then [] else filterResult' res aas
  where
    res = ordNub $ as <> new
    new = ordNub (fst <$> filter f aas)
    f (a', as') = or $ (`elem` as) <$> (a' : as')

run :: IO ()
run = readFile "inputs/day7.txt" >>= print . evaluate1
