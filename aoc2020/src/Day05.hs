{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day05 where

import Relude hiding (tail)
import Text.Megaparsec
import Text.Megaparsec.Char

example :: [Input]
example =
  [ Input [F, B, F, B, B, F, F] [R, L, R], -- row 44, column 5, seat ID 357
    Input [B, F, F, F, B, B, F] [R, R, R], -- row 70, column 7, seat ID 567
    Input [F, F, F, B, B, B, F] [R, R, R], -- row 14, column 7, seat ID 119
    Input [B, B, F, F, B, B, F] [R, L, L] -- row 102, column 4, seat ID 820
  ]

data Input = Input {row :: [RowIndicator], column :: [ColumnIndicator]}

data RowIndicator = F | B

data ColumnIndicator = L | R

evaluate1 :: Text -> Maximum Int
evaluate1 = foldMap (Maximum . seatId) . parseInput

parseInput :: Text -> [Input]
parseInput = fromMaybe [] . parseMaybe parseInputLines

type Parser = Parsec Void Text

parseInputLines :: Parser [Input]
parseInputLines = inputParser `sepBy` newline

inputParser :: Parser Input
inputParser = do
  rows <- count 7 $ string "F" <|> string "B"
  columns <- count 3 $ string "L" <|> string "R"
  pure $ Input (toRowIndicator <$> rows) (toColumnIndicator <$> columns)
  where
    toRowIndicator "F" = F
    toRowIndicator "B" = B
    toColumnIndicator "L" = L
    toColumnIndicator "R" = R

-- This might not be the most concise way of getting the maximum of the list
-- But it allows us to calculate the seat ID and the maximum in one go
newtype Maximum a = Maximum a
  deriving newtype (Bounded, Eq, Ord, Show)

instance Ord a => Semigroup (Maximum a) where
  (<>) = max

instance (Ord a, Bounded a) => Monoid (Maximum a) where
  mempty = minBound

seatId :: Input -> Int
seatId (Input rows columns) = rowNumber rows * 8 + columnNumber columns

rowNumber :: [RowIndicator] -> Int
rowNumber =
  fst . foldl' step (0, 127)
  where
    step :: (Int, Int) -> RowIndicator -> (Int, Int)
    step (lower, upper) r = case r of
      F -> (lower, lower + (upper - lower) `div` 2)
      B -> (lower + (upper - lower) `div` 2 + 1, upper)

columnNumber :: [ColumnIndicator] -> Int
columnNumber =
  fst . foldl' step (0, 7)
  where
    step :: (Int, Int) -> ColumnIndicator -> (Int, Int)
    step (lower, upper) r = case r of
      L -> (lower, lower + (upper - lower) `div` 2)
      R -> (lower + (upper - lower) `div` 2 + 1, upper)

evaluate2 :: Text -> Maybe Int
evaluate2 t = findGap $ seatId <$> parseInput t

-- | This function assumes that the input list contains only elements that are direct neighbours
-- Except for one, where the gap is 2 (one number is missing). This is the number we're looking for
findGap :: [Int] -> Maybe Int
findGap = go . sort
  where
    go :: [Int] -> Maybe Int
    go [] = Nothing
    go [a] = Just a
    go as@(a : a' : _) = if a' - a > 1 then Just (a' - 1) else go (tail as)

-- | Because Relude only contains a tail function for NonEmpty
tail :: [a] -> [a]
tail [] = []
tail (_ : as) = as

run :: IO ()
run = readFileText "inputs/day5.txt" >>= print . evaluate2
