{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day10 where

import Data.List (maximum)
import Data.Map (insertWith, lookup, singleton)
import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

newtype Adapter = Adapter Int deriving newtype (Enum, Eq, Num, Ord)

parseInput :: Parser [Adapter]
parseInput = fmap Adapter <$> L.decimal `sepBy1` M.newline

-- | Sort the input list ascending, then count how many single steps and how many 3-steps we encounter
--   There might be a more elegant solution using some kind of sliding windows function
evaluate1 :: [Adapter] -> Int
evaluate1 xs = result $ foldl' step (0, 0, 1) (sort xs)
  where
    result (_, a, b) = a * b
    step (lastElement, ones, threes) i
      | i - lastElement == 1 = (i, ones + 1, threes)
      | i - lastElement == 3 = (i, ones, threes + 1)
      | otherwise = (i, ones, threes)

-- | This one sorts the inpt list descending and adds 0 (the starting point)
--   as well as the maximum element of the list +3 (which is the ending point)
--   It goes through the list and builds up a map from Adapter to Int
--
--   This map indicates how many ways there are from the key to the ending point
--   (this also explains why fold starts with a map that contains one entry for the ending point)
--
--   For each step, it will check how many connections the map contains for the current Adapter:
--   A connection to the end can be established if there's an Adapter that's 1,2 or 3 away
--   It will sum up the number of these connections
--
--   After going through the whole list, the map entry for 0 gives us the desired result
evaluate2 :: [Adapter] -> Maybe Int
evaluate2 xs = getSum <$> lookup 0 (go (singleton end 1) input)
  where
    end = maximum xs + 3
    input = reverse . sort $ 0 : end : xs

    go :: Map Adapter (Sum Int) -> [Adapter] -> Map Adapter (Sum Int)
    go acc [] = acc
    go acc (i : is) = go (insertWith (<>) i s acc) is
      where
        s = fold $ catMaybes $ (\p -> lookup (i + p) acc) <$> [1 .. 3]

run :: IO ()
run =
  readFile "inputs/day10-2.txt" >>= \s -> print (M.parseMaybe parseInput s >>= evaluate2)
