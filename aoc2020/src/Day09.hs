{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day09 where

import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

parseInput :: Parser [Int]
parseInput = L.decimal `sepBy1` M.newline

windows :: Int -> [a] -> [[a]]
windows n xs = filter (\l -> length l == n) $ map (reverse . take n) (tails xs)

example :: [Int]
example = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

sumOf2s :: [Int] -> [Int]
sumOf2s xs = [x + x' | x <- xs, x' <- xs, x /= x']

containsSumOf2sInTail :: [Int] -> Bool
containsSumOf2sInTail [] = False
containsSumOf2sInTail (a : as) = a `elem` sumOf2s as

evaluate1 :: Int -> [Int] -> Maybe Int
evaluate1 preambleSize xs = find (not . containsSumOf2sInTail) (windows (preambleSize + 1) xs) >>= listToMaybe

evaluate2 :: Int -> [Int] -> Maybe Int
evaluate2 x xs = sumOfMinMax <$> find (\s -> x == sum s) (combs xs)

-- | this function is _really_ inefficient
combs :: [Int] -> [[Int]]
combs xs = ordNub $ (xs : tails xs) >>= \xxs -> inits xxs

sumOfMinMax :: [Int] -> Int
sumOfMinMax = (\(a, b) -> unMin a + unMax b) . foldMap (\i -> (Min i, Max i))

newtype Min = Min {unMin :: Int} deriving newtype (Eq, Ord, Show)

newtype Max = Max {unMax :: Int} deriving newtype (Eq, Ord, Show)

instance Semigroup Min where
  (<>) = min

instance Monoid Min where
  mempty = Min maxInt

instance Semigroup Max where
  (<>) = max

instance Monoid Max where
  mempty = Max minInt

run :: IO ()
run = readFile "inputs/day9-2.txt" >>= \s -> print (M.parseMaybe parseInput s >>= evaluate2 57195069)
