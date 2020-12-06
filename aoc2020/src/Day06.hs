{-# LANGUAGE TupleSections #-}

module Day06 where

import Data.List.Split (splitOn)
import qualified Data.Map as M (fromList, unionWith)
import Relude

evaluate1 :: [String] -> Int
evaluate1 input = getSum $ foldMap Sum $ countDistinct . removeLinebreaks <$> input
  where
    removeLinebreaks = filter (/= '\n')
    countDistinct = length . ordNub

evaluate2 :: [String] -> Int
evaluate2 inputs = getSum $ foldMap Sum $ evaluate2' . splitByPeople <$> inputs

splitByPeople :: String -> [String]
splitByPeople = splitOn "\n"

evaluate2' :: [String] -> Int
evaluate2' inputs = length . filter (== numberOfPeople) . combineOccurrences . countOccurrences $ inputs
  where
    numberOfPeople = length inputs

countOccurrences :: [String] -> [Map Char Int]
countOccurrences = fmap (M.fromList . map (,1))

combineOccurrences :: [Map Char Int] -> [Int]
combineOccurrences = toList . foldl' (M.unionWith (+)) mempty

run :: IO ()
run = readFile "inputs/day6-3.txt" >>= print . evaluate2 . splitOn "\n\n"
