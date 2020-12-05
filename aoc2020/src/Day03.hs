module Day03 where

import Data.List.Split (splitOn)
import Relude

example :: [String]
example =
  [ "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ]

evaluate1 :: [String] -> Int
evaluate1 xs = evaluate1' (cycle <$> xs) 0 0

type Index = Int

evaluate1' :: [String] -> Index -> Int -> Int
evaluate1' [] _ acc = acc
evaluate1' (x : xs) i acc = evaluate1' xs (i + 3) (if x !!? i == Just '#' then acc + 1 else acc)

-- | equivalent solution for the first challenge using the solution from the second one
evaluate1'' :: [String] -> Product Trees
evaluate1'' = evaluate2 [(3, 1, 0, 0)]

-- | In which row are we?
type Row = Int

-- | In which column are we?
type Column = Int

-- | How many trees did we encounter so far?
type Trees = Int

-- | How much do we go right?
type ColumnStep = Int

-- | How much do we go down?
type RowStep = Int

type Result =
  ( ColumnStep, -- This one will just be carried through
    RowStep, -- This one will just be carried through
    Column, -- this carries the current column index
    Trees -- this counts how many trees have been encountered so far
  )

-- | These are the initial values given by the challenge
-- Note that the third and fourth values are always zero
-- The actual configuration is done in the first and the second value
initialValues :: [Result]
initialValues =
  [ (1, 1, 0, 0), -- Right 1, down 1. == 2
    (3, 1, 0, 0), -- Right 3, down 1. == 7 (This is the slope you already checked.)
    (5, 1, 0, 0), -- Right 5, down 1. == 3
    (7, 1, 0, 0), -- Right 7, down 1. == 4
    (1, 2, 0, 0) --  Right 1, down 2. == 2 <-- this one is particularly interesting because we go down 2 steps
  ]

--evaluate2 :: Int -> Int -> [String] -> Trees
--evaluate2 a b xs = (\(_, _, _, t) -> t) $ foldl' evaluate2' (a, b, 0, 0) xsWithIndex
--  where
--    xsWithIndex = zipWith (\a b -> (a, b)) (cycle <$> xs) [0 ..]

-- | This computes the result for a given list of initialResults
-- It does the following steps
-- * add an index to the input lines. This is necessary to be able to cover the case where the downward step is larger than 1
--   (does this mean that we have to traverse the input list twice? Probably, I'm not sure)
--   When zipping, each input lines is turned into an infinite list to respect the repeating nature of the pattern
-- * It folds over the input list, using the list of results as accumulator.
--   The first two values of the tuple stay unchanged as they define the slope.
--   It's necessary to carry them through, because we have a list of tuples where each element has its own configuration
-- * The result of the fold contains the number of trees for each slope. We have to multiply them.
--   To do that we turn the last element into a Product and use foldMap to multiply them together
evaluate2 :: [Result] -> [String] -> Product Trees
evaluate2 initialResults xs = foldMap (\(_, _, _, t) -> Product t) $ foldl' (\accs r -> evaluate2' r <$> accs) initialResults xsWithIndex
  where
    xsWithIndex = zipWith (\a b -> (a, b)) (cycle <$> xs) [0 ..]

evaluate2' :: (String, Row) -> Result -> Result
evaluate2' (line, row) (columnStep, rowStep, column, trees)
  | row `mod` rowStep == 0 = (columnStep, rowStep, column + columnStep, if line !!? column == Just '#' then trees + 1 else trees)
  | otherwise = (columnStep, rowStep, column, trees)

run :: IO ()
run = readFile "inputs/day3.txt" >>= print . evaluate2 initialValues . splitOn "\n"
