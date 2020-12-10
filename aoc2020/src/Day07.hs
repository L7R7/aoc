{-# LANGUAGE ScopedTypeVariables #-}

module Day07 where

import Data.Graph.DGraph
import qualified Data.Graph.DGraph as G (transpose)
import Data.Graph.Traversal (bfsVertices)
import Data.Graph.Types
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

exampleGraph :: DGraph String Int
exampleGraph =
  fromArcsList
    [ -- "light red bags contain 1 bright white bag, 2 muted yellow bags.",
      Arc "bright white" "light red" 1,
      Arc "muted yellow" "light red" 2,
      -- "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      Arc "bright white" "dark orange" 3,
      Arc "muted yellow" "dark orange" 4,
      -- "bright white bags contain 1 shiny gold bag.",
      Arc "shiny gold" "bright white" 1,
      -- "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      Arc "shiny gold" "muted yellow" 2,
      Arc "faded blue" "muted yellow" 2,
      -- "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      Arc "dark olive" "shiny gold" 1,
      Arc "vibrant plum" "shiny gold" 2,
      -- "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      Arc "faded blue" "dark olive" 3,
      Arc "dotted black" "dark olive" 4,
      -- "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      Arc "faded blue" "vibrant plum" 5,
      Arc "dotted black" "vibrant plum" 6
      -- "faded blue bags contain no other bags.",
      -- "dotted black bags contain no other bags."
    ]

type Parser = M.Parsec Void String

parseExample :: DGraph String Int
parseExample = fold $ catMaybes $ M.parseMaybe lineParser <$> example

-- | we're parsing the input data into a directed, weighted graph
-- The edges point towards the outside, e.g: red -- 3 --> black translates to "black bags contain 3 red bags"
inputParser :: Parser (DGraph String Int)
inputParser = fold <$> lineParser `sepBy1` M.newline

lineParser :: Parser (DGraph String Int)
lineParser = fromArcsList <$> (createArc <$> colorParser <* M.string " bags contain " <*> bagContentParser)

createArc :: v -> [(v, e)] -> [Arc v e]
createArc to froms = (\(from, count) -> Arc from to count) <$> froms

colorParser :: Parser String
colorParser = (\p1 p2 -> p1 <> " " <> p2) <$> M.some M.letterChar <* M.spaceChar <*> M.some M.letterChar

bagContentParser :: Parser [(String, Int)]
bagContentParser = (emptyBagParser <|> notEmptyBagParser) <* M.string "."
  where
    emptyBagParser = M.string "no other bags" >> pure []
    single = do
      count <- L.decimal
      _ <- M.spaceChar
      color <- colorParser
      _ <- M.spaceChar
      b <- M.string "bags" <|> M.string "bag"
      guard $ (count == 1 && b == "bag") || (count > 1 && b == "bags")
      pure (color, count)
    notEmptyBagParser = single `sepBy1` M.string ", "

evaluate1 :: String -> Maybe Int
evaluate1 input = countUniqueVerticesStartingAt "shiny gold" <$> M.parseMaybe inputParser input

-- | To count how many bags can contain a bag of a certain color, we start at the color and do a bread first traversal.
-- depth first traversal should work as well.
-- We then count how many unique colors we encounter, excluding the one we started with
countUniqueVerticesStartingAt :: (Ord a, Hashable a) => a -> DGraph a e -> Int
countUniqueVerticesStartingAt color graph = length . ordNub $ filter (color /=) $ bfsVertices graph color

-- | We have to invert the arrows in the graph because we want to go the other way round than in the first task
evaluate2 :: String -> Maybe Int
evaluate2 input = countBags "shiny gold" . G.transpose <$> M.parseMaybe inputParser input

-- | We subtract one from the result because the outermost bag shouldn't be included in the result
countBags :: (Hashable v, Eq v) => v -> DGraph v Int -> Int
countBags color graph = countBags' color graph - 1

-- | the number of bags is 1 + the number of bags in the outbound arcs, multiplied by the weight of the edge
countBags' :: (Hashable v, Eq v) => v -> DGraph v Int -> Int
countBags' color graph = 1 + sum (countDescendants <$> outboundingArcs graph color)
  where
    countDescendants (Arc _ v c) = c * countBags' v graph

run :: IO ()
run = readFile "inputs/day7-1.txt" >>= print . evaluate2
