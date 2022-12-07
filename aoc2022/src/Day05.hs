module Day05 (run) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Relude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parsec Void Text Input
parseInput = Input <$> stacksParser <* (newline <* newline) <*> instructionsParser

stacksParser :: Parsec Void Text Stacks
stacksParser = do
  res <- Stacks . buildStacks <$> stackLineParser `sepBy` newline
  _ <- (char ' ' >> digitChar >> char ' ') `sepBy` char ' '
  pure res
  where
    buildStacks = foldl' f V.empty
    f :: Vector (Vector Char) -> [Maybe Char] -> Vector (Vector Char)
    f acc mcs
      | null acc = f (V.replicate (length mcs) V.empty) mcs
      | null mcs = acc
      | otherwise = V.zipWith (\stack m_c -> maybe stack (V.snoc stack) m_c) acc (V.fromList mcs)

    stackLineParser :: Parsec Void Text [Maybe Char]
    stackLineParser = (foo <|> box) `sepBy` char ' '
      where
        foo = Nothing <$ string "   "
        box = Just <$> (char '[' *> letterChar <* char ']')

instructionsParser :: Parsec Void Text [Instruction]
instructionsParser = instruction `sepBy` newline
  where
    instruction = (\n f t -> Instruction n (f - 1) (t - 1)) <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

data Input = Input Stacks [Instruction] deriving stock (Show)

newtype Stacks = Stacks (Vector (Vector Char)) deriving stock (Show)

data Instruction = Instruction {numberOfCrates :: Int, from :: Int, to :: Int} deriving stock (Show)

evaluate1 :: Input -> String
evaluate1 (Input initial instructions) = V.toList $ extractHeads $ foldl' step initial instructions
  where
    step :: Stacks -> Instruction -> Stacks
    step (Stacks stacks) (Instruction 1 f t) = Stacks $ V.update stacks (V.fromList [(f, V.tail vectorToRemoveFrom), (t, V.cons elementToMove vectorToAddTo)])
      where
        vectorToRemoveFrom = stacks V.! f
        elementToMove = V.head vectorToRemoveFrom
        vectorToAddTo = stacks V.! t
    step stacks (Instruction i f t) = step (step stacks (Instruction 1 f t)) (Instruction (i - 1) f t)
    extractHeads :: Stacks -> Vector Char
    extractHeads (Stacks vec) = V.head <$> vec

evaluate2 :: Input -> String
evaluate2 (Input initial instructions) = V.toList $ extractHeads $ foldl' step initial instructions
  where
    step :: Stacks -> Instruction -> Stacks
    step (Stacks stacks) (Instruction i f t) = Stacks $ V.update stacks (V.fromList [(f, V.drop i vectorToRemoveFrom), (t, elementsToMove V.++ vectorToAddTo)])
      where
        vectorToRemoveFrom = stacks V.! f
        elementsToMove = V.take i vectorToRemoveFrom
        vectorToAddTo = stacks V.! t
    extractHeads :: Stacks -> Vector Char
    extractHeads (Stacks vec) = V.head <$> vec

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/05-example.txt"
  let exampleParsed = parseMaybe parseInput exampleInputRaw
  print $ evaluate1 <$> exampleParsed

  inputRaw <- readFileText "inputs/05.txt"
  let parsed = parseMaybe parseInput inputRaw
  print $ evaluate1 <$> parsed

  print $ evaluate2 <$> exampleParsed

  print $ evaluate2 <$> parsed
