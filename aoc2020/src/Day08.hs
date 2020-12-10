{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Data.Vector hiding (elem, find, head)
import qualified Data.Vector as V
import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show)

inputParser :: Parser Instructions
inputParser = V.fromList <$> lineParser `sepBy1` M.newline

-- | Actually, I would like to split this thing into two parsers:
-- argumentParser :: Parser Int
-- operationParser :: Int -> Parser Instruction
--
-- But the operation parser needs the result of the argument parser to work.
-- I combined things into one for now, until I find a way to combine the two individual parsers
lineParser :: Parser Instruction
lineParser = do
  operation <- M.string "acc" <|> M.string "jmp" <|> M.string "nop"
  _ <- M.spaceChar
  sign <- M.string "+" <|> M.string "-"
  i <- L.decimal
  let argument = (if sign == "-" then -1 else 1) * i
  pure
    if
        | operation == "acc" -> Acc argument
        | operation == "jmp" -> Jmp argument
        | otherwise -> Nop argument

type Instructions = Vector Instruction

type Accumulator = Int

type Index = Int

type KnownIndexes = [Index]

findInfiniteLoop :: Instructions -> Maybe Int
findInfiniteLoop instructions = findInfiniteLoop' instructions 0 0 []

-- | basic idea:
-- Parse the input into a data structure that supports fast access to a given index.
-- (It probably doesn't make too much of a difference overall given the inputs, but still. Let's pretend).
-- This will be our instruction set that won't be modified during evaluation
--
-- We walk through the instructions using an index starting at 0
-- We evaluate the instruction at the given index and return the new index and the accumulator (which might have been changed by the instruction)
-- Also, we build up a list of indexes that we already visited.
-- If the new index after evaluating is already in the list, we have our infinite loop
--
-- Technically, this could lead to an infinite recursion if there's no infinite loop in the instructions.
-- Let's ignore that for now
findInfiniteLoop' :: Instructions -> Accumulator -> Index -> KnownIndexes -> Maybe Int
findInfiniteLoop' instructions acc i is = do
  instruction <- instructions !? i
  let newAcc = applyInstruction instruction acc
  let newIndex = applyIndex instruction i
  if newIndex `elem` is then pure acc else findInfiniteLoop' instructions newAcc newIndex (i : is)

applyInstruction :: Instruction -> Accumulator -> Accumulator
applyInstruction (Acc a) = (+) a
applyInstruction _ = id

applyIndex :: Instruction -> Index -> Index
applyIndex (Jmp i) = (+) i
applyIndex _ = (+) 1

evaluate1 :: String -> Maybe Int
evaluate1 input = M.parseMaybe inputParser input >>= findInfiniteLoop

-- | tries evaluating a given set of instructions, flipping one instruction at a time.
-- It starts by flipping no instruction, then the first one, then the second one, ...
-- It will return the first result that terminates.
tryEvaluateInstructions :: Instructions -> Maybe Int
tryEvaluateInstructions instructions = join . find isJust $ evaluateInstructionsFlipping instructions <$> [-1 .. (V.length instructions)]

type IndexToFlip = Index

evaluateInstructionsFlipping :: Instructions -> IndexToFlip -> Maybe Int
evaluateInstructionsFlipping instructions indexToFlip = evaluateInstructionsFlipping' instructions 0 0 indexToFlip []

-- | Evaluates the instructions, flipping the instruction at the given index
-- returns Nothing if instruction set doesn't terminate
-- returns Just x, if the instruction set terminates. x will be the value of the accumulator after the last instruction
evaluateInstructionsFlipping' :: Instructions -> Accumulator -> Index -> IndexToFlip -> KnownIndexes -> Maybe Int
evaluateInstructionsFlipping' instructions acc i _ _ | i >= V.length instructions = pure acc
evaluateInstructionsFlipping' instructions acc i indexToFlip is = do
  instruction <- instructions !? i
  let actualInstruction = if i == indexToFlip then flipInstruction instruction else instruction
  let newAcc = applyInstruction actualInstruction acc
  let newIndex = applyIndex actualInstruction i
  if newIndex `elem` is then Nothing else evaluateInstructionsFlipping' instructions newAcc newIndex indexToFlip (i : is)

flipInstruction :: Instruction -> Instruction
flipInstruction (Nop i) = Jmp i
flipInstruction (Jmp i) = Nop i
flipInstruction x = x

evaluate2 :: String -> Maybe Int
evaluate2 input = M.parseMaybe inputParser input >>= tryEvaluateInstructions

run :: IO ()
run = readFile "inputs/day8-2.txt" >>= print . evaluate2
