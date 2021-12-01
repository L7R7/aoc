module Day14 where

import qualified Data.Map as M
import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show

type Parser = M.Parsec Void String

data BitMask = One | Zero | X

instance Show BitMask where
  show One = "1"
  show Zero = "0"
  show X = "X"

data Instruction = Instruction
  { bitMask :: [BitMask],
    assignments :: [(Int, Int)]
  }
  deriving (Show)

instructionParser :: Parser Instruction
instructionParser = do
  _ <- M.string "mask = "
  mask <- M.count 36 (M.choice [One <$ M.char '1', Zero <$ M.char '0', X <$ M.char 'X'])
  _ <- M.newline
  as <- assignmentParser `sepBy1` M.newline
  pure $ Instruction mask as

assignmentParser :: Parser (Int, Int)
assignmentParser = do
  _ <- M.string "mem["
  address <- L.decimal
  _ <- M.string "] = "
  val <- L.decimal
  pure (address, val)

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy1` M.newline

parseInput :: String -> Maybe [Instruction]
parseInput = M.parseMaybe inputParser

toBinary :: Int -> String
toBinary = skipWhile0 . go ""
  where
    go :: String -> Int -> String
    go acc 0 = '0' : acc
    go acc i = go ((if even i then '0' else '1') : acc) (i `div` 2)

fromBinary :: String -> Int
fromBinary = go 0
  where
    go :: Int -> String -> Int
    go acc "" = acc
    go acc ('1' : s) = go (2 * acc + 1) s
    go acc (_ : s) = go (2 * acc) s

skipWhile0 :: String -> String
skipWhile0 "" = ""
skipWhile0 ('0' : s) = skipWhile0 s
skipWhile0 s = s

leftPadWith :: Char -> Int -> String -> String
leftPadWith c len s
  | length s < len = leftPadWith c len (c : s)
  | otherwise = s

applyMask :: [BitMask] -> Int -> Int
applyMask mask n = fromBinary $ reverse $ zipWith apply inBin revMask
  where
    inBin = reverse (toBinary n) ++ repeat '0'
    revMask = reverse mask

apply :: Char -> BitMask -> Char
apply c X = c
apply _ One = '1'
apply _ Zero = '0'

process :: [Instruction] -> Map Int Int
process instructions = fold $ process' <$> instructions

process' :: Instruction -> Map Int Int
process' (Instruction mask as) = M.fromList $ (\(cell, val) -> (cell, applyMask mask val)) <$> as

evaluate1 :: [Instruction] -> Int
evaluate1 instructions = sum (process instructions)

run :: IO ()
run = readFile "inputs/day14-1.txt" >>= \s -> print (evaluate1 <$> parseInput s)