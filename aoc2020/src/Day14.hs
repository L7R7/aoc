{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day14 (run) where

import qualified Data.Map as M
import Relude hiding (State)
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

data Bit = One | Zero | X

instance Semigroup Bit where
    X <> b = b
    b <> X = b
    One <> _ = One
    Zero <> b = b

instance Monoid Bit where
    mempty = X

newtype MemoryAddress = MemoryAddress Int deriving newtype (Eq, Ord)

data Instruction
  = Mask [Bit]
  | Instruction MemoryAddress Int

data State = State {bitMask :: [Bit], mem :: Map MemoryAddress Int}

maskParser :: Parser Instruction
maskParser = M.string "mask = " *> (Mask <$> M.count 36 (M.choice [One <$ M.char '1', Zero <$ M.char '0', X <$ M.char 'X']))

assignmentParser :: Parser Instruction
assignmentParser = do
  _ <- M.string "mem["
  address <- L.decimal
  _ <- M.string "] = "
  val <- L.decimal
  pure $ Instruction (MemoryAddress address) val

instructionParser :: Parser Instruction
instructionParser = maskParser <|> assignmentParser

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

evaluate1 :: [Instruction] -> Int -- 14925946402938
evaluate1 instructions = sumState $ foldl' update (State (replicate 36 X) mempty) instructions

update :: State -> Instruction -> State
update s (Mask m) = s {bitMask = m}
update s@(State mask ms) (Instruction addr val) = s {mem = M.insert addr (applyMask mask val) ms}

applyMask :: [Bit] -> Int -> Int
applyMask mask n = fromBinary $ reverse $ zipWith apply inBin revMask
  where
    inBin = reverse (toBinary n) ++ repeat '0'
    revMask = reverse mask

apply :: Char -> Bit -> Char
apply c X = c
apply _ One = '1'
apply _ Zero = '0'

sumState :: State -> Int
sumState (State _ m) = sum m

run :: IO ()
run = readFile "inputs/day14-2.txt" >>= \s -> print (evaluate1 <$> parseInput s)
