module Day12_1 where

import Relude
import Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy1` M.newline

instructionParser :: Parser Instruction
instructionParser = do
  command <-
    M.choice
      [ N <$ M.string "N",
        S <$ M.string "S",
        E <$ M.string "E",
        W <$ M.string "W",
        L <$ M.string "L",
        R <$ M.string "R",
        F <$ M.string "F"
      ]
  value <- L.decimal
  guard $ value > 0
  guard $ (command `elem` [L, R] && value `mod` 90 == 0) || command `elem` [N, S, E, W, F]
  pure $ I command value

parseInput :: String -> Maybe [Instruction]
parseInput = M.parseMaybe inputParser

-- | Action N means to move north by the given value.
--   Action S means to move south by the given value.
--   Action E means to move east by the given value.
--   Action W means to move west by the given value.
--   Action L means to turn left the given number of degrees.
--   Action R means to turn right the given number of degrees.
--   Action F means to move forward by the given value in the direction the ship is currently facing.
data Command = N | S | E | W | L | R | F deriving (Eq)

data Instruction = I Command Int

data Direction = North | East | South | West

data ShipState = ShipState (Int, Int) Direction

update :: ShipState -> Instruction -> ShipState
update (ShipState location direction) (I L degrees) = ShipState location (rotateL direction degrees)
update (ShipState location direction) (I R degrees) = ShipState location (rotateR direction degrees)
update s (I F steps) = moveForward s steps
update s (I N steps) = move s North steps
update s (I S steps) = move s South steps
update s (I E steps) = move s East steps
update s (I W steps) = move s West steps

rotateL :: Direction -> Int -> Direction
rotateL dir 0 = dir
rotateL North deg = rotateL West (deg - 90)
rotateL East deg = rotateL North (deg - 90)
rotateL South deg = rotateL East (deg - 90)
rotateL West deg = rotateL South (deg - 90)

rotateR :: Direction -> Int -> Direction
rotateR dir 0 = dir
rotateR North deg = rotateR East (deg - 90)
rotateR East deg = rotateR South (deg - 90)
rotateR South deg = rotateR West (deg - 90)
rotateR West deg = rotateR North (deg - 90)

moveForward :: ShipState -> Int -> ShipState
moveForward s@(ShipState _ dir) = move s dir

move :: ShipState -> Direction -> Int -> ShipState
move (ShipState (x, y) d) North steps = ShipState (x, y + steps) d
move (ShipState (x, y) d) South steps = ShipState (x, y - steps) d
move (ShipState (x, y) d) East steps = ShipState (x + steps, y) d
move (ShipState (x, y) d) West steps = ShipState (x - steps, y) d

manhattanDistance :: ShipState -> Int
manhattanDistance (ShipState (x, y) _) = abs x + abs y

evaluate1 :: ShipState -> [Instruction] -> Int
evaluate1 initialState instructions = manhattanDistance $ foldl' update initialState instructions

run :: IO ()
run = readFile "inputs/day12-2.txt" >>= \s -> print (evaluate1 (ShipState (0, 0) East) <$> parseInput s)
