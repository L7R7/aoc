module Day12_2 where

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

-- | Action N means to move the waypoint north by the given value.
--   Action S means to move the waypoint south by the given value.
--   Action E means to move the waypoint east by the given value.
--   Action W means to move the waypoint west by the given value.
--   Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
--   Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
--   Action F means to move forward to the waypoint a number of times equal to the given value.
data Command = N | S | E | W | L | R | F deriving (Eq)

data Instruction = I Command Int

type Pos = (Int, Int)

data ShipState = ShipState {shipPos :: Pos, waypointPos :: Pos}

update :: ShipState -> Instruction -> ShipState
update s@(ShipState _ (x, y)) (I N v) = s {waypointPos = (x, y + v)}
update s@(ShipState _ (x, y)) (I S v) = s {waypointPos = (x, y - v)}
update s@(ShipState _ (x, y)) (I E v) = s {waypointPos = (x + v, y)}
update s@(ShipState _ (x, y)) (I W v) = s {waypointPos = (x - v, y)}
update s (I L v) = rotateL s v
update s (I R v) = rotateR s v
update s@(ShipState (sX, sY) (wX, wY)) (I F v) = s {shipPos = (sX + v * wX, sY + v * wY)}

rotateL :: ShipState -> Int -> ShipState
rotateL s 0 = s
rotateL s@(ShipState _ (x, y)) deg = rotateL (s {waypointPos = (- y, x)}) (deg - 90)

rotateR :: ShipState -> Int -> ShipState
rotateR s 0 = s
rotateR s@(ShipState _ (x, y)) deg = rotateR (s {waypointPos = (y, - x)}) (deg - 90)

manhattanDistance :: ShipState -> Int
manhattanDistance (ShipState (x, y) _) = abs x + abs y

evaluate2 :: ShipState -> [Instruction] -> Int
evaluate2 initialState instructions = manhattanDistance $ foldl' update initialState instructions

run :: IO ()
run = readFile "inputs/day12-2.txt" >>= \s -> print (evaluate2 (ShipState (0, 0) (10, 1)) <$> parseInput s)
