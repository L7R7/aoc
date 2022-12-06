module Day02 (run) where

import Relude
import Text.Megaparsec
import Text.Megaparsec.Char

parseInput1 :: Parsec Void Text [Input1]
parseInput1 = parseInput1Line `sepBy` newline

parseInput1Line :: Parsec Void Text Input1
parseInput1Line = Input1 <$> opponentChoiceParser <* space <*> playerChoiceParser

parseInput2 :: Parsec Void Text [Input2]
parseInput2 = parseInput2Line `sepBy` newline

parseInput2Line :: Parsec Void Text Input2
parseInput2Line = Input2 <$> opponentChoiceParser <* space <*> resultParser

resultParser :: Parsec Void Text Result
resultParser = Lose <$ char 'X' <|> Draw <$ char 'Y' <|> Win <$ char 'Z'

playerChoiceParser :: Parsec Void Text PlayerChoice
playerChoiceParser = PlayerChoice <$> (Rock <$ char 'X' <|> Paper <$ char 'Y' <|> Scissors <$ char 'Z')

opponentChoiceParser :: Parsec Void Text OpponentChoice
opponentChoiceParser = OpponentChoice <$> (Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C')

data Input1 = Input1 OpponentChoice PlayerChoice deriving stock (Show)

data Input2 = Input2 OpponentChoice Result deriving stock (Show)

newtype PlayerChoice = PlayerChoice RPS deriving newtype (Show)

newtype OpponentChoice = OpponentChoice RPS deriving newtype (Show)

data RPS = Rock | Paper | Scissors deriving stock (Show)

data Result = Win | Draw | Lose deriving stock (Show)

type Score = Sum Int

score :: PlayerChoice -> Result -> Score
score pc res = choiceScore pc + resultScore res
  where
    choiceScore (PlayerChoice Rock) = 1
    choiceScore (PlayerChoice Paper) = 2
    choiceScore (PlayerChoice Scissors) = 3

    resultScore Win = 6
    resultScore Draw = 3
    resultScore Lose = 0

evaluate1 :: [Input1] -> Sum Int
evaluate1 = foldMap $ \i@(Input1 _ pc) -> score pc (evaluate i)
  where
    evaluate :: Input1 -> Result
    evaluate (Input1 (OpponentChoice Rock) (PlayerChoice Rock)) = Draw
    evaluate (Input1 (OpponentChoice Rock) (PlayerChoice Paper)) = Win
    evaluate (Input1 (OpponentChoice Rock) (PlayerChoice Scissors)) = Lose
    evaluate (Input1 (OpponentChoice Paper) (PlayerChoice Rock)) = Lose
    evaluate (Input1 (OpponentChoice Paper) (PlayerChoice Paper)) = Draw
    evaluate (Input1 (OpponentChoice Paper) (PlayerChoice Scissors)) = Win
    evaluate (Input1 (OpponentChoice Scissors) (PlayerChoice Rock)) = Win
    evaluate (Input1 (OpponentChoice Scissors) (PlayerChoice Paper)) = Lose
    evaluate (Input1 (OpponentChoice Scissors) (PlayerChoice Scissors)) = Draw

evaluate2 :: [Input2] -> Sum Int
evaluate2 = foldMap $ \i@(Input2 _ res) -> score (evaluate i) res
  where
    evaluate :: Input2 -> PlayerChoice
    evaluate (Input2 (OpponentChoice Rock) Win) = PlayerChoice Paper
    evaluate (Input2 (OpponentChoice Rock) Draw) = PlayerChoice Rock
    evaluate (Input2 (OpponentChoice Rock) Lose) = PlayerChoice Scissors
    evaluate (Input2 (OpponentChoice Paper) Win) = PlayerChoice Scissors
    evaluate (Input2 (OpponentChoice Paper) Draw) = PlayerChoice Paper
    evaluate (Input2 (OpponentChoice Paper) Lose) = PlayerChoice Rock
    evaluate (Input2 (OpponentChoice Scissors) Win) = PlayerChoice Rock
    evaluate (Input2 (OpponentChoice Scissors) Draw) = PlayerChoice Scissors
    evaluate (Input2 (OpponentChoice Scissors) Lose) = PlayerChoice Paper

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/02-example.txt"
  let exampleParsed1 = parseMaybe parseInput1 exampleInputRaw
  print $ traverse evaluate1 exampleParsed1

  inputRaw <- readFileText "inputs/02.txt"
  let parsed1 = parseMaybe parseInput1 inputRaw
  print $ traverse evaluate1 parsed1

  let exampleParsed2 = parseMaybe parseInput2 exampleInputRaw
  print $ traverse evaluate2 exampleParsed2

  let parsed2 = parseMaybe parseInput2 inputRaw
  print $ traverse evaluate2 parsed2
