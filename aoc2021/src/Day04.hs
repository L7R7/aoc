{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Day04 where

import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.List (partition)
import Relude hiding (init)
import Relude.Extra (fmapToSnd)
import Text.Megaparsec
import qualified Text.Megaparsec as T
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

exampleParsed :: Input
exampleParsed =
  Input
    (7 :| [4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1])
    [ [ [22, 13, 17, 11, 0],
        [8, 2, 23, 4, 24],
        [21, 9, 14, 16, 7],
        [6, 10, 3, 18, 5],
        [1, 12, 20, 15, 19]
      ],
      [ [3, 15, 0, 2, 22],
        [9, 18, 13, 17, 5],
        [19, 8, 7, 25, 23],
        [20, 11, 10, 24, 4],
        [14, 21, 16, 12, 6]
      ],
      [ [14, 21, 17, 24, 4],
        [10, 16, 15, 9, 19],
        [18, 8, 23, 26, 20],
        [22, 11, 13, 6, 5],
        [2, 0, 12, 3, 7]
      ]
    ]

parseInput :: Parsec Void Text Input
parseInput = Input <$> numbersParser <* skipCount 2 newline <*> boardsParser
  where
    numbersParser = decimal `NE.sepBy1` char ','
    boardLineParser = someTill (skipMany (char ' ') >> decimal) newline
    boardParser = T.some boardLineParser
    boardsParser = boardParser `sepBy` newline

type Board = [[Int]]

data Input = Input
  { numbers :: NonEmpty Int,
    boards :: [Board]
  }
  deriving (Eq, Show)

evaluate1 :: Input -> Maybe Int
evaluate1 (Input (n :| nums) bs) = calculateScore <$> (extractWinningCombination =<< findWinningStep)
  where
    findWinningStep :: Maybe Step
    findWinningStep = find wins (scanl' (<>) (init bs n) (init bs <$> nums))
    extractWinningCombination :: Step -> Maybe (Int, BoardState)
    extractWinningCombination (Step (Last num) states) = (,) <$> num <*> find wins' states

evaluate1' :: Input -> Maybe Int
evaluate1' = evaluateX listToMaybe

calculateScore :: (Int, BoardState) -> Int
calculateScore (i, BoardState states) = i * sum (fst <$> filter ((== False) . snd) (join states))

newtype BoardState = BoardState [[(Int, Bool)]] deriving newtype (Eq, Show)

instance Semigroup BoardState where
  (BoardState bs) <> (BoardState bs') = BoardState $ zipWith (zipWith (\(_, b) (i', b') -> (i', b || b'))) bs bs'

data Step = Step
  { number :: Last Int,
    boardsState :: [BoardState]
  }
  deriving (Show)

instance Semigroup Step where
  (Step n bs) <> (Step n' bs') = Step (n <> n') (zipWith (<>) bs bs')

wins :: Step -> Bool
wins (Step _ bs) = any wins' bs

wins' :: BoardState -> Bool
wins' (BoardState bs) = lineWins bs || lineWins (transpose bs)
  where
    lineWins xs = or (fmap (all ((== True) . snd)) xs)

init :: [Board] -> Int -> Step
init bs i = Step (Last (Just i)) (BoardState . fmap (fmapToSnd (== i)) <$> bs)

initBoard :: Functor f => f [[Int]] -> f BoardState
initBoard bs = BoardState . fmap (fmapToSnd (const False)) <$> bs

evaluate2 :: Input -> Maybe Int
evaluate2 = evaluateX $ getLast . foldMap (Last . Just)

evaluateX :: ([(Int, [BoardState])] -> Maybe (Int, [BoardState])) -> Input -> Maybe Int
evaluateX choose (Input nums bs) = calculateScore <$> findWinningStep
  where
    findWinningStep :: Maybe (Int, BoardState)
    findWinningStep = choose (filter hasWins findSolutions) >>= findFirst
    findSolutions = fmap fst <$> scanl' f (-1, ([], initBoard bs)) (toList nums)
    hasWins = not . null . snd
    findFirst = \(i, ls) -> (i,) <$> listToMaybe ls
    f :: (Int, ([BoardState], [BoardState])) -> Int -> (Int, ([BoardState], [BoardState]))
    f (_, (_, notWon)) i = (i, partition wins' (updateBoardState i <$> notWon))

updateBoardState :: Int -> BoardState -> BoardState
updateBoardState i (BoardState st) = BoardState $ fmap (\(j, b) -> (j, b || j == i)) <$> st

run :: IO ()
run = do
  example <- readFileText "inputs/day04-example.txt"
  putStrLn $ "Parsing the example works: " <> show (parseMaybe parseInput example == Just exampleParsed)
  inputRaw <- readFileText "inputs/day04.txt"

  case parseMaybe parseInput inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == Just 4512)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == Just 44736)
      let solution3 = evaluate1' input
      putStrLn $ "Solution with second attempt for input is: " <> show solution3
      putStrLn $ "Solution with second attempt is correct for input: " <> show (solution3 == Just 44736)

      putStrLn "=== Part 2"
      let solution4 = evaluate2 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4

      putStrLn $ "Solution is correct for example input: " <> show (solution4 == Just 1924)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == Just 1827)
