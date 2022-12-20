module Day08 (run) where

import Data.Char (digitToInt)
import Data.List (zipWith4)
import Relude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char

inputParser :: Parsec Void Text Input1
inputParser = many (digitToInt <$> digitChar) `sepBy` newline

type Input1 = [[Int]]

evaluate1 :: Input1 -> Sum Int
evaluate1 leftToRight = foldMap (\b -> if b then Sum 1 else mempty) resGrid
  where
    rightToLeft = reverse <$> leftToRight
    topToBottom = transpose leftToRight
    bottomToTop = reverse <$> topToBottom
    resGrid =
      Compose $
        zipWith4
          (zipWith4 (\b b' b3 b4 -> b || b' || b3 || b4))
          (f leftToRight)
          (reverse <$> f rightToLeft)
          (transpose $ f topToBottom)
          (transpose $ reverse <$> f bottomToTop)
    f :: [[Int]] -> [[Bool]]
    f grid = snd . mapAccumL g Nothing <$> grid
    g :: Maybe Int -> Int -> (Maybe Int, Bool)
    g Nothing n = (Just n, True)
    g (Just prev) n = (Just (max prev n), prev < n)

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/08-example.txt"
  let exampleParsed1 = parseMaybe inputParser exampleInputRaw
  print $ traverse evaluate1 exampleParsed1

  inputRaw <- readFileText "inputs/08.txt"
  let parsed1 = parseMaybe inputParser inputRaw
  print $ traverse evaluate1 parsed1
