{-# LANGUAGE TupleSections #-}

module Day10 where

import qualified Data.Map as M
import qualified Data.Set as S
import Relude hiding (init)
import Relude.Extra (universe)
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char

example :: Text
example =
  unlines
    [ "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

exampleParsed :: [String]
exampleParsed =
  [ "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

parseInput :: Parsec Void Text [String]
parseInput = Text.Megaparsec.some (choice [char '{', char '}', char '(', char ')', char '[', char ']', char '<', char '>']) `sepEndBy1` newline

evaluate1 :: [String] -> Int
evaluate1 = sum . mapMaybe (fmap points . snd . foldl' f ([], Nothing))
  where
    f :: ([Char], Maybe Char) -> Char -> ([Char], Maybe Char)
    f x@(stack, err) c
      | isJust err = x
      | isOpening c = (push stack c, Nothing)
      | otherwise = if all (`matches` c) c' then (stack', Nothing) else (stack, Just c)
      where
        (c', stack') = pop stack

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

type Stack a = [a]

push :: Stack a -> a -> Stack a
push as a = a : as

pop :: Stack a -> (Maybe a, Stack a)
pop (a : as) = (Just a, as)
pop _ = (Nothing, [])

matches :: Char -> Char -> Bool
matches '{' '}' = True
matches '(' ')' = True
matches '[' ']' = True
matches '<' '>' = True
matches _ _ = False

isOpening :: Char -> Bool
isOpening '{' = True
isOpening '(' = True
isOpening '[' = True
isOpening '<' = True
isOpening _ = False

evaluate2 :: [String] -> Int
evaluate2 = findMiddle . fmap points2 . fmap (fmap findClosing) . fmap fst . filter (isNothing . snd) . fmap (foldl' f ([], Nothing))
  where
    f :: ([Char], Maybe Char) -> Char -> ([Char], Maybe Char)
    f x@(stack, err) c
      | isJust err = x
      | isOpening c = (push stack c, Nothing)
      | otherwise = if all (`matches` c) c' then (stack', Nothing) else (stack, Just c)
      where
        (c', stack') = pop stack

findClosing :: Char -> Char
findClosing '{' = '}'
findClosing '(' = ')'
findClosing '[' = ']'
findClosing '<' = '>'
findClosing c = c

points2 :: [Char] -> Int
points2 = foldl' (\acc i -> acc * 5 + i) 0 . fmap p
  where
    p :: Char -> Int
    p ')' = 1
    p ']' = 2
    p '}' = 3
    p '>' = 4
    p _ = 0

findMiddle :: [Int] -> Int
findMiddle is = sort is Unsafe.!! x
  where
    x = (length is - 1) `div` 2

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (parseMaybe parseInput example == Just exampleParsed)
  inputRaw <- readFileText "inputs/day10.txt"

  case parseMaybe parseInput inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 26397)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 339411)

      putStrLn "=== Part 2"
      let solution4 = evaluate2 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4
      putStrLn $ "Solution is correct for example input: " <> show (solution4 == 288957)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 1011785)