module Day10 where

import Relude hiding (init)
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

preprocess :: [String] -> [(String, Maybe Char)]
preprocess = fmap (foldl' f ([], Nothing))
  where
    f :: (String, Maybe Char) -> Char -> (String, Maybe Char)
    f x@(stack, err) c
      | isJust err = x
      | isOpening c = (c : stack, Nothing)
      | otherwise = if all (`matches` c) c' then (stack', Nothing) else (stack, Just c)
      where
        (c', stack') = pop stack
        pop (a : as) = (Just a, as)
        pop _ = (Nothing, [])

evaluate1 :: [String] -> Int
evaluate1 = sum . fmap points . mapMaybe snd . preprocess

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

matches :: Char -> Char -> Bool
matches c c' = findClosing c == c'

isOpening :: Char -> Bool
isOpening '{' = True
isOpening '(' = True
isOpening '[' = True
isOpening '<' = True
isOpening _ = False

evaluate2 :: [String] -> Int
evaluate2 = findMiddle . fmap (points2 . fmap findClosing . fst) . filter (isNothing . snd) . preprocess

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
findMiddle is = sort is Unsafe.!! (length is - 1) `div` 2

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
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 2289754624)
