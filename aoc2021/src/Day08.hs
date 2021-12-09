module Day08 where

import Relude hiding (init)
import Text.Megaparsec
import Text.Megaparsec.Char

example :: [Text]
example =
  [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

exampleParsed :: [Input]
exampleParsed =
  [ Input ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"] ["fdgacbe", "cefdb", "cefbgd", "gcbe"],
    Input ["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"] ["fcgedb", "cgb", "dgebacf", "gc"],
    Input ["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"] ["cg", "cg", "fdcagb", "cbg"],
    Input ["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"] ["efabcd", "cedba", "gadfec", "cb"],
    Input ["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"] ["gecf", "egdcabf", "bgf", "bfgea"],
    Input ["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"] ["gebdcfa", "ecba", "ca", "fadegcb"],
    Input ["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"] ["cefg", "dcbef", "fcge", "gbcadfe"],
    Input ["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"] ["ed", "bcgafe", "cdgba", "cbgef"],
    Input ["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"] ["gbdfcae", "bgc", "cg", "cgb"],
    Input ["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"] ["fgae", "cfgab", "fg", "bagce"]
  ]

parseInput :: Parsec Void Text Input
parseInput = do
  signal <- count 10 signalParser
  _ <- string "| "
  output <- count 4 signalParser
  pure $ Input signal output

signalParser :: ParsecT Void Text Identity String
signalParser = do
  x <- Text.Megaparsec.some $ choice [char 'a', char 'b', char 'c', char 'd', char 'e', char 'f', char 'g']
  _ <- optional $ string " "
  pure x

signalPatternsParser :: ParsecT Void Text Identity [String]
signalPatternsParser = Text.Megaparsec.some signalParser 

data Input = Input
  { signalPatterns :: [String],
    outputValue :: [String]
  }
  deriving (Eq, Show)

evaluate1 :: [Input] -> Int
evaluate1 inputs = sum $ f <$> inputs
  where
    f :: Input -> Int
    f (Input _ strs) = length $filter (\s -> length s == 2 || length s == 4 || length s == 3 || length s == 7) strs

evaluate2 :: [Input] -> Int
evaluate2 inputs = 0

run :: IO ()
run = do
  putStrLn $ "Parsing the example works: " <> show (traverse (parseMaybe parseInput) example == Just exampleParsed)
  inputRaw <- lines <$> readFileText "inputs/day08.txt"

  case traverse (parseMaybe parseInput) inputRaw of
    Nothing -> putStrLn "Parsing the inputs failed"
    Just input -> do
      putStrLn "=== Part 1"
      let solution1 = evaluate1 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution1
      putStrLn $ "Solution is correct for example input: " <> show (solution1 == 26)
      let solution2 = evaluate1 input
      putStrLn $ "Solution for input is: " <> show solution2
      putStrLn $ "Solution is correct for input: " <> show (solution2 == 390)

-- putStrLn "=== Part 2"
-- let solution4 = evaluate2 exampleParsed
-- putStrLn $ "Solution for example is: " <> show solution4
-- putStrLn $ "Solution is correct for example input: " <> show (solution4 == 12)
-- let solution5 = evaluate2 input
-- putStrLn $ "Solution for input is: " <> show solution5
-- putStrLn $ "Solution is correct for input: " <> show (solution5 == 16716)
