module Day08 (run) where

import qualified Data.Map as M
import qualified Data.Set as S
import Relude hiding (init)
import Relude.Extra (universe)
import qualified Relude.Unsafe as Unsafe
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
parseInput = Input <$> count 10 signalParser <* string "| " <*> count 4 signalParser
  where
    signalParser = Text.Megaparsec.some (choice [char 'a', char 'b', char 'c', char 'd', char 'e', char 'f', char 'g']) <* optional (char ' ')

data Input = Input
  { signalPatterns :: [String],
    outputValue :: [String]
  }
  deriving (Eq, Show)

evaluate1 :: [Input] -> Int
evaluate1 inputs = sum $ length . filter (\s -> let l = length s in l == 2 || l == 4 || l == 3 || l == 7) . outputValue <$> inputs

evaluate2 :: [Input] -> Int
evaluate2 inputs = sum $ solve <$> inputs

--  aaaa
-- b    c
-- b    c
--  ....
-- e    f
-- e    f
--  gggg
data Digit = A | B | C | D | E | F | G deriving (Bounded, Enum, Eq, Ord, Show)

allChars :: [Char]
allChars = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

allPerms :: [Map Char Digit]
allPerms = M.fromList . flip zip universe <$> permutations allChars

sumUp :: [Int] -> Int
sumUp = go 0
  where
    go acc [] = acc
    go acc (i : is) = go (acc * 10 + i) is

numbers :: Map (S.Set Digit) Int
numbers =
  M.fromList
    [ (S.fromList [A, B, C, E, F, G], 0),
      (S.fromList [C, F], 1),
      (S.fromList [A, C, D, E, G], 2),
      (S.fromList [A, C, D, F, G], 3),
      (S.fromList [B, C, D, F], 4),
      (S.fromList [A, B, D, F, G], 5),
      (S.fromList [A, B, D, E, F, G], 6),
      (S.fromList [A, C, F], 7),
      (S.fromList [A, B, C, D, E, F, G], 8),
      (S.fromList [A, B, C, D, F, G], 9)
    ]

solve :: Input -> Int
solve (Input signals outputs) = sumUp $ translate translationMap <$> outputs
  where
    translationMap :: Map Char Digit
    translationMap = Unsafe.head $ filter possible allPerms
    possible :: Map Char Digit -> Bool
    possible mcd = all isJust $ resolve mcd <$> signals
    translate :: Map Char Digit -> String -> Int
    translate mcd s = numbers M.! S.fromList ((mcd M.!) <$> s)
    resolve :: Map Char Digit -> String -> Maybe Int
    resolve mcd s = M.lookup digits numbers
      where
        digits = S.fromList $ (mcd M.!) <$> s

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

      putStrLn "=== Part 2"
      let solution4 = evaluate2 exampleParsed
      putStrLn $ "Solution for example is: " <> show solution4
      putStrLn $ "Solution is correct for example input: " <> show (solution4 == 61229)
      let solution5 = evaluate2 input
      putStrLn $ "Solution for input is: " <> show solution5
      putStrLn $ "Solution is correct for input: " <> show (solution5 == 1011785)
