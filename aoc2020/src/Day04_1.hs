module Day04_1 where

import Data.List.Split (splitOn)
import Relude
import Text.Megaparsec
import Text.Megaparsec.Char

example :: IO [String]
example = splitOn "\n\n" <$> readFile "inputs/day4-1.txt"

type Parser = Parsec Void String

evaluate1 :: [String] -> Maybe Int
evaluate1 input = length . filter isValid <$> sequence (parseMaybe passportEntry <$> input)

isValid :: [(String, String)] -> Bool
isValid xs = isJust $ parseMaybe passportEntryContent $ intercalate " " $ (\(k, v) -> k ++ ":" ++ v) <$> sortNub xs

passportEntryContent :: Parser ()
passportEntryContent = do
  _ <- valueWithKey "byr" >> spaceChar
  _ <- count' 0 1 $ valueWithKey "cid" >> spaceChar >> pass
  _ <- valueWithKey "ecl" >> spaceChar
  _ <- valueWithKey "eyr" >> spaceChar
  _ <- valueWithKey "hcl" >> spaceChar
  _ <- valueWithKey "hgt" >> spaceChar
  _ <- valueWithKey "iyr" >> spaceChar
  _ <- valueWithKey "pid"
  pass

valueWithKey :: String -> Parser (String, String)
valueWithKey k = string k >> string ":" >> value >>= (\v -> pure (k, v))

passportEntry :: Parser [(String, String)]
passportEntry = keyValue `sepBy1` (newline <|> spaceChar)

keyValue :: Parser (String, String)
keyValue = do
  k <- key
  _ <- string ":"
  v <- value
  pure (k, v)

key :: Parser String
key =
  string "byr"
    <|> string "cid"
    <|> string "ecl"
    <|> string "eyr"
    <|> string "hcl"
    <|> string "hgt"
    <|> string "iyr"
    <|> string "pid"

value :: Parser String
value = do
  hash <- count' 0 1 (char '#')
  v <- Text.Megaparsec.some alphaNumChar
  pure $ hash ++ v

run :: IO ()
run = do
  inputRaw <- splitOn "\n\n" <$> readFile "inputs/day4-2.txt"
  case evaluate1 inputRaw of
    Nothing -> print ("No result found" :: String)
    Just i -> print i
