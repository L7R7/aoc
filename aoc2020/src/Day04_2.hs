{-# LANGUAGE ScopedTypeVariables #-}

module Day04_2 where

import Data.List.Split (splitOn)
import Relude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

exampleInvalid :: IO [String]
exampleInvalid = splitOn "\n\n" <$> readFile "inputs/day4-3.txt"

-- | contains 4 valid passport entries
exampleValid :: IO [String]
exampleValid = splitOn "\n\n" <$> readFile "inputs/day4-4.txt"

evaluate2 :: [String] -> Maybe Int
evaluate2 input = length . filter isValid <$> sequence (parseMaybe passportEntry <$> input)

isValid :: [(String, String)] -> Bool
isValid xs = isJust $ parseMaybe passportEntryContent $ intercalate " " $ (\(k, v) -> k ++ ":" ++ v) <$> sortNub xs

passportEntryContent :: Parser ()
passportEntryContent = do
  _ <- byr >> spaceChar
  _ <- count' 0 1 $ cid >> spaceChar
  _ <- ecl >> spaceChar
  _ <- eyr >> spaceChar
  _ <- hcl >> spaceChar
  _ <- hgt >> spaceChar
  _ <- iyr >> spaceChar
  pid

passportEntry :: Parser [(String, String)]
passportEntry = keyValue `sepBy1` (newline <|> spaceChar)

keyValue :: Parser (String, String)
keyValue = do
  k <- key
  _ <- string ":"
  v <- value
  pure (k, v)

value :: Parser String
value = do
  hash <- count' 0 1 (char '#')
  v <- Text.Megaparsec.some alphaNumChar
  pure $ hash ++ v

key :: Parser String
key = string "byr" <|> string "cid" <|> string "ecl" <|> string "eyr" <|> string "hcl" <|> string "hgt" <|> string "iyr" <|> string "pid"

-- | byr (Birth Year) - four digits; at least 1920 and at most 2002.
byr :: Parser ()
byr = do
  _ <- string "byr:"
  (number :: Int) <- decimal
  guard (number >= 1920 && number <= 2002)

-- | iyr (Issue Year) - four digits; at least 2010 and at most 2020.
iyr :: Parser ()
iyr = do
  _ <- string "iyr:"
  (number :: Int) <- decimal
  guard (number >= 2010 && number <= 2020)

-- | eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
eyr :: Parser ()
eyr = do
  _ <- string "eyr:"
  (number :: Int) <- decimal
  guard (number >= 2020 && number <= 2030)

-- | hgt (Height) - a number followed by either cm or in:
-- | If cm, the number must be at least 150 and at most 193.
-- | If in, the number must be at least 59 and at most 76.
hgt :: Parser ()
hgt = do
  _ <- string "hgt:"
  (number :: Int) <- decimal
  unit <- string "cm" <|> string "in"
  guard $
    (unit == "cm" && number >= 150 && number <= 193)
      || (unit == "in" && number >= 59 && number <= 76)

-- | hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
hcl :: Parser ()
hcl = string "hcl:#" >> count 6 hexDigitChar >> pass

-- | ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
ecl :: Parser ()
ecl = string "ecl:" >> string "amb" <|> string "blu" <|> string "brn" <|> string "gry" <|> string "grn" <|> string "hzl" <|> string "oth" >> pass

-- | pid (Passport ID) - a nine-digit number, including leading zeroes.
pid :: Parser ()
pid = string "pid:" >> skipCount 9 digitChar

-- | cid (Country ID) - ignored, missing or not.
cid :: Parser ()
cid = string "cid:" >> Text.Megaparsec.some alphaNumChar >> pass

run :: IO ()
run = do
  inputRaw <- splitOn "\n\n" <$> readFile "inputs/day4-2.txt"
  case evaluate2 inputRaw of
    Nothing -> print ("No result found" :: String)
    Just i -> print i
