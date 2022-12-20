module Day07 (run) where

import Data.Foldable (Foldable (minimum))
import qualified Data.Map as M
import Relude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Prelude

parseInput1 :: Parsec Void Text [Input]
parseInput1 = many parseInput

parseInput :: Parsec Void Text Input
parseInput =
  CdRoot <$ string "$ cd /" <* splitter
    <|> CdPop <$ string "$ cd .." <* splitter
    <|> Cd <$> (string "$ cd " *> many alphaNumChar <* splitter)
    <|> Ls <$> (string "$ ls" >> newline *> many (dirContentParser <* splitter))
  where
    splitter = optional newline

dirContentParser :: Parsec Void Text DirContent
dirContentParser =
  Directory <$> (string "dir " *> many alphaNumChar)
    <|> curry File <$> decimal <* char ' ' <*> many (alphaNumChar <|> char '.')

type FileName = String

type FileSize = Integer

type DirectoryName = String

data DirContent = File (FileSize, FileName) | Directory DirectoryName deriving stock (Show)

data Input = Cd DirectoryName | CdRoot | CdPop | Ls [DirContent] deriving stock (Show)

evaluate1 :: [Input] -> Sum FileSize
evaluate1 = foldMap snd . filter (\x0 -> snd x0 <= Sum 100000) . process

evaluate2 :: FileSize -> [Input] -> Sum FileSize
evaluate2 occupied = minimum . fmap snd . filter (\x0 -> snd x0 >= Sum toBeFreed) . process
  where
    maxSpace = 70000000
    requiredSpace = 30000000
    toBeFreed = requiredSpace - (maxSpace - occupied)

process :: [Input] -> [([String], Sum FileSize)]
process inputs = sumUpFileSizes <$> fmap (\(d, cs) -> (d, ordNub $ join $ cs : mapMaybe (\(dir', cs') -> if d `isPrefixOf` dir' then Just cs' else Nothing) directories)) directories
  where
    sumUpFileSizes = fmap (foldMap (Sum . snd))
    directories = M.toAscList $ M.mapKeys reverse contents
    (_, contents) = foldl' f ([], M.empty) inputs

    f :: ([String], Map [String] [(FileName, FileSize)]) -> Input -> ([String], Map [String] [(FileName, FileSize)])
    f acc (Cd s) = first (s :) acc
    f acc CdRoot = first (const []) acc
    f acc CdPop = first Prelude.tail acc
    f (cursor, files) (Ls dcs) = (cursor, M.insert cursor (mapMaybe g dcs) files)

    g :: DirContent -> Maybe (FileName, FileSize)
    g (File x0) = Just $ swap x0
    g (Directory _) = Nothing

run :: IO ()
run = do
  exampleInputRaw <- readFileText "inputs/07-example.txt"
  let exampleParsed = parseMaybe parseInput1 exampleInputRaw
  print $ traverse evaluate1 exampleParsed

  inputRaw <- readFileText "inputs/07.txt"
  let parsed = parseMaybe parseInput1 inputRaw

  print $ traverse evaluate1 parsed

  print $ traverse (evaluate2 48381165) exampleParsed

  print $ traverse (evaluate2 43441553) parsed
