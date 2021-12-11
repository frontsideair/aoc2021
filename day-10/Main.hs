{-# LANGUAGE LambdaCase #-}

import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (sort)
import Data.Map (elemAt)
import System.Posix.Internals (puts)
import Text.Parsec (between, char, eof, many, many1, parse, sepEndBy1, string, (<|>))
import Text.Parsec.Error (Message (Expect, Message, SysUnExpect, UnExpect), errorMessages, messageString)
import Text.Parsec.Text (parseFromFile)

score :: Char -> Int
score c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> 0

f (Left err, line) = g err line
f (Right _, _) = undefined

g err line =
  let messages = errorMessages err
   in case (head messages, last messages) of
        (SysUnExpect ['"', c, '"'], _) -> Left c
        (SysUnExpect _, Expect ['"', c, '"']) -> Right (c, line)
        _ -> undefined

step :: String -> (Char, String) -> String
step acc (c, line) = case parse parser "" (line ++ [c]) of
  Left err -> let Right (c', line') = g err (line ++ [c]) in step (c : acc) (c', line')
  Right _ -> c : acc

score2 :: Char -> Int
score2 c = case c of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _ -> 0

multiplier :: Int
multiplier = 5

calculateScore2 :: String -> Int
calculateScore2 = foldr (\c acc -> (acc * multiplier) + score2 c) 0

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

main :: IO ()
main = do
  file <- lines <$> readFile "input.txt"
  let parseResults = parse parser "" <$> file
  let errors = f <$> zip parseResults file
  let (corrupted, incomplete) = partitionEithers errors
  print $ part1 corrupted
  print $ part2 incomplete

part1 corrupted = sum $ score <$> corrupted

part2 incomplete =
  let scores = calculateScore2 . step [] <$> incomplete
   in median scores

parser = many1 chunk `sepEndBy1` eof

chunk = type1 <|> type2 <|> type3 <|> type4

type1 = between (char '(') (char ')') (many chunk) $> ()

type2 = between (char '[') (char ']') (many chunk) $> ()

type3 = between (char '{') (char '}') (many chunk) $> ()

type4 = between (char '<') (char '>') (many chunk) $> ()