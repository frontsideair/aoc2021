{-# LANGUAGE TupleSections #-}

import Control.Monad.State (State, evalState, foldM, get, modify, runState)
import Data.List (foldl', foldl1', sortOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Text.Parsec (anyChar, char, count, endBy1, endOfLine, eof, letter, many1, sepBy1, string, try)
import Text.Parsec.Text (Parser, parseFromFile)

type Pair = (Char, Char)

type Rules = Map (Pair, Int) [Pair]

main :: IO ()
main = do
  (template, rules) <- parseFromFile parser "input.txt" >>= either (fail . show) return
  print $ part1 template rules
  print $ part2 template rules

expand :: [Pair] -> Int -> State Rules [Pair]
expand template n =
  foldM
    ( \acc pair -> do
        rules <- get
        case Map.lookup (pair, n) rules of
          Nothing -> do
            v <- expand (rules ! (pair, 1)) (n - 1)
            modify $ Map.insert (pair, n) v
            return (acc ++ v)
          Just result -> return (acc ++ result)
    )
    []
    template

part1 :: [Pair] -> Map (Pair, Int) [Pair] -> Int
part1 = go 10

part2 :: [Pair] -> Map (Pair, Int) [Pair] -> Int
part2 = go 25 -- 40

go :: Int -> [Pair] -> Map (Pair, Int) [Pair] -> Int
go n template rules = maximum sorted - minimum sorted
  where
    result = unpairs $ evalState (expand template n) rules
    sorted = snd <$> Map.toList (groupByCount result)

groupByCount :: Ord a => [a] -> Map a Int
groupByCount = Map.fromListWith (+) . map (,1)

pairs :: [a] -> [(a, a)]
pairs template = zip template $ tail template

unpairs :: [(a, a)] -> [a]
unpairs xs = fst (head xs) : (snd <$> xs)

parser :: Parser ([Pair], Rules)
parser = do
  template <- many1 letter
  endOfLine
  endOfLine
  rules <- ruleParser `endBy1` endOfLine
  eof
  return (pairs template, Map.fromList rules)

ruleParser :: Parser ((Pair, Int), [Pair])
ruleParser = do
  first <- letter
  second <- letter
  string " -> "
  right <- letter
  return (((first, second), 1), [(first, right), (right, second)])