import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Text.Parsec (endBy1, endOfLine, eof, letter, many1, string)
import Text.Parsec.Text (Parser, parseFromFile)

type Pair = (Char, Char)

type Rules = Map Pair [Pair]

main :: IO ()
main = do
  (template, rules) <- parseFromFile parser "input.txt" >>= either (fail . show) return
  print $ part1 template rules
  print $ part2 template rules

part1 :: Map Pair Double -> Rules -> Integer
part1 = go 10

part2 :: Map Pair Double -> Rules -> Integer
part2 = go 40

go :: Int -> Map Pair Double -> Rules -> Integer
go n template rules = maximum freqs - minimum freqs
  where
    result = iterate (expand rules) template !! n
    freqs = countChars result

expand :: Rules -> Map Pair Double -> Map Pair Double
expand rules map = foldl' (\acc (pair, m) -> foldl' (\acc (pair, n) -> Map.insertWith (+) pair (n * m) acc) acc ((pair, -1) : zip (rules ! pair) (repeat 1))) map (Map.toList map)

countChars :: Map Pair Double -> [Integer]
countChars map = ceiling . (/ 2) . snd <$> Map.toList (foldl' (\acc ((l, r), n) -> Map.insertWith (+) l n $ Map.insertWith (+) r n acc) Map.empty (Map.toList map))

pairs :: [a] -> [(a, a)]
pairs template = zip template $ tail template

unpairs :: [(a, a)] -> [a]
unpairs xs = fst (head xs) : (snd <$> xs)

parser :: Parser (Map Pair Double, Rules)
parser = do
  template <- many1 letter
  endOfLine
  endOfLine
  rules <- ruleParser `endBy1` endOfLine
  eof
  return (Map.fromListWith (+) $ zip (pairs template) (repeat 1), Map.fromList rules)

ruleParser :: Parser (Pair, [Pair])
ruleParser = do
  first <- letter
  second <- letter
  string " -> "
  right <- letter
  return ((first, second), [(first, right), (right, second)])